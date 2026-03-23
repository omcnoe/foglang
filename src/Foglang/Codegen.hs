module Foglang.Codegen (codegenGoFile) where

import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), MatchArm (..), PackageClause (..), Param (..), Pattern (..), StringLit (..), TypeExpr (..), pattern UnitType, exprType)
import Foglang.Inference (inferAndResolve)
import System.Process (readProcess)

ind :: Int -> T.Text
ind n = T.replicate n "\t"

-- Suppress Go's "declared and not used" error for a local variable.
useVar :: Int -> T.Text -> T.Text
useVar indent name = ind indent <> "_ = " <> name <> "\n"

-- Inline variant for use inside IIFEs (semicolon-separated).
useVarInline :: T.Text -> T.Text
useVarInline name = "_ = " <> name <> "; "

-- Single-line counterpart to genLocalFunc: wraps a pre-rendered expression
-- as an inline Go func literal. Used for partial application wrappers and
-- simple (non-sequence) lambda bodies.
genInlineFunc :: T.Text -> TypeExpr -> T.Text -> T.Text
genInlineFunc params (UnitType) body = "func(" <> params <> ") { " <> body <> " }"
genInlineFunc params retTy body = "func(" <> params <> ") " <> typeExprGoText retTy <> " { return " <> body <> " }"

-- Render a let-bound local function as a multi-line func literal,
-- using the appropriate statement/body generator for the function body.
genLocalFunc :: Int -> T.Text -> TypeExpr -> Expr -> T.Text
genLocalFunc indent params retTy trhs =
  "func("
    <> params
    <> ")"
    <> retTypeGoText retTy
    <> " {\n"
    <> bodyText (indent + 1) trhs
    <> ind indent
    <> "}"
  where
    bodyText = case retTy of
      UnitType -> genStmtBody
      _ -> genRetBody

identText :: Ident -> T.Text
identText (Ident t) = t

intLitText :: IntLit -> T.Text
intLitText (IntDecimal t) = t
intLitText (IntBinary t) = t
intLitText (IntOctal t) = t
intLitText (IntHex t) = t

floatLitText :: FloatLit -> T.Text
floatLitText (FloatDecimal t) = t
floatLitText (FloatHex t) = t

typeExprGoText :: TypeExpr -> T.Text
typeExprGoText (UnitType) = "struct{}"
typeExprGoText (TNamed (Ident t)) = t
typeExprGoText (TSlice t) = "[]" <> typeExprGoText t
typeExprGoText (TMap k v) = "map[" <> typeExprGoText k <> "]" <> typeExprGoText v
typeExprGoText (TFunc fixedTys mVarTy retTy) =
  "func("
    <> T.intercalate ", " (map paramTypeGoText fixedTys ++ maybe [] (\vTy -> ["..." <> typeExprGoText vTy]) mVarTy)
    <> ")"
    <> retTypeGoText retTy
typeExprGoText (TVar _) = error "TVar survived to codegen"
typeExprGoText (TConstrained _ _) = error "TConstrained survived to codegen"

-- Alias for readability in parameter positions.
paramTypeGoText :: TypeExpr -> T.Text
paramTypeGoText = typeExprGoText

-- Return type annotation for Go: empty for unit, " T" otherwise.
retTypeGoText :: TypeExpr -> T.Text
retTypeGoText (UnitType) = ""
retTypeGoText ty = " " <> typeExprGoText ty

-- Render a parameter list as Go source.
-- A sole anonymous PUnit is the zero-param rewrite: produces "" (no params).
-- In any other param list, each PUnit becomes "_pN struct{}".
paramListGoText :: [Param] -> T.Text
paramListGoText [PUnit] = ""
paramListGoText params = T.intercalate ", " $ zipWith paramText [0 :: Int ..] params
  where
    paramText _ (PVariadic name ty) = identText name <> " ..." <> typeExprGoText ty
    paramText _ (PTyped name ty) = identText name <> " " <> paramTypeGoText ty
    paramText i PUnit = "_p" <> T.pack (show i) <> " struct{}"

-- Build synthetic Params with fresh _pN names from bare types.
-- Used for partial application and coercion wrappers where we need to generate
-- a Go closure with named parameters from just the type signature.
syntheticParams :: [TypeExpr] -> Maybe TypeExpr -> [Param]
syntheticParams fixedTys mVarTy =
  [PTyped (Ident ("_p" <> T.pack (show i))) ty | (i, ty) <- zip [0 :: Int ..] fixedTys]
    ++ maybe [] (\vTy -> [PVariadic (Ident "_args") vTy]) mVarTy

-- Build the parameter declaration and call argument texts for a closure wrapper.
-- Returns (closureParamDecl, callArgNames) where callArgNames are just the
-- parameter names (with ... for variadics), ready to be joined with any
-- pre-supplied arguments.
closureParamsAndCallArgs :: [TypeExpr] -> Maybe TypeExpr -> (T.Text, [T.Text])
closureParamsAndCallArgs fixedTys mVarTy = (paramListGoText params, callArgNames params)
  where
    params = syntheticParams fixedTys mVarTy
    callArgNames = map argName
    argName (PTyped name _) = identText name
    argName (PVariadic name _) = identText name <> "..."
    argName PUnit = error "closureParamsAndCallArgs: unexpected PUnit"

genElsePart :: BodyMode -> Int -> Expr -> T.Text
genElsePart mode indent (EIf _ _ cond then' else') =
  " else if "
    <> genExpr cond
    <> " {\n"
    <> genBody mode (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart mode indent else'
genElsePart mode indent e =
  " else {\n"
    <> genBody mode (indent + 1) e
    <> ind indent
    <> "}"

genIfChain :: BodyMode -> Int -> Expr -> Expr -> Expr -> T.Text
genIfChain mode indent cond then' else' =
  ind indent
    <> "if "
    <> genExpr cond
    <> " {\n"
    <> genBody mode (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart mode indent else'
    <> "\n"

data BodyMode = ReturnLast | StmtOnly

-- Generate a match expression as a statement-level if/else chain.
genMatchBody :: BodyMode -> Int -> Expr -> [MatchArm] -> T.Text
genMatchBody _ _ _ [] = ""
genMatchBody mode indent tscrut arms =
  -- Detect tuple match by scanning ALL arms for any tuple pattern.
  -- Go multi-return (e.g. map comma-ok) must be immediately destructured,
  -- so we also alias scrutVar := scrutVar_0 for non-tuple arms to reference
  -- (only when there are non-tuple arms, to avoid unused variable errors in Go).
  -- Scan arms once: detect tuple arity and whether non-tuple arms exist.
  let (tupleArity, hasNonTupleArm) =
        foldr
          ( \(MatchArm _ p _) (arity, hasNon) -> case p of
              PtTuple pats -> (max arity (length pats), hasNon)
              _ -> (arity, True)
          )
          (0, False)
          arms
      isTupleMatch = tupleArity > 0
      scrutVar = "_scrut" <> T.pack (show indent)
      tupleVarNames = [scrutVar <> "_" <> T.pack (show i) | i <- [0 .. tupleArity - 1]]
      scrutDecl
        | isTupleMatch =
            let vars = T.intercalate ", " tupleVarNames
                useVars = T.concat [useVar indent v | v <- tupleVarNames]
                alias =
                  if hasNonTupleArm
                    then ind indent <> scrutVar <> " := " <> scrutVar <> "_0\n" <> useVar indent scrutVar
                    else ""
             in ind indent <> vars <> " := " <> genExpr tscrut <> "\n" <> useVars <> alias
        | otherwise = ind indent <> scrutVar <> " := " <> genExpr tscrut <> "\n" <> useVar indent scrutVar
      genArm isFirst isLast (MatchArm _ pat tbody) =
        let (cond, bindings) = genPatternCond scrutVar pat
            keyword'
              | isFirst = "if " <> cond <> " "
              | isLast && isIrrefutablePattern pat = "} else "
              | otherwise = "} else if " <> cond <> " "
            bindingText = T.concat [ind (indent + 1) <> n <> " := " <> v <> "\n" <> useVar (indent + 1) n | (n, v) <- bindings]
         in ind indent <> keyword' <> "{\n" <> bindingText <> genBody mode (indent + 1) tbody
      lastFlags = replicate (length arms - 1) False ++ [True]
      armTexts = zipWith3 genArm (True : repeat False) lastFlags arms
      lastArmIsCatchAll = case last arms of
        MatchArm _ pat _ -> isIrrefutablePattern pat
      defaultReturn = case mode of
        ReturnLast | not lastArmIsCatchAll -> ind indent <> "panic(\"match not exhaustive\")\n"
        _ -> ""
   in scrutDecl <> T.concat armTexts <> ind indent <> "}\n" <> defaultReturn

-- Generate a match expression as an inline IIFE.
genMatchExpr :: TypeExpr -> Expr -> [MatchArm] -> T.Text
genMatchExpr ty tscrut tarms =
  "func() "
    <> typeExprGoText ty
    <> " {\n"
    <> genMatchBody ReturnLast 1 tscrut tarms
    <> "}()"

-- Combine Go conditions with &&, filtering out trivial "true" entries.
combineConds :: [T.Text] -> T.Text
combineConds conds =
  case filter (/= "true") conds of
    [] -> "true"
    cs -> T.intercalate " && " cs

-- Whether a pattern always matches (no condition check needed).
isIrrefutablePattern :: Pattern -> Bool
isIrrefutablePattern PtWildcard = True
isIrrefutablePattern (PtVar _) = True
isIrrefutablePattern (PtTuple pats) = all isIrrefutablePattern pats
isIrrefutablePattern _ = False

-- Generate the condition check and variable bindings for a pattern.
-- Returns (condition text, [(varName, valueExpr)])
genPatternCond :: T.Text -> Pattern -> (T.Text, [(T.Text, T.Text)])
genPatternCond _ PtWildcard = ("true", [])
genPatternCond scrut (PtVar (Ident name)) = ("true", [(name, scrut)])
genPatternCond scrut (PtIntLit lit) = (scrut <> " == " <> intLitText lit, [])
genPatternCond scrut (PtBoolLit True) = (scrut, [])
genPatternCond scrut (PtBoolLit False) = ("!(" <> scrut <> ")", [])
genPatternCond scrut PtSliceEmpty = ("len(" <> scrut <> ") == 0", [])
genPatternCond scrut (PtCons hdPat tlPat) =
  let hdExpr = scrut <> "[0]"
      tlExpr = scrut <> "[1:]"
      (hdCond, hdBindings) = genPatternCond hdExpr hdPat
      (tlCond, tlBindings) = genPatternCond tlExpr tlPat
   in (combineConds ["len(" <> scrut <> ") > 0", hdCond, tlCond], hdBindings ++ tlBindings)
genPatternCond scrut (PtTuple pats) =
  let results = [genPatternCond (scrut <> "_" <> T.pack (show i)) pat | (i, pat) <- zip [0 :: Int ..] pats]
   in (combineConds (map fst results), concatMap snd results)

-- Generate the continuation of a let binding (the in-expression).
-- Nothing means the let is the last thing in the block.
genCont :: BodyMode -> Int -> Maybe Expr -> T.Text
genCont _ _ Nothing = ""
genCont mode indent (Just tin) = genBody mode indent tin

-- Unified body generator. ReturnLast emits 'return expr' for the final expression;
-- StmtOnly emits it as a plain statement.
genBody :: BodyMode -> Int -> Expr -> T.Text
genBody StmtOnly _ (EUnitLit _) = ""
genBody mode indent (EIf _ _ cond then' else') = genIfChain mode indent cond then' else'
genBody mode indent (EMatch _ _ tscrut tarms) = genMatchBody mode indent tscrut tarms
genBody StmtOnly indent (ESequence _ _ texprs) = T.concat (map (genBody StmtOnly indent) texprs)
genBody ReturnLast indent (ESequence _ _ texprs)
  | null texprs = error "genBody ReturnLast: empty ESequence (should be unreachable)"
  | otherwise =
      T.concat (map (genBody StmtOnly indent) (init texprs))
        <> genBody ReturnLast indent (last texprs)
genBody mode indent (ELet _ _ name (Binding [] retTy trhs) mtin)
  | retTy == UnitType || exprType trhs == UnitType,
    name == Ident "_" =
      ind indent
        <> genExpr trhs
        <> "\n"
        <> genCont mode indent mtin
  | retTy == UnitType || exprType trhs == UnitType =
      ind indent
        <> genExpr trhs
        <> "\n"
        <> ind indent
        <> identText name
        <> " := struct{}{}\n"
        <> useVar indent (identText name)
        <> genCont mode indent mtin
  | otherwise =
      ind indent
        <> identText name
        <> " := "
        <> genExpr trhs
        <> "\n"
        <> useVar indent (identText name)
        <> genCont mode indent mtin
genBody mode indent (ELet _ _ name (Binding params retTy trhs) mtin) =
  -- Use var declaration + assignment for local functions to support recursion.
  -- Go closures can't reference themselves with :=, but can with var + =.
  let paramsText = paramListGoText params
      funcTy = "func(" <> paramsText <> ")" <> retTypeGoText retTy
   in ind indent
        <> "var "
        <> identText name
        <> " "
        <> funcTy
        <> "\n"
        <> ind indent
        <> identText name
        <> " = "
        <> genLocalFunc indent paramsText retTy trhs
        <> "\n"
        <> useVar indent (identText name)
        <> genCont mode indent mtin
genBody StmtOnly indent te@(EApplication {}) = ind indent <> genExpr te <> "\n"
genBody StmtOnly indent te = ind indent <> "_ = " <> genExpr te <> "\n"
genBody ReturnLast indent te = ind indent <> "return " <> genExpr te <> "\n"

genRetBody :: Int -> Expr -> T.Text
genRetBody = genBody ReturnLast

genStmtBody :: Int -> Expr -> T.Text
genStmtBody = genBody StmtOnly

genFunc :: Ident -> [Param] -> TypeExpr -> Expr -> T.Text
genFunc name params retTy tbody =
  "func "
    <> identText name
    <> "("
    <> paramListGoText params
    <> ")"
    <> retTypeGoText retTy
    <> " {\n"
    <> bodyText 1 tbody
    <> "}\n"
  where
    bodyText = case retTy of
      UnitType -> genStmtBody
      _ -> genRetBody

-- Generate a Go expression from an Expr.
-- For Application nodes, the function's Expr carries its type, enabling
-- arity-aware partial application codegen.
-- Map foglang operators to Go operators (triple-char bitwise/shift -> Go equivalents)
goInfixOp :: T.Text -> T.Text
goInfixOp "|||" = "|"
goInfixOp "&&&" = "&"
goInfixOp "^^^" = "^"
goInfixOp "<<<" = "<<"
goInfixOp ">>>" = ">>"
goInfixOp op = op

genExpr :: Expr -> T.Text
genExpr (EVar _ _ i) = identText i
genExpr (EIntLit _ _ lit) = intLitText lit
genExpr (EFloatLit _ _ lit) = floatLitText lit
genExpr (EStrLit _ _ (StringLit t)) = "\"" <> t <> "\""
genExpr (EUnitLit _) = "struct{}{}"
genExpr (EVariadicSpread _ _ te) = genExpr te <> "..."
genExpr (EIndex _ _ te tidx) = genExpr te <> "[" <> genExpr tidx <> "]"
genExpr (EInfixOp _ _ e1 "::" e2) = "append(" <> typeExprGoText (exprType e2) <> "{" <> genExpr e1 <> "}, " <> genExpr e2 <> "...)"
genExpr (EInfixOp _ _ e1 op e2) = "(" <> genExpr e1 <> " " <> goInfixOp op <> " " <> genExpr e2 <> ")"
genExpr (EApplication _ _ tf targs) =
  case exprType tf of
    TFunc fixedTys mVarTy retTy ->
      let nFixed = length fixedTys
          nSupplied = length targs
          -- Partial when not all fixed args supplied, or when all fixed params
          -- supplied but the variadic arg slot is still empty.
          isPartial = nSupplied < nFixed || (nSupplied == nFixed && mVarTy /= Nothing)
       in if isPartial
            then
              let (closureParams, argNames) = closureParamsAndCallArgs (drop nSupplied fixedTys) mVarTy
                  callArgs = T.intercalate ", " (map genExpr targs ++ argNames)
               in genInlineFunc closureParams retTy (genExpr tf <> "(" <> callArgs <> ")")
            else
              let args = case mVarTy of
                    Nothing ->
                      -- When nFixed == 0 (PUnit), strip the single () sentinel fixed arg.
                      if nFixed == 0 then [] else map genExpr targs
                    Just _ ->
                      -- Strip a single () sentinel (indicates empty variadic arg provided).
                      -- EVariadicSpread nodes emit expr... via genExpr.
                      let varArgs = case drop nFixed targs of
                            [EUnitLit _] -> []
                            rest -> rest
                       in map genExpr (take nFixed targs) ++ map genExpr varArgs
               in genExpr tf <> "(" <> T.intercalate ", " args <> ")"
    _ -> genExpr tf <> "(" <> T.intercalate ", " (map genExpr targs) <> ")"
genExpr (EIf _ ty cond then' else')
  | ty == UnitType =
      "func() { if " <> genExpr cond <> " { " <> genExpr then' <> " }; " <> genExpr else' <> " }()"
  | otherwise =
      "func() " <> typeExprGoText ty <> " { if " <> genExpr cond <> " { return " <> genExpr then' <> " }; return " <> genExpr else' <> " }()"
genExpr (ESequence _ _ []) = "struct{}{}"
genExpr (ESequence _ ty texprs)
  | ty == UnitType =
      "func() { "
        <> T.intercalate "; " (map genExpr texprs)
        <> " }()"
  | otherwise =
      "func() "
        <> typeExprGoText ty
        <> " { "
        <> T.intercalate "; " (map genExpr (init texprs))
        <> "; return "
        <> genExpr (last texprs)
        <> " }()"
genExpr (ELet _ ty name (Binding [] _ trhs) (Just tin))
  | exprType trhs == UnitType,
    name == Ident "_" =
      "func() "
        <> retTyText
        <> "{ "
        <> genExpr trhs
        <> "; "
        <> returnKw
        <> genExpr tin
        <> " }()"
  | exprType trhs == UnitType =
      "func() "
        <> retTyText
        <> "{ "
        <> genExpr trhs
        <> "; "
        <> identText name
        <> " := struct{}{}; "
        <> useVarInline (identText name)
        <> returnKw
        <> genExpr tin
        <> " }()"
  | otherwise =
      "func() "
        <> retTyText
        <> "{ "
        <> identText name
        <> " := "
        <> genExpr trhs
        <> "; "
        <> useVarInline (identText name)
        <> returnKw
        <> genExpr tin
        <> " }()"
  where
    retTyText = case ty of
      UnitType -> ""
      t -> typeExprGoText t <> " "
    returnKw = case ty of
      UnitType -> ""
      _ -> "return "
genExpr (ELet _ ty name (Binding params retTy trhs) (Just tin)) =
  -- Use var declaration + assignment for local functions to support recursion,
  -- matching the statement-level codegen in genBody.
  let paramsText = paramListGoText params
      funcTy = "func(" <> paramsText <> ")" <> retTypeGoText retTy
   in "func() "
        <> outerRetTyText
        <> "{ var "
        <> identText name
        <> " "
        <> funcTy
        <> "; "
        <> identText name
        <> " = "
        <> genLocalFunc 0 paramsText retTy trhs
        <> "; "
        <> useVarInline (identText name)
        <> outerReturnKw
        <> genExpr tin
        <> " }()"
  where
    outerRetTyText = case ty of
      UnitType -> ""
      t -> typeExprGoText t <> " "
    outerReturnKw = case ty of
      UnitType -> ""
      _ -> "return "
genExpr (ELet _ _ name (Binding [] _ trhs) Nothing)
  | name == Ident "_" =
      "func() { " <> genExpr trhs <> " }()"
  | otherwise =
      "func() { "
        <> identText name
        <> " := "
        <> genExpr trhs
        <> "; "
        <> useVarInline (identText name)
        <> "}()"
genExpr (ELet _ _ name (Binding params retTy trhs) Nothing) =
  let paramsText = paramListGoText params
      funcTy = "func(" <> paramsText <> ")" <> retTypeGoText retTy
   in "func() { var "
        <> identText name
        <> " "
        <> funcTy
        <> "; "
        <> identText name
        <> " = "
        <> genLocalFunc 0 paramsText retTy trhs
        <> "; "
        <> useVarInline (identText name)
        <> "}()"
genExpr (ESliceLit _ (TSlice (TNamed (Ident "opaque"))) []) = "nil"
genExpr (ESliceLit _ ty []) = typeExprGoText ty <> "{}"
genExpr (ESliceLit _ ty texprs) = typeExprGoText ty <> "{" <> T.intercalate ", " (map genExpr texprs) <> "}"
genExpr (EMapLit _ (TMap (TNamed (Ident "opaque")) (TNamed (Ident "opaque")))) = "nil"
genExpr (EMapLit _ ty) = typeExprGoText ty <> "{}"
genExpr (EMatch _ ty tscrut tarms) = genMatchExpr ty tscrut tarms
genExpr (ELambda _ _ (Binding params retTy tbody@(ESequence {}))) =
  genLocalFunc 0 (paramListGoText params) retTy tbody
genExpr (ELambda _ _ (Binding params retTy tbody)) =
  genInlineFunc (paramListGoText params) retTy (genExpr tbody)

codegenImport :: ImportDecl -> T.Text
codegenImport (ImportDecl Default path) = "import \"" <> path <> "\"\n"
codegenImport (ImportDecl Dot path) = "import . \"" <> path <> "\"\n"
codegenImport (ImportDecl Blank path) = "import _ \"" <> path <> "\"\n"
codegenImport (ImportDecl (Alias i) path) = "import " <> identText i <> " \"" <> path <> "\"\n"

codegenHeader :: Header -> T.Text
codegenHeader (Header (PackageClause pkg) []) = "package " <> identText pkg <> "\n"
codegenHeader (Header (PackageClause pkg) imports) =
  "package "
    <> identText pkg
    <> "\n"
    <> T.concat (map codegenImport imports)

codegenDecl :: Ident -> [Param] -> TypeExpr -> Expr -> T.Text
codegenDecl name [] valTy tbody@(ESequence {}) =
  "var "
    <> identText name
    <> " "
    <> typeExprGoText valTy
    <> " = func() "
    <> typeExprGoText valTy
    <> " {\n"
    <> genRetBody 1 tbody
    <> "}()\n"
codegenDecl name [] valTy tbody = "var " <> identText name <> " " <> typeExprGoText valTy <> " = " <> coerceValue valTy tbody <> "\n"
codegenDecl name params retTy tbody = genFunc name params retTy tbody

-- Wrap a function value when its declared return type differs from its actual
-- return type in the unit<->struct{} dimension (void unit coercion).
-- Handles any TFunc with matching param lists.
coerceValue :: TypeExpr -> Expr -> T.Text
coerceValue declTy tbody =
  case (declTy, exprType tbody) of
    ( TFunc fixedTys mVarTy (TNamed (Ident "struct{}")),
      TFunc fixedTys' mVarTy' (UnitType)
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") struct{} { " <> genExpr tbody <> "(" <> callArgs <> "); return struct{}{} }"
    ( TFunc fixedTys mVarTy (UnitType),
      TFunc fixedTys' mVarTy' (TNamed (Ident "struct{}"))
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") { " <> genExpr tbody <> "(" <> callArgs <> ") }"
    _ -> genExpr tbody

codegenTopLevel :: Expr -> T.Text
codegenTopLevel (ELet _ _ n (Binding p t trhs) mtin) = codegenDecl n p t trhs <> maybe "" codegenTopLevel mtin
codegenTopLevel (ESequence _ _ texprs) = T.concat (map codegenTopLevel texprs)
codegenTopLevel e@(EApplication {}) = "func init() {\n" <> genStmtBody 1 e <> "}\n"
codegenTopLevel e@(EIf {}) = "func init() {\n" <> genStmtBody 1 e <> "}\n"
codegenTopLevel e@(EMatch {}) = "func init() {\n" <> genStmtBody 1 e <> "}\n"
-- EInfixOp may contain callable sub-expressions (e.g. f() + g()), so we must
-- wrap in init() even though the infix op itself is pure. If new expression
-- types are added that can contain callable sub-expressions, they will also
-- need init() wrapping here.
codegenTopLevel e@(EInfixOp {}) = "func init() {\n" <> genStmtBody 1 e <> "}\n"
-- TODO CLAUDE: replace errors with warnings when warnings support is added.
-- These are pure expressions with no side effects at the top level.
codegenTopLevel e@(EVar {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EIntLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EFloatLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EStrLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EUnitLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(ELambda {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EIndex {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(ESliceLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EMapLit {}) = error $ "unsupported top-level expression: " <> show e
codegenTopLevel e@(EVariadicSpread {}) = error $ "unsupported top-level expression: " <> show e

codegenGoFile :: FogFile -> IO T.Text
codegenGoFile (FogFile hdr body) = do
  let tbody = case inferAndResolve body of
        Right te -> te
        Left errs -> error $ "inference errors: " <> show errs
      raw = codegenHeader hdr <> codegenTopLevel tbody
  formatted <- readProcess "gofmt" [] (T.unpack raw)
  return $ T.pack formatted
