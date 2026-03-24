module Foglang.Codegen (genGoFile) where

import Data.Text qualified as T
import Foglang.AST (Binding (..), Coercion (..), Expr (..), ExprAnn (..), FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), MatchArm (..), PackageClause (..), Param (..), Pattern (..), StringLit (..), TypeExpr (..), pattern UnitType, exprAnn, exprType)
import Foglang.Inference (inferAndResolve)
import System.Process (readProcess)

ind :: Int -> T.Text
ind n = T.replicate n "\t"

-- Suppress Go's "declared and not used" error for a local variable.
useVar :: Int -> T.Text -> T.Text
useVar indent name = ind indent <> "_ = " <> name <> "\n"

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
      UnitType -> genBody Void
      _ -> genBody Returning

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
retTypeGoText t = " " <> typeExprGoText t

-- Render a parameter list as Go source.
-- A sole anonymous PUnit is the zero-param rewrite: produces "" (no params).
-- In any other param list, each PUnit becomes "_pN struct{}".
paramListGoText :: [Param] -> T.Text
paramListGoText [PUnit] = ""
paramListGoText params = T.intercalate ", " $ zipWith paramText [0 :: Int ..] params
  where
    paramText _ (PVariadic name t) = identText name <> " ..." <> typeExprGoText t
    paramText _ (PTyped name t) = identText name <> " " <> paramTypeGoText t
    paramText i PUnit = "_p" <> T.pack (show i) <> " struct{}"

-- Build synthetic Params with fresh _pN names from bare types.
-- Used for partial application and coercion wrappers where we need to generate
-- a Go closure with named parameters from just the type signature.
syntheticParams :: [TypeExpr] -> Maybe TypeExpr -> [Param]
syntheticParams fixedTys mVarTy =
  [PTyped (Ident ("_p" <> T.pack (show i))) t | (i, t) <- zip [0 :: Int ..] fixedTys]
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
genElsePart mode indent (EIf _ cond then' else') =
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

data BodyMode = Returning | Void

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
        Returning | not lastArmIsCatchAll -> ind indent <> "panic(\"match not exhaustive\")\n"
        _ -> ""
   in scrutDecl <> T.concat armTexts <> ind indent <> "}\n" <> defaultReturn

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

-- Generate a function body. Returning: last expression gets 'return'.
-- Void: all expressions emitted as statements.
genBody :: BodyMode -> Int -> Expr -> T.Text
genBody Void _ (EUnitLit _) = ""
genBody mode indent (EIf _ cond then' else') = genIfChain mode indent cond then' else'
genBody mode indent (EMatch _ tscrut tarms) = genMatchBody mode indent tscrut tarms
genBody Void indent (ESequence _ texprs) = T.concat (map (genBody Void indent) texprs)
genBody Returning indent (ESequence _ texprs)
  | null texprs = error "genBody Returning: empty ESequence (should be unreachable)"
  | otherwise =
      T.concat (map (genBody Void indent) (init texprs))
        <> genBody Returning indent (last texprs)
-- Void binding: RHS is void (can't bind in Go), or declared type is unit
genBody mode indent (ELet _ (Ident "_") (Binding [] retTy trhs) mtin)
  | retTy == UnitType || exprType trhs == UnitType =
      -- bound to "_", ignore for cleaner output
      genBody Void indent trhs
        <> genCont mode indent mtin
genBody mode indent (ELet _ name (Binding [] retTy trhs) mtin)
  | retTy == UnitType || exprType trhs == UnitType =
      -- bound to name, synthesize struct{} value
      genBody Void indent trhs
        <> ind indent <> identText name <> " := struct{}{}\n"
        <> useVar indent (identText name)
        <> genCont mode indent mtin
-- Normal value binding.
genBody mode indent (ELet _ name (Binding [] _ trhs) mtin) =
  ind indent <> identText name <> " := " <> genExpr trhs <> "\n"
    <> useVar indent (identText name)
    <> genCont mode indent mtin
genBody mode indent (ELet _ name (Binding params retTy trhs) mtin) =
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
genBody Void indent te
  | isStmt (exprAnn te) = ind indent <> genExpr te <> "\n"
  | otherwise = ind indent <> "_ = " <> genExpr te <> "\n"
genBody Returning indent te = ind indent <> "return " <> genExpr te <> "\n"

-- Wrap a compound expression in an immediately-invoked function expression.
-- Three cases:
--   void + side effects → func() struct{} { <Void body>; return struct{}{} }()
--   void + pure         → struct{}{}
--   non-void            → func() T { <Returning body> }()
genExprIIFE :: Expr -> T.Text
genExprIIFE e = case exprAnn e of
  ExprAnn{ty = UnitType, isStmt = True} ->
    "func() struct{} {\n" <> genBody Void 1 e <> "\treturn struct{}{}\n}()"
  ExprAnn{ty = UnitType} -> "struct{}{}"
  ExprAnn{ty = t} ->
    "func() " <> typeExprGoText t <> " {\n" <> genBody Returning 1 e <> "}()"

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
genExpr (EVar _ i) = identText i
genExpr (EIntLit _ lit) = intLitText lit
genExpr (EFloatLit _ lit) = floatLitText lit
genExpr (EStrLit _ (StringLit t)) = "\"" <> t <> "\""
genExpr (EUnitLit _) = "struct{}{}"
genExpr (EVariadicSpread _ te) = genExpr te <> "..."
genExpr (EIndex _ te tidx) = genExpr te <> "[" <> genExpr tidx <> "]"
genExpr (EInfixOp _ e1 "::" e2) = "append(" <> typeExprGoText (exprType e2) <> "{" <> genExpr e1 <> "}, " <> genExpr e2 <> "...)"
genExpr (EInfixOp _ e1 op e2) = "(" <> genExpr e1 <> " " <> goInfixOp op <> " " <> genExpr e2 <> ")"
genExpr (EApplication _ tf targs) =
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
genExpr e@(EIf {}) = genExprIIFE e
genExpr (ESequence _ []) = error "genExpr: empty ESequence should not reach expression context"
genExpr e@(ESequence {}) = genExprIIFE e
genExpr e@(ELet _ _ _ (Just _)) = genExprIIFE e
genExpr (ELet _ _ _ Nothing) = error "genExpr: ELet without continuation should be in statement context"
genExpr (ESliceLit ExprAnn{ty = TSlice (TNamed (Ident "opaque"))} []) = "nil"
genExpr (ESliceLit ExprAnn{ty = t} []) = typeExprGoText t <> "{}"
genExpr (ESliceLit ExprAnn{ty = t} texprs) = typeExprGoText t <> "{" <> T.intercalate ", " (map genExpr texprs) <> "}"
genExpr (EMapLit ExprAnn{ty = TMap (TNamed (Ident "opaque")) (TNamed (Ident "opaque"))}) = "nil"
genExpr (EMapLit ExprAnn{ty = t}) = typeExprGoText t <> "{}"
genExpr e@(EMatch {}) = genExprIIFE e
genExpr (ELambda _ (Binding params retTy tbody@(ESequence {}))) =
  genLocalFunc 0 (paramListGoText params) retTy tbody
genExpr (ELambda _ (Binding params retTy tbody)) =
  genInlineFunc (paramListGoText params) retTy (genExpr tbody)
-- Function-type coercion wrapper: generates a wrapper lambda that forwards
-- args and adjusts the return type in the unit<->struct{} dimension.
genExpr (ECoerce ExprAnn{ty = targetTy} FuncVoidCoerce inner) =
  case (targetTy, exprType inner) of
    -- () return -> struct{} return
    ( TFunc fixedTys mVarTy (TNamed (Ident "struct{}")),
      TFunc fixedTys' mVarTy' UnitType
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") struct{} { " <> genExpr inner <> "(" <> callArgs <> "); return struct{}{} }"
    -- struct{} return -> () return
    ( TFunc fixedTys mVarTy UnitType,
      TFunc fixedTys' mVarTy' (TNamed (Ident "struct{}"))
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") { " <> genExpr inner <> "(" <> callArgs <> ") }"
    _ -> error $ "genExpr ECoerce FuncVoidCoerce: unhandled types " <> show (exprType inner) <> " -> " <> show targetTy

genImport :: ImportDecl -> T.Text
genImport (ImportDecl Default path) = "import \"" <> path <> "\"\n"
genImport (ImportDecl Dot path) = "import . \"" <> path <> "\"\n"
genImport (ImportDecl Blank path) = "import _ \"" <> path <> "\"\n"
genImport (ImportDecl (Alias i) path) = "import " <> identText i <> " \"" <> path <> "\"\n"

genHeader :: Header -> T.Text
genHeader (Header (PackageClause pkg) []) = "package " <> identText pkg <> "\n"
genHeader (Header (PackageClause pkg) imports) =
  "package "
    <> identText pkg
    <> "\n"
    <> T.concat (map genImport imports)

genDecl :: Ident -> [Param] -> TypeExpr -> Expr -> T.Text
genDecl name [] valTy tbody = "var " <> identText name <> " " <> typeExprGoText valTy <> " = " <> genExpr tbody <> "\n"
genDecl name params retTy tbody =
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
      UnitType -> genBody Void
      _ -> genBody Returning

genPackageLevel :: Expr -> T.Text
genPackageLevel (ELet _ n (Binding p t trhs) mtin) = genDecl n p t trhs <> maybe "" genPackageLevel mtin
genPackageLevel (ESequence _ texprs) = T.concat (map genPackageLevel texprs)
genPackageLevel e
  | isStmt (exprAnn e) = "func init() {\n" <> genBody Void 1 e <> "}\n"
  | otherwise = error $ "unsupported top-level expression: " <> show e

genGoFile :: FogFile -> IO T.Text
genGoFile (FogFile hdr body) = do
  let tbody = case inferAndResolve body of
        Right te -> te
        Left errs -> error $ "inference errors: " <> show errs
      raw = genHeader hdr <> genPackageLevel tbody
  formatted <- readProcess "gofmt" [] (T.unpack raw)
  return $ T.pack formatted
