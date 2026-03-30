module Foglang.Codegen (genGoFile) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foglang.AST (Binding (..), Coercion (..), Expr (..), ExprAnn (..), FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), MatchArm (..), PackageClause (..), Param (..), Pattern (..), StringLit (..), TypeExpr (..), pattern UnitType, exprAnn, exprType)
import Foglang.Inference (inferAndResolve)
import System.Process (readProcess)

-- Scope tracks how many times each name has been bound, enabling shadowing
-- via renaming. First binding emits "x", second emits "x_1", etc.
type Scope = Map.Map Ident Int

emptyScope :: Scope
emptyScope = Map.empty

-- Bind a name in scope. Returns the Go-level name and updated scope.
-- Shadows produce _shadowN suffixes. Fog identifiers ending with _shadow\d+
-- get "double-mangled" (start at _shadow1 instead of bare) to avoid collisions
-- — e.g. fog `x_shadow1` emits `x_shadow1_shadow1`, so it never collides
-- with a shadow of fog `x` which emits `x_shadow1`.
bindName :: Scope -> Ident -> (T.Text, Scope)
bindName scope ident@(Ident name) =
  case Map.lookup ident scope of
    Nothing
      | endsWithShadowDigits name ->
          (name <> "_shadow1", Map.insert ident 1 scope)
      | otherwise ->
          (name, Map.insert ident 0 scope)
    Just n ->
      (name <> "_shadow" <> T.pack (show (n + 1)), Map.insert ident (n + 1) scope)

-- Does a name end with _shadow\d+ (could collide with shadow suffixes)?
endsWithShadowDigits :: T.Text -> Bool
endsWithShadowDigits name =
  case T.breakOnEnd "_shadow" name of
    ("", _) -> False
    (_, digits) -> not (T.null digits) && T.all (\c -> c >= '0' && c <= '9') digits

-- Bind function parameters into scope (params don't get renamed — they're
-- always fresh in their Go scope — but we register them so that references
-- inside the body resolve correctly).
bindParams :: Scope -> [Param] -> Scope
bindParams scope [] = scope
bindParams scope [PUnit] = scope
bindParams scope (PTyped name _ : rest) = bindParams (snd (bindName scope name)) rest
bindParams scope (PVariadic name _ : rest) = bindParams (snd (bindName scope name)) rest
bindParams scope (PUnit : rest) = bindParams scope rest

-- Resolve a name to its current Go-level name.
resolveName :: Scope -> Ident -> T.Text
resolveName scope ident@(Ident name) =
  case Map.lookup ident scope of
    Nothing -> name  -- not yet bound (e.g. top-level Go import like fmt.Println)
    Just 0  -> name  -- first binding, no suffix
    Just n  -> name <> "_shadow" <> T.pack (show n)

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
genLocalFunc :: Scope -> Int -> T.Text -> TypeExpr -> Expr -> T.Text
genLocalFunc scope indent params retTy trhs =
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
      UnitType -> genBody scope Void
      _ -> genBody scope Returning

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
typeExprGoText (TFunc [UnitType] Nothing retTy) =
  "func()" <> retTypeGoText retTy
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
closureParamsAndCallArgs [UnitType] Nothing = ("", [])
closureParamsAndCallArgs fixedTys mVarTy = (paramListGoText params, callArgNames params)
  where
    params = syntheticParams fixedTys mVarTy
    callArgNames = map argName
    argName (PTyped name _) = identText name
    argName (PVariadic name _) = identText name <> "..."
    argName PUnit = error "closureParamsAndCallArgs: unexpected PUnit"

genElsePart :: Scope -> BodyMode -> Int -> Expr -> T.Text
genElsePart scope mode indent (EIf _ cond then' else') =
  " else if "
    <> genExpr scope cond
    <> " {\n"
    <> genBody scope mode (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart scope mode indent else'
genElsePart scope mode indent e =
  " else {\n"
    <> genBody scope mode (indent + 1) e
    <> ind indent
    <> "}"

genIfChain :: Scope -> BodyMode -> Int -> Expr -> Expr -> Expr -> T.Text
genIfChain scope mode indent cond then' else' =
  ind indent
    <> "if "
    <> genExpr scope cond
    <> " {\n"
    <> genBody scope mode (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart scope mode indent else'
    <> "\n"

data BodyMode = Returning | Void

-- Generate a match expression as a statement-level if/else chain.
genMatchBody :: Scope -> BodyMode -> Int -> Expr -> [MatchArm] -> T.Text
genMatchBody _ _ _ _ [] = ""
genMatchBody scope mode indent tscrut arms =
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
             in ind indent <> vars <> " := " <> genExpr scope tscrut <> "\n" <> useVars <> alias
        | otherwise = ind indent <> scrutVar <> " := " <> genExpr scope tscrut <> "\n" <> useVar indent scrutVar
      genArm isFirst isLast (MatchArm _ pat tbody) =
        let (cond, bindings) = genPatternCond scrutVar pat
            keyword'
              | isFirst = "if " <> cond <> " "
              | isLast && isIrrefutablePattern pat = "} else "
              | otherwise = "} else if " <> cond <> " "
            bindingText = T.concat [ind (indent + 1) <> n <> " := " <> v <> "\n" <> useVar (indent + 1) n | (n, v) <- bindings]
         in ind indent <> keyword' <> "{\n" <> bindingText <> genBody scope mode (indent + 1) tbody
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
genPatternCond scrut (PtStrLit (StringLit s)) = (scrut <> " == \"" <> s <> "\"", [])
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
genCont :: Scope -> BodyMode -> Int -> Maybe Expr -> T.Text
genCont _ _ _ Nothing = ""
genCont scope mode indent (Just tin) = genBody scope mode indent tin

-- Generate a function body. Returning: last expression gets 'return'.
-- Void: all expressions emitted as statements.
genBody :: Scope -> BodyMode -> Int -> Expr -> T.Text
genBody _ Void _ (EUnitLit _) = ""
genBody scope mode indent (EIf _ cond then' else') = genIfChain scope mode indent cond then' else'
genBody scope mode indent (EMatch _ tscrut tarms) = genMatchBody scope mode indent tscrut tarms
genBody scope Void indent (ESequence _ texprs) = T.concat (map (genBody scope Void indent) texprs)
genBody scope Returning indent (ESequence _ texprs)
  | null texprs = error "genBody Returning: empty ESequence (should be unreachable)"
  | otherwise =
      T.concat (map (genBody scope Void indent) (init texprs))
        <> genBody scope Returning indent (last texprs)
-- Void binding: RHS is void (can't bind in Go), or declared type is unit
genBody scope mode indent (ELet _ (Ident "_") (Binding [] retTy trhs) mtin)
  | retTy == UnitType || exprType trhs == UnitType =
      genBody scope Void indent trhs
        <> genCont scope mode indent mtin
genBody scope mode indent (ELet _ name (Binding [] retTy trhs) mtin)
  | retTy == UnitType || exprType trhs == UnitType =
      let (goName, scope') = bindName scope name
      in genBody scope Void indent trhs
        <> ind indent <> goName <> " := struct{}{}\n"
        <> useVar indent goName
        <> genCont scope' mode indent mtin
-- Normal value binding.
genBody scope mode indent (ELet _ name (Binding [] _ trhs) mtin) =
  let (goName, scope') = bindName scope name
  in ind indent <> goName <> " := " <> genExpr scope trhs <> "\n"
    <> useVar indent goName
    <> genCont scope' mode indent mtin
genBody scope mode indent (ELet _ name (Binding params retTy trhs) mtin) =
  -- Use var declaration + assignment for local functions to support recursion.
  -- Go closures can't reference themselves with :=, but can with var + =.
  let (goName, scope') = bindName scope name
      -- Bind the function name before generating the body, so recursive
      -- calls resolve to the correct name.
      paramScope = bindParams scope' params
      paramsText = paramListGoText params
      funcTy = "func(" <> paramsText <> ")" <> retTypeGoText retTy
   in ind indent
        <> "var "
        <> goName
        <> " "
        <> funcTy
        <> "\n"
        <> ind indent
        <> goName
        <> " = "
        <> genLocalFunc paramScope indent paramsText retTy trhs
        <> "\n"
        <> useVar indent goName
        <> genCont scope' mode indent mtin
genBody scope Void indent te
  | isStmt (exprAnn te) = ind indent <> genExpr scope te <> "\n"
  | otherwise = ind indent <> "_ = " <> genExpr scope te <> "\n"
genBody scope Returning indent te = ind indent <> "return " <> genExpr scope te <> "\n"

-- Wrap a compound expression in an immediately-invoked function expression.
-- Three cases:
--   void + side effects -> func() struct{} { <Void body>; return struct{}{} }()
--   void + pure         -> struct{}{}
--   non-void            -> func() T { <Returning body> }()
genExprIIFE :: Scope -> Expr -> T.Text
genExprIIFE scope e = case exprAnn e of
  ExprAnn{ty = UnitType, isStmt = True} ->
    "func() struct{} {\n" <> genBody scope Void 1 e <> "\treturn struct{}{}\n}()"
  ExprAnn{ty = UnitType} -> "struct{}{}"
  ExprAnn{ty = t} ->
    "func() " <> typeExprGoText t <> " {\n" <> genBody scope Returning 1 e <> "}()"

-- Map foglang operators to Go operators (triple-char bitwise/shift -> Go equivalents)
goInfixOp :: T.Text -> T.Text
goInfixOp "|||" = "|"
goInfixOp "&&&" = "&"
goInfixOp "^^^" = "^"
goInfixOp "<<<" = "<<"
goInfixOp ">>>" = ">>"
goInfixOp op = op

genExpr :: Scope -> Expr -> T.Text
genExpr scope (EVar _ i) = resolveName scope i
genExpr _ (EIntLit _ lit) = intLitText lit
genExpr _ (EFloatLit _ lit) = floatLitText lit
genExpr _ (EStrLit _ (StringLit t)) = "\"" <> t <> "\""
genExpr _ (EUnitLit _) = "struct{}{}"
genExpr scope (EVariadicSpread _ te) = genExpr scope te <> "..."
genExpr scope (EIndex _ te tidx) = genExpr scope te <> "[" <> genExpr scope tidx <> "]"
genExpr scope (EInfixOp _ e1 "::" e2) = "append(" <> typeExprGoText (exprType e2) <> "{" <> genExpr scope e1 <> "}, " <> genExpr scope e2 <> "...)"
genExpr scope (EInfixOp _ e1 op e2) = "(" <> genExpr scope e1 <> " " <> goInfixOp op <> " " <> genExpr scope e2 <> ")"
genExpr scope (EApplication _ tf targs) =
  case exprType tf of
    TFunc fixedTys mVarTy retTy ->
      let nFixed = length fixedTys
          nSupplied = length targs
          isPartial = nSupplied < nFixed || (nSupplied == nFixed && mVarTy /= Nothing)
       in if isPartial
            then
              let (closureParams, argNames) = closureParamsAndCallArgs (drop nSupplied fixedTys) mVarTy
                  callArgs = T.intercalate ", " (map (genExpr scope) targs ++ argNames)
               in genInlineFunc closureParams retTy (genExpr scope tf <> "(" <> callArgs <> ")")
            else
              let -- A single EUnitLit arg matching a single UnitType param is the
                  -- zero-param call convention: f() in fog -> f() in Go, not f(struct{}{}).
                  isZeroArgCall = case (fixedTys, targs) of
                    ([UnitType], [EUnitLit _]) -> True
                    _ -> False
                  args = case mVarTy of
                    Nothing ->
                      if isZeroArgCall then [] else map (genExpr scope) targs
                    Just _ ->
                      -- A lone () after fixed args is the sentinel for "no variadic args".
                      let varArgs = case drop nFixed targs of
                            [EUnitLit _] -> []
                            rest -> rest
                       in map (genExpr scope) (take nFixed targs) ++ map (genExpr scope) varArgs
               in genExpr scope tf <> "(" <> T.intercalate ", " args <> ")"
    _ -> genExpr scope tf <> "(" <> T.intercalate ", " (map (genExpr scope) targs) <> ")"
genExpr scope e@(EIf {}) = genExprIIFE scope e
genExpr _ (ESequence _ []) = error "genExpr: empty ESequence should not reach expression context"
genExpr scope e@(ESequence {}) = genExprIIFE scope e
genExpr scope e@(ELet _ _ _ (Just _)) = genExprIIFE scope e
genExpr _ (ELet _ _ _ Nothing) = error "genExpr: ELet without continuation should be in statement context"
genExpr _ (ESliceLit ExprAnn{ty = TSlice (TNamed (Ident "opaque"))} []) = "nil"
genExpr _ (ESliceLit ExprAnn{ty = t} []) = typeExprGoText t <> "{}"
genExpr scope (ESliceLit ExprAnn{ty = t} texprs) = typeExprGoText t <> "{" <> T.intercalate ", " (map (genExpr scope) texprs) <> "}"
genExpr _ (EMapLit ExprAnn{ty = TMap (TNamed (Ident "opaque")) (TNamed (Ident "opaque"))}) = "nil"
genExpr _ (EMapLit ExprAnn{ty = t}) = typeExprGoText t <> "{}"
genExpr scope e@(EMatch {}) = genExprIIFE scope e
genExpr scope (ELambda _ (Binding params retTy tbody@(ESequence {}))) =
  genLocalFunc scope 0 (paramListGoText params) retTy tbody
genExpr scope (ELambda _ (Binding params retTy tbody)) =
  genInlineFunc (paramListGoText params) retTy (genExpr scope tbody)
-- Function-type coercion wrapper: generates a wrapper lambda that forwards
-- args and adjusts the return type in the unit<->struct{} dimension.
genExpr scope (ECoerce ExprAnn{ty = targetTy} FuncVoidCoerce inner) =
  case (targetTy, exprType inner) of
    -- () return -> struct{} return
    ( TFunc fixedTys mVarTy (TNamed (Ident "struct{}")),
      TFunc fixedTys' mVarTy' UnitType
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") struct{} { " <> genExpr scope inner <> "(" <> callArgs <> "); return struct{}{} }"
    -- struct{} return -> () return
    ( TFunc fixedTys mVarTy UnitType,
      TFunc fixedTys' mVarTy' (TNamed (Ident "struct{}"))
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let (closureParams, argNames) = closureParamsAndCallArgs fixedTys mVarTy
                callArgs = T.intercalate ", " argNames
             in "func(" <> closureParams <> ") { " <> genExpr scope inner <> "(" <> callArgs <> ") }"
    _ -> error $ "genExpr ECoerce FuncVoidCoerce: unhandled types " <> show (exprType inner) <> " -> " <> show targetTy

genImport :: ImportDecl -> T.Text
genImport (ImportDecl Default path) = "import \"" <> path <> "\"\n"
genImport (ImportDecl Dot path) = "import . \"" <> path <> "\"\n"
genImport (ImportDecl Blank path) = "import _ \"" <> path <> "\"\n"
genImport (ImportDecl (Alias i) path) = "import " <> identText i <> " \"" <> path <> "\"\n"

genHeader :: Header -> T.Text
genHeader (Header (PackageClause pkg) imports) =
  "package "
    <> identText pkg
    <> "\n"
    <> T.concat (map genImport imports)

genDecl :: Scope -> Ident -> [Param] -> TypeExpr -> Expr -> T.Text
genDecl scope name [] valTy tbody = "var " <> identText name <> " " <> typeExprGoText valTy <> " = " <> genExpr scope tbody <> "\n"
genDecl scope name params retTy tbody =
  let paramScope = bindParams scope params
  in "func "
    <> identText name
    <> "("
    <> paramListGoText params
    <> ")"
    <> retTypeGoText retTy
    <> " {\n"
    <> bodyText paramScope 1 tbody
    <> "}\n"
  where
    bodyText bodySc = case retTy of
      UnitType -> genBody bodySc Void
      _ -> genBody bodySc Returning

genPackageLevel :: Scope -> Expr -> (T.Text, Scope)
genPackageLevel scope (ELet _ n (Binding p t trhs) mtin) =
  let (goName, scope') = bindName scope n
      -- Value bindings: RHS uses old scope (before this binding).
      -- Function bindings: body uses new scope (for recursion).
      rhsScope = case p of [] -> scope; _ -> scope'
      declText = genDecl rhsScope (Ident goName) p t trhs
  in case mtin of
    Nothing -> (declText, scope')
    Just tin -> let (rest, scope'') = genPackageLevel scope' tin in (declText <> rest, scope'')
genPackageLevel scope (ESequence _ texprs) =
  foldl (\(acc, s) e -> let (t, s') = genPackageLevel s e in (acc <> t, s')) ("", scope) texprs
genPackageLevel scope e
  | isStmt (exprAnn e) = ("func init() {\n" <> genBody scope Void 1 e <> "}\n", scope)
  | otherwise = error $ "unsupported top-level expression: " <> show e

genGoFile :: FogFile -> IO T.Text
genGoFile (FogFile hdr body) = do
  let tbody = case inferAndResolve body of
        Right te -> te
        Left errs -> error $ "inference errors: " <> show errs
      (bodyText, _) = genPackageLevel emptyScope tbody
      raw = genHeader hdr <> bodyText
  formatted <- readProcess "gofmt" [] (T.unpack raw)
  return $ T.pack formatted
