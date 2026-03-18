module Foglang.Codegen (codegenGoFile) where

import Data.Text qualified as T
import Foglang.AST (FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), PackageClause (..), Param (..), StringLit (..), TypeExpr (..))
import Foglang.Elaboration (elabExpr, preludeEnv)
import Foglang.TAST (TBinding (..), TExpr (..), tExprType)
import System.Process (readProcess)

ind :: Int -> T.Text
ind n = T.replicate n "\t"

-- Wrap a single expression as a Go inline closure body.
-- Unit-returning closures omit the return type and 'return' keyword.
closureExpr :: T.Text -> TypeExpr -> T.Text -> T.Text
closureExpr params (NamedType (Ident "()")) body = "func(" <> params <> ") { " <> body <> " }"
closureExpr params retTy body = "func(" <> params <> ") " <> typeExprGoText retTy <> " { return " <> body <> " }"

-- Render a let-bound local function as a multi-line func literal,
-- using the appropriate statement/body generator for the function body.
genLocalFunc :: Int -> T.Text -> TypeExpr -> TExpr -> T.Text
genLocalFunc indent params retTy trhs =
  "func("
    <> params
    <> ")"
    <> retTypeText
    <> " {\n"
    <> bodyText (indent + 1) trhs
    <> ind indent
    <> "}"
  where
    retTypeText = case retTy of
      NamedType (Ident "()") -> ""
      ty -> " " <> typeExprGoText ty
    bodyText = case retTy of
      NamedType (Ident "()") -> genTStmtBody
      _ -> genTBody

identText :: Ident -> T.Text
identText (Ident t) = t

intLitText :: IntLit -> T.Text
intLitText (Decimal t) = t
intLitText (Binary t) = t
intLitText (Octal t) = t
intLitText (Hex t) = t

floatLitText :: FloatLit -> T.Text
floatLitText (DecimalFloat t) = t
floatLitText (HexFloat t) = t

typeExprGoText :: TypeExpr -> T.Text
typeExprGoText (NamedType (Ident "()")) = ""
typeExprGoText (NamedType (Ident t)) = t
typeExprGoText (SliceType t) = "[]" <> typeExprGoText t
typeExprGoText (FuncType fixedTys mVarTy retTy) =
  "func("
    <> T.intercalate ", " (map paramTypeGoText fixedTys ++ maybe [] (\vTy -> ["..." <> typeExprGoText vTy]) mVarTy)
    <> ")"
    <> case retTy of
      NamedType (Ident "()") -> ""
      _ -> " " <> typeExprGoText retTy

-- Like typeExprGoText but maps unit to struct{} for use in parameter positions.
paramTypeGoText :: TypeExpr -> T.Text
paramTypeGoText (NamedType (Ident "()")) = "struct{}"
paramTypeGoText t = typeExprGoText t

-- Render a parameter list as Go source.
-- A sole anonymous UnitParam is the zero-param rewrite: produces "" (no params).
-- In any other param list, each UnitParam becomes "_pN struct{}".
paramListGoText :: [Param] -> T.Text
paramListGoText [UnitParam] = ""
paramListGoText params = T.intercalate ", " $ zipWith paramText [0 :: Int ..] params
  where
    paramText _ (VariadicParam name ty) = identText name <> " ..." <> typeExprGoText ty
    paramText _ (TypedParam name ty) = identText name <> " " <> paramTypeGoText ty
    paramText i UnitParam = "_p" <> T.pack (show i) <> " struct{}"

genElsePart :: BodyMode -> Int -> TExpr -> T.Text
genElsePart mode indent (TIf _ cond then' else') =
  " else if "
    <> genTExpr cond
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

genIfChain :: BodyMode -> Int -> TExpr -> TExpr -> TExpr -> T.Text
genIfChain mode indent cond then' else' =
  ind indent
    <> "if "
    <> genTExpr cond
    <> " {\n"
    <> genBody mode (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart mode indent else'
    <> "\n"

data BodyMode = ReturnLast | StmtOnly

-- Unified body generator. ReturnLast emits 'return expr' for the final expression;
-- StmtOnly emits it as a plain statement.
genBody :: BodyMode -> Int -> TExpr -> T.Text
genBody StmtOnly _ TUnitLit = ""
genBody mode indent (TIf _ cond then' else') = genIfChain mode indent cond then' else'
genBody StmtOnly indent (TSequence _ texprs) = T.concat (map (genBody StmtOnly indent) texprs)
genBody ReturnLast indent (TSequence _ texprs)
  | null texprs = ind indent <> "return ()\n"
  | otherwise =
      T.concat (map (genBody StmtOnly indent) (init texprs))
        <> genBody ReturnLast indent (last texprs)
genBody mode indent (TLet _ name (TBinding [] retTy trhs) tin)
  | retTy == NamedType (Ident "()") || tExprType trhs == NamedType (Ident "()"),
    name == Ident "_" =
      ind indent
        <> genTExpr trhs
        <> "\n"
        <> genBody mode indent tin
  | retTy == NamedType (Ident "()") || tExprType trhs == NamedType (Ident "()") =
      ind indent
        <> genTExpr trhs
        <> "\n"
        <> ind indent
        <> identText name
        <> " := struct{}{}\n"
        <> genBody mode indent tin
  | otherwise =
      ind indent
        <> identText name
        <> " := "
        <> genTExpr trhs
        <> "\n"
        <> genBody mode indent tin
genBody mode indent (TLet _ name (TBinding params retTy trhs) tin) =
  ind indent
    <> identText name
    <> " := "
    <> genLocalFunc indent (paramListGoText params) retTy trhs
    <> "\n"
    <> genBody mode indent tin
genBody StmtOnly indent te@(TApplication _ _ _) = ind indent <> genTExpr te <> "\n"
genBody StmtOnly indent te = ind indent <> "_ = " <> genTExpr te <> "\n"
genBody ReturnLast indent te = ind indent <> "return " <> genTExpr te <> "\n"

genTBody :: Int -> TExpr -> T.Text
genTBody = genBody ReturnLast

genTStmtBody :: Int -> TExpr -> T.Text
genTStmtBody = genBody StmtOnly

genFunc :: Ident -> [Param] -> TypeExpr -> TExpr -> T.Text
genFunc name params retTy tbody =
  "func "
    <> identText name
    <> "("
    <> paramListGoText params
    <> ")"
    <> retTypeText
    <> " {\n"
    <> bodyText 1 tbody
    <> "}\n"
  where
    retTypeText = case retTy of
      NamedType (Ident "()") -> ""
      ty -> " " <> typeExprGoText ty
    bodyText = case retTy of
      NamedType (Ident "()") -> genTStmtBody
      _ -> genTBody

isUnitArg :: TExpr -> Bool
isUnitArg TUnitLit = True
isUnitArg _ = False

-- Generate a Go expression from a TExpr.
-- For Application nodes, the function's TExpr carries its type, enabling
-- arity-aware partial application codegen.
genTExpr :: TExpr -> T.Text
genTExpr (TVar _ i) = identText i
genTExpr (TIntLit _ lit) = intLitText lit
genTExpr (TFloatLit _ lit) = floatLitText lit
genTExpr (TStrLit _ (StringLit t)) = "\"" <> t <> "\""
genTExpr TUnitLit = "struct{}{}"
genTExpr (TVariadicSpread _ te) = genTExpr te <> "..."
genTExpr (TBinaryOp _ e1 op e2) = "(" <> genTExpr e1 <> " " <> op <> " " <> genTExpr e2 <> ")"
genTExpr (TApplication _ tf targs) =
  case tExprType tf of
    FuncType fixedTys mVarTy retTy ->
      let nFixed = length fixedTys
          nSupplied = length targs
       in case mVarTy of
            Nothing ->
              -- Non-variadic: partial if nSupplied < nFixed, else full call.
              -- When nFixed == 0 (UnitParam), strip any () sentinel args.
              if nSupplied < nFixed
                then
                  let remainingTys = drop nSupplied fixedTys
                      freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length remainingTys - 1]]
                      closureParams = T.intercalate ", " $ zipWith (\n ty -> n <> " " <> paramTypeGoText ty) freshNames remainingTys
                      callArgs = T.intercalate ", " (map genTExpr targs ++ freshNames)
                   in closureExpr closureParams retTy (genTExpr tf <> "(" <> callArgs <> ")")
                else
                  let args = if nFixed == 0 then [] else map genTExpr targs
                   in genTExpr tf <> "(" <> T.intercalate ", " args <> ")"
            Just varTy ->
              if nSupplied <= nFixed
                then
                  -- Partial application: some or all fixed params supplied, variadic slot not yet filled.
                  -- Generate a variadic closure capturing the supplied fixed args.
                  let remainingFixed = drop nSupplied fixedTys
                      freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length remainingFixed - 1]]
                      varArgName = "_args"
                      closureParams =
                        T.intercalate ", " $
                          zipWith (\n ty -> n <> " " <> paramTypeGoText ty) freshNames remainingFixed
                            ++ [varArgName <> " ..." <> typeExprGoText varTy]
                      callArgs =
                        T.intercalate ", " $
                          map genTExpr targs ++ freshNames ++ [varArgName <> "..."]
                   in closureExpr closureParams retTy (genTExpr tf <> "(" <> callArgs <> ")")
                else
                  -- Full variadic call: fixed params supplied + variadic args (or () sentinel).
                  -- () sentinel is stripped; TVariadicSpread nodes emit expr... via genTExpr.
                  let varArgs = filter (not . isUnitArg) (drop nFixed targs)
                      allArgs = map genTExpr (take nFixed targs) ++ map genTExpr varArgs
                   in genTExpr tf <> "(" <> T.intercalate ", " allArgs <> ")"
    _ -> genTExpr tf <> "(" <> T.intercalate ", " (map genTExpr targs) <> ")"
genTExpr (TIf ty cond then' else') =
  "func() " <> typeExprGoText ty <> " { if " <> genTExpr cond <> " { return " <> genTExpr then' <> " }; return " <> genTExpr else' <> " }()"
genTExpr (TSequence _ []) = "struct{}{}"
genTExpr (TSequence ty texprs) =
  "func() " <> typeExprGoText ty <> " { "
    <> T.intercalate "; " (map genTExpr (init texprs))
    <> "; return "
    <> genTExpr (last texprs)
    <> " }()"
genTExpr (TLet ty name (TBinding [] _ trhs) tin)
  | tExprType trhs == NamedType (Ident "()"),
    name == Ident "_" =
      "func() "
        <> retTyText
        <> "{ "
        <> genTExpr trhs
        <> "; "
        <> returnKw
        <> genTExpr tin
        <> " }()"
  | tExprType trhs == NamedType (Ident "()") =
      "func() "
        <> retTyText
        <> "{ "
        <> genTExpr trhs
        <> "; "
        <> identText name
        <> " := struct{}{}; "
        <> returnKw
        <> genTExpr tin
        <> " }()"
  | otherwise =
      "func() "
        <> retTyText
        <> "{ "
        <> identText name
        <> " := "
        <> genTExpr trhs
        <> "; "
        <> returnKw
        <> genTExpr tin
        <> " }()"
  where
    retTyText = case ty of
      NamedType (Ident "()") -> ""
      t -> typeExprGoText t <> " "
    returnKw = case ty of
      NamedType (Ident "()") -> ""
      _ -> "return "
genTExpr (TLet ty name (TBinding params retTy trhs) tin) =
  "func() "
    <> outerRetTyText
    <> "{ "
    <> identText name
    <> " := "
    <> closureExpr (paramListGoText params) retTy (genTExpr trhs)
    <> "; "
    <> outerReturnKw
    <> genTExpr tin
    <> " }()"
  where
    outerRetTyText = case ty of
      NamedType (Ident "()") -> ""
      t -> typeExprGoText t <> " "
    outerReturnKw = case ty of
      NamedType (Ident "()") -> ""
      _ -> "return "
genTExpr (TLambda _ (TBinding params retTy tbody@(TSequence _ _))) =
  genLocalFunc 0 (paramListGoText params) retTy tbody
genTExpr (TLambda _ (TBinding params retTy tbody)) = case retTy of
  NamedType (Ident "()") ->
    "func(" <> paramListGoText params <> ") { " <> genTExpr tbody <> " }"
  _ ->
    "func(" <> paramListGoText params <> ") " <> typeExprGoText retTy <> " { return " <> genTExpr tbody <> " }"

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

codegenDecl :: Ident -> [Param] -> TypeExpr -> TExpr -> T.Text
codegenDecl name [] valTy (TSequence _ texprs) =
  "var "
    <> identText name
    <> " "
    <> typeExprGoText valTy
    <> " = func() "
    <> typeExprGoText valTy
    <> " {\n"
    <> genTBody 1 (TSequence (NamedType (Ident "()")) texprs)
    <> "}()\n"
codegenDecl name [] valTy tbody = "var " <> identText name <> " " <> typeExprGoText valTy <> " = " <> coerceValue valTy tbody <> "\n"
codegenDecl name params retTy tbody = genFunc name params retTy tbody

-- Wrap a function value when its declared return type differs from its actual
-- return type in the unit<->struct{} dimension (void unit coercion).
-- Handles any FuncType with matching param lists.
coerceValue :: TypeExpr -> TExpr -> T.Text
coerceValue declTy tbody =
  case (declTy, tExprType tbody) of
    ( FuncType fixedTys mVarTy (NamedType (Ident "struct{}")),
      FuncType fixedTys' mVarTy' (NamedType (Ident "()"))
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length fixedTys - 1]]
                varPart = maybe [] (\vTy -> ["_args ..." <> typeExprGoText vTy]) mVarTy
                closureParams =
                  T.intercalate ", " $
                    zipWith (\n ty -> n <> " " <> paramTypeGoText ty) freshNames fixedTys ++ varPart
                callArgs =
                  T.intercalate ", " $
                    freshNames ++ maybe [] (const ["_args..."]) mVarTy
             in "func(" <> closureParams <> ") struct{} { " <> genTExpr tbody <> "(" <> callArgs <> "); return struct{}{} }"
    ( FuncType fixedTys mVarTy (NamedType (Ident "()")),
      FuncType fixedTys' mVarTy' (NamedType (Ident "struct{}"))
      )
        | fixedTys == fixedTys',
          mVarTy == mVarTy' ->
            let freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length fixedTys - 1]]
                varPart = maybe [] (\vTy -> ["_args ..." <> typeExprGoText vTy]) mVarTy
                closureParams =
                  T.intercalate ", " $
                    zipWith (\n ty -> n <> " " <> paramTypeGoText ty) freshNames fixedTys ++ varPart
                callArgs =
                  T.intercalate ", " $
                    freshNames ++ maybe [] (const ["_args..."]) mVarTy
             in "func(" <> closureParams <> ") { " <> genTExpr tbody <> "(" <> callArgs <> ") }"
    _ -> genTExpr tbody

isStmtTExpr :: TExpr -> Bool
isStmtTExpr (TApplication _ _ _) = True
isStmtTExpr (TIf _ _ _ _) = True
isStmtTExpr (TSequence _ _) = True
isStmtTExpr (TBinaryOp _ _ _ _) = True
isStmtTExpr _ = False

codegenTopLevel :: TExpr -> T.Text
codegenTopLevel (TLet _ n (TBinding p t trhs) tin) = codegenDecl n p t trhs <> codegenTopLevel tin
codegenTopLevel (TSequence _ texprs) = T.concat (map codegenTopLevel texprs)
codegenTopLevel te
  | isStmtTExpr te = "func init() {\n" <> genTStmtBody 1 te <> "}\n"
  | otherwise = ""

codegenGoFile :: FogFile -> IO T.Text
codegenGoFile (FogFile hdr body) = do
  let tbody = case elabExpr preludeEnv body of
        Right te -> te
        Left err -> error $ "elaboration error: " <> show err
      raw = codegenHeader hdr <> codegenTopLevel tbody
  formatted <- readProcess "gofmt" [] (T.unpack raw)
  return $ T.pack formatted
