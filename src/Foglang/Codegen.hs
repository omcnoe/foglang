module Foglang.Codegen (codegenGoFile) where

import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Foglang.AST (FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), PackageClause (..), Param (..), StringLit (..), TypeExpr (..))
import Foglang.Elaboration (elabExpr, preludeEnv)
import Foglang.TAST (TBinding (..), TExpr (..), tExprType)

ind :: Int -> T.Text
ind n = T.replicate n "\t"

-- Wrap a single expression as a Go inline closure body.
-- Unit-returning closures omit the return type and 'return' keyword.
closureExpr :: T.Text -> TypeExpr -> T.Text -> T.Text
closureExpr params (NamedType (Ident "unit")) body = "func(" <> params <> ") { " <> body <> " }"
closureExpr params retTy body = "func(" <> params <> ") " <> typeExprGoText retTy <> " { return " <> body <> " }"

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
typeExprGoText (NamedType (Ident "unit")) = ""
typeExprGoText (NamedType (Ident t)) = t
typeExprGoText (SliceType t) = "[]" <> typeExprGoText t
typeExprGoText (FuncType fixedTys mVarTy retTy) =
  "func("
    <> T.intercalate ", " (map typeExprGoText fixedTys ++ maybe [] (\vTy -> ["..." <> typeExprGoText vTy]) mVarTy)
    <> ") "
    <> typeExprGoText retTy

paramGoText :: Param -> Maybe T.Text
paramGoText UnitParam = Nothing
paramGoText (VariadicParam name ty) = Just $ identText name <> " ..." <> typeExprGoText ty
paramGoText (TypedParam name ty) = Just $ identText name <> " " <> typeExprGoText ty

paramListText :: [Param] -> T.Text
paramListText params = T.intercalate ", " (mapMaybe paramGoText params)

genElsePart :: Int -> TExpr -> T.Text
genElsePart indent (TIf _ cond then' else') =
  " else if "
    <> genTExpr cond
    <> " {\n"
    <> genTBody (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart indent else'
genElsePart indent e =
  " else {\n"
    <> genTBody (indent + 1) e
    <> ind indent
    <> "}"

genIfChain :: Int -> TExpr -> TExpr -> TExpr -> T.Text
genIfChain indent cond then' else' =
  ind indent
    <> "if "
    <> genTExpr cond
    <> " {\n"
    <> genTBody (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart indent else'
    <> "\n"

-- Generate statements for a function body, with the given indent level.
genTBody :: Int -> TExpr -> T.Text
genTBody indent (TIf _ cond then' else') = genIfChain indent cond then' else'
genTBody indent (TSequence _ texprs)
  | null texprs = ind indent <> "return ()\n"
  | otherwise =
      T.concat (map (genTStmtBody indent) (init texprs))
        <> genTBody indent (last texprs)
genTBody indent (TLet _ name (TBinding [] _ trhs) tin) =
  ind indent
    <> identText name
    <> " := "
    <> genTExpr trhs
    <> "\n"
    <> genTBody indent tin
genTBody indent (TLet _ name (TBinding params retTy trhs) tin) =
  ind indent
    <> identText name
    <> " := "
    <> closureExpr (paramListText params) retTy (genTExpr trhs)
    <> "\n"
    <> genTBody indent tin
genTBody indent te = ind indent <> "return " <> genTExpr te <> "\n"

genFunc :: Ident -> [Param] -> TypeExpr -> TExpr -> T.Text
genFunc name params retTy tbody =
  "func "
    <> identText name
    <> "("
    <> paramListText params
    <> ")"
    <> retTypeText
    <> " {\n"
    <> bodyText 1 tbody
    <> "}\n\n"
  where
    retTypeText = case retTy of
      NamedType (Ident "unit") -> ""
      ty -> " " <> typeExprGoText ty
    bodyText = case retTy of
      NamedType (Ident "unit") -> genTStmtBody
      _ -> genTBody

genMainFunc :: TExpr -> T.Text
genMainFunc tbody =
  "func main() {\n"
    <> genTStmtBody 1 tbody
    <> "}\n"

-- Like genTBody but emits the expression as a plain statement (no 'return').
genTStmtBody :: Int -> TExpr -> T.Text
genTStmtBody indent (TIf _ cond then' else') = genIfChain indent cond then' else'
genTStmtBody indent (TSequence _ texprs) = T.concat (map (genTStmtBody indent) texprs)
genTStmtBody indent (TLet _ name (TBinding [] _ trhs) tin) =
  ind indent
    <> identText name
    <> " := "
    <> genTExpr trhs
    <> "\n"
    <> genTStmtBody indent tin
genTStmtBody indent (TLet _ name (TBinding params retTy trhs) tin) =
  ind indent
    <> identText name
    <> " := "
    <> closureExpr (paramListText params) retTy (genTExpr trhs)
    <> "\n"
    <> genTStmtBody indent tin
genTStmtBody indent te = ind indent <> genTExpr te <> "\n"

-- Generate a Go expression from a TExpr.
-- For Application nodes, the function's TExpr carries its type, enabling
-- arity-aware partial application codegen.
genTExpr :: TExpr -> T.Text

isUnitArg :: TExpr -> Bool
isUnitArg TUnitLit = True
isUnitArg _ = False
genTExpr (TVar _ i) = identText i
genTExpr (TIntLit _ lit) = intLitText lit
genTExpr (TFloatLit _ lit) = floatLitText lit
genTExpr (TStrLit _ (StringLit t)) = "\"" <> t <> "\""
genTExpr TUnitLit = "()"
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
             if nSupplied < nFixed
               then
                 let remainingTys = drop nSupplied fixedTys
                     freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length remainingTys - 1]]
                     closureParams = T.intercalate ", " $ zipWith (\n ty -> n <> " " <> typeExprGoText ty) freshNames remainingTys
                     callArgs = T.intercalate ", " (map genTExpr targs ++ freshNames)
                  in closureExpr closureParams retTy (genTExpr tf <> "(" <> callArgs <> ")")
               else genTExpr tf <> "(" <> T.intercalate ", " (map genTExpr targs) <> ")"
           Just varTy ->
             if nSupplied <= nFixed
               then
                 -- Partial application: some or all fixed params supplied, variadic slot not yet filled.
                 -- Generate a variadic closure capturing the supplied fixed args.
                 let remainingFixed = drop nSupplied fixedTys
                     freshNames = ["_p" <> T.pack (show i) | i <- [0 .. length remainingFixed - 1]]
                     varArgName = "_args"
                     closureParams = T.intercalate ", " $
                       zipWith (\n ty -> n <> " " <> typeExprGoText ty) freshNames remainingFixed
                         ++ [varArgName <> " ..." <> typeExprGoText varTy]
                     callArgs = T.intercalate ", " $
                       map genTExpr targs ++ freshNames ++ [varArgName <> "..."]
                  in closureExpr closureParams retTy (genTExpr tf <> "(" <> callArgs <> ")")
               else
                 -- Full variadic call: fixed params supplied + variadic args (or () sentinel).
                 -- () sentinel is stripped; TVariadicSpread nodes emit expr... via genTExpr.
                 let varArgs = filter (not . isUnitArg) (drop nFixed targs)
                     allArgs = map genTExpr (take nFixed targs) ++ map genTExpr varArgs
                  in genTExpr tf <> "(" <> T.intercalate ", " allArgs <> ")"
    _ -> genTExpr tf <> "(" <> T.intercalate ", " (map genTExpr targs) <> ")"
genTExpr (TIf _ cond then' else') =
  "func() any { if " <> genTExpr cond <> " { return " <> genTExpr then' <> " }; return " <> genTExpr else' <> " }()"
genTExpr (TSequence _ []) = "()"
genTExpr (TSequence _ texprs) =
  "func() any { "
    <> T.intercalate "; " (map genTExpr (init texprs))
    <> "; return "
    <> genTExpr (last texprs)
    <> " }()"
genTExpr (TLet _ name (TBinding [] _ trhs) tin) =
  "func() any { "
    <> identText name
    <> " := "
    <> genTExpr trhs
    <> "; return "
    <> genTExpr tin
    <> " }()"
genTExpr (TLet _ name (TBinding params retTy trhs) tin) =
  "func() any { "
    <> identText name
    <> " := func("
    <> paramListText params
    <> ") "
    <> typeExprGoText retTy
    <> " { return "
    <> genTExpr trhs
    <> " }; return "
    <> genTExpr tin
    <> " }()"
genTExpr (TLambda _ (TBinding params retTy tbody)) = case retTy of
  NamedType (Ident "unit") ->
    "func(" <> paramListText params <> ") { " <> genTExpr tbody <> " }"
  _ ->
    "func(" <> paramListText params <> ") " <> typeExprGoText retTy <> " { return " <> genTExpr tbody <> " }"

codegenImport :: ImportDecl -> T.Text
codegenImport (ImportDecl Default path) = "import \"" <> path <> "\"\n"
codegenImport (ImportDecl Dot path) = "import . \"" <> path <> "\"\n"
codegenImport (ImportDecl Blank path) = "import _ \"" <> path <> "\"\n"
codegenImport (ImportDecl (Alias i) path) = "import " <> identText i <> " \"" <> path <> "\"\n"

codegenHeader :: Header -> T.Text
codegenHeader (Header (PackageClause pkg) []) = "package " <> identText pkg <> "\n\n"
codegenHeader (Header (PackageClause pkg) imports) =
  "package "
    <> identText pkg
    <> "\n\n"
    <> T.concat (map codegenImport imports)
    <> "\n"

codegenDecl :: Ident -> [Param] -> TypeExpr -> TExpr -> T.Text
codegenDecl (Ident "main") _ _ tbody = genMainFunc tbody
codegenDecl name [] valTy (TSequence _ texprs) =
  "var "
    <> identText name
    <> " "
    <> typeExprGoText valTy
    <> " = func() "
    <> typeExprGoText valTy
    <> " {\n"
    <> genTBody 1 (TSequence (NamedType (Ident "unit")) texprs)
    <> "}()\n\n"
codegenDecl name [] valTy tbody = "var " <> identText name <> " " <> typeExprGoText valTy <> " = " <> genTExpr tbody <> "\n"
codegenDecl name params retTy tbody = genFunc name params retTy tbody

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

codegenGoFile :: FogFile -> T.Text
codegenGoFile (FogFile hdr body) =
  let tbody = case elabExpr preludeEnv body of
        Right te -> te
        Left err -> error $ "elaboration error: " <> show err
   in codegenHeader hdr <> codegenTopLevel tbody
