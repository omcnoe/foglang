module Foglang.Codegen (codegen, codegenGoFile) where

import Data.Text qualified as T
import Foglang.AST (Expr (..), FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), PackageClause (..), QualIdent (..))

ind :: Int -> T.Text
ind n = T.replicate n "\t"

identText :: Ident -> T.Text
identText (Ident t) = t

qualIdentText :: QualIdent -> T.Text
qualIdentText (QualIdent parts) = T.intercalate "." (map identText parts)

intLitText :: IntLit -> T.Text
intLitText (Decimal t) = t
intLitText (Binary t) = t
intLitText (Octal t) = t
intLitText (Hex t) = t

floatLitText :: FloatLit -> T.Text
floatLitText (DecimalFloat t) = t
floatLitText (HexFloat t) = t

genElsePart :: Int -> Expr -> T.Text
genElsePart indent (If cond then' else') =
  " else if "
    <> genExpr cond
    <> " {\n"
    <> genBody (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart indent else'
genElsePart indent e =
  " else {\n"
    <> genBody (indent + 1) e
    <> ind indent
    <> "}"

genIfChain :: Int -> Expr -> Expr -> Expr -> T.Text
genIfChain indent cond then' else' =
  ind indent
    <> "if "
    <> genExpr cond
    <> " {\n"
    <> genBody (indent + 1) then'
    <> ind indent
    <> "}"
    <> genElsePart indent else'
    <> "\n"

-- Generate statements for a function body, with the given indent level
genBody :: Int -> Expr -> T.Text
genBody indent (If cond then' else') = genIfChain indent cond then' else'
genBody indent e = ind indent <> "return " <> genExpr e <> "\n"

genFunc :: Ident -> [Ident] -> Expr -> T.Text
genFunc name params body =
  "func "
    <> identText name
    <> "("
    <> paramList
    <> ") any {\n"
    <> genBody 1 body
    <> "}\n"
  where
    paramList = T.intercalate ", " (map ((<> " any") . identText) params)

-- Generate a Go expression. BinaryOp sub-expressions are parenthesised to
-- preserve foglang's precedences without relying on Go's
genExpr :: Expr -> T.Text
genExpr (Var qi) = qualIdentText qi
genExpr (IntLit lit) = intLitText lit
genExpr (FloatLit lit) = floatLitText lit
genExpr (BinaryOp e1 op e2) = "(" <> genExpr e1 <> " " <> op <> " " <> genExpr e2 <> ")"
genExpr (Application f args) = genExpr f <> "(" <> T.intercalate ", " (map genExpr args) <> ")"
genExpr (If cond then' else') =
  "func() any { if " <> genExpr cond <> " { return " <> genExpr then' <> " }; return " <> genExpr else' <> " }()"
genExpr (Let name [] body) =
  "func() any { " <> identText name <> " := " <> genExpr body <> "; return " <> identText name <> " }()"
genExpr (Let name params body) = genFunc name params body

codegenImport :: ImportDecl -> T.Text
codegenImport (ImportDecl None path) = "import \"" <> path <> "\"\n"
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

codegenDecl :: Expr -> T.Text
codegenDecl (Let name [] body) = "var " <> identText name <> " = " <> genExpr body <> "\n"
codegenDecl (Let name params body) = genFunc name params body
codegenDecl e = genExpr e <> "\n"

header :: T.Text
header = "package main\n\n"

-- Generate a Go top-level declaration from a foglang expression
codegen :: Expr -> T.Text
codegen (Let name [] body) = header <> "var " <> identText name <> " = " <> genExpr body <> "\n"
codegen (Let name params body) = header <> genFunc name params body
codegen e = header <> genExpr e <> "\n"

codegenGoFile :: FogFile -> T.Text
codegenGoFile (FogFile hdr exprs) = codegenHeader hdr <> T.concat (map codegenDecl exprs)
