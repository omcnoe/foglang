module Foglang.Codegen (codegen, codegenGoFile) where

import Data.Text qualified as T
import Foglang.AST (Expr (..), FloatLit (..), GoFile (..), Header (..), Ident, ImportDecl (..), IntLit (..), PackageDecl (..))

ind :: Int -> T.Text
ind n = T.replicate n "\t"

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
    <> name
    <> "("
    <> paramList
    <> ") any {\n"
    <> genBody 1 body
    <> "}\n"
  where
    paramList = T.intercalate ", " (map (<> " any") params)

-- Generate a Go expression. BinaryOp sub-expressions are parenthesised to
-- preserve foglang's precedences without relying on Go's
genExpr :: Expr -> T.Text
genExpr (Ident i) = i
genExpr (IntLit lit) = intLitText lit
genExpr (FloatLit lit) = floatLitText lit
genExpr (BinaryOp e1 op e2) = "(" <> genExpr e1 <> " " <> op <> " " <> genExpr e2 <> ")"
genExpr (Application f args) = genExpr f <> "(" <> T.intercalate ", " (map genExpr args) <> ")"
genExpr (If cond then' else') =
  "func() any { if " <> genExpr cond <> " { return " <> genExpr then' <> " }; return " <> genExpr else' <> " }()"
genExpr (Let name [] body) =
  "func() any { " <> name <> " := " <> genExpr body <> "; return " <> name <> " }()"
genExpr (Let name params body) = genFunc name params body

codegenImport :: ImportDecl -> T.Text
codegenImport (ImportDecl Nothing path) = "import \"" <> path <> "\"\n"
codegenImport (ImportDecl (Just alias) path) = "import " <> alias <> " \"" <> path <> "\"\n"

codegenHeader :: Header -> T.Text
codegenHeader (Header (PackageDecl pkg) []) = "package " <> pkg <> "\n\n"
codegenHeader (Header (PackageDecl pkg) imports) =
  "package "
    <> pkg
    <> "\n\n"
    <> T.concat (map codegenImport imports)
    <> "\n"

codegenDecl :: Expr -> T.Text
codegenDecl (Let name [] body) = "var " <> name <> " = " <> genExpr body <> "\n"
codegenDecl (Let name params body) = genFunc name params body
codegenDecl e = genExpr e <> "\n"

header :: T.Text
header = "package main\n\n"

-- Generate a Go top-level declaration from a foglang expression
codegen :: Expr -> T.Text
codegen (Let name [] body) = header <> "var " <> name <> " = " <> genExpr body <> "\n"
codegen (Let name params body) = header <> genFunc name params body
codegen e = header <> genExpr e <> "\n"

codegenGoFile :: GoFile -> T.Text
codegenGoFile (GoFile hdr exprs) = codegenHeader hdr <> T.concat (map codegenDecl exprs)
