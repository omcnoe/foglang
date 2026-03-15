module Foglang.Codegen (codegenGoFile) where

import Data.Text qualified as T
import Foglang.AST (Expr (..), FloatLit (..), FogFile (..), Header (..), Ident (..), ImportAlias (..), ImportDecl (..), IntLit (..), PackageClause (..), StringLit (..))

ind :: Int -> T.Text
ind n = T.replicate n "\t"

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

paramListText :: [Ident] -> T.Text
paramListText params = T.intercalate ", " [identText p <> " any" | p <- params, p /= Ident "()"]

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
genBody indent (Sequence exprs)
  | null exprs = ind indent <> "return ()\n"
  | otherwise =
      T.concat (map (genStmtBody indent) (init exprs))
        <> genBody indent (last exprs)
genBody indent (Let name [] rhs inExpr) =
  ind indent <> identText name <> " := " <> genExpr rhs <> "\n"
    <> genBody indent inExpr
genBody indent (Let name params rhs inExpr) =
  ind indent <> identText name <> " := func(" <> paramListText params <> ") any { return "
    <> genExpr rhs <> " }\n"
    <> genBody indent inExpr
genBody indent e = ind indent <> "return " <> genExpr e <> "\n"

genFunc :: Ident -> [Ident] -> Expr -> T.Text
genFunc name params body =
  "func "
    <> identText name
    <> "("
    <> paramListText params
    <> ") any {\n"
    <> genBody 1 body
    <> "}\n\n"

genMainFunc :: Expr -> T.Text
genMainFunc body =
  "func main() {\n"
    <> genStmtBody 1 body
    <> "}\n"

-- Like genBody but emits the expression as a plain statement (no 'return').
genStmtBody :: Int -> Expr -> T.Text
genStmtBody indent (If cond then' else') = genIfChain indent cond then' else'
genStmtBody indent (Sequence exprs) = T.concat (map (genStmtBody indent) exprs)
genStmtBody indent (Let name [] rhs inExpr) =
  ind indent <> identText name <> " := " <> genExpr rhs <> "\n"
    <> genStmtBody indent inExpr
genStmtBody indent (Let name params rhs inExpr) =
  ind indent <> identText name <> " := func(" <> paramListText params <> ") any { return "
    <> genExpr rhs <> " }\n"
    <> genStmtBody indent inExpr
genStmtBody indent e = ind indent <> genExpr e <> "\n"

-- Generate a Go expression. BinaryOp sub-expressions are parenthesised to
-- preserve foglang's precedences without relying on Go's
genExpr :: Expr -> T.Text
genExpr (Var i) = identText i
genExpr (IntLit lit) = intLitText lit
genExpr (FloatLit lit) = floatLitText lit
genExpr (StrLit (StringLit t)) = "\"" <> t <> "\""
genExpr (BinaryOp e1 op e2) = "(" <> genExpr e1 <> " " <> op <> " " <> genExpr e2 <> ")"
genExpr (Application f args) = genExpr f <> "(" <> T.intercalate ", " (map genExpr args) <> ")"
genExpr (If cond then' else') =
  "func() any { if " <> genExpr cond <> " { return " <> genExpr then' <> " }; return " <> genExpr else' <> " }()"
genExpr (Sequence []) = "()"
genExpr (Sequence exprs) =
  "func() any { "
    <> T.intercalate "; " (map genExpr (init exprs))
    <> "; return "
    <> genExpr (last exprs)
    <> " }()"
genExpr (Let name [] rhs inExpr) =
  "func() any { " <> identText name <> " := " <> genExpr rhs
    <> "; return " <> genExpr inExpr <> " }()"
genExpr (Let name params rhs inExpr) =
  "func() any { " <> identText name <> " := func(" <> paramListText params <> ") any { return "
    <> genExpr rhs <> " }; return " <> genExpr inExpr <> " }()"

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

codegenDecl :: Ident -> [Ident] -> Expr -> T.Text
codegenDecl (Ident "main") _ body = genMainFunc body
codegenDecl name [] (Sequence exprs) =
  "var " <> identText name <> " = func() any {\n"
    <> genBody 1 (Sequence exprs)
    <> "}()\n\n"
codegenDecl name [] body = "var " <> identText name <> " = " <> genExpr body <> "\n"
codegenDecl name params body = genFunc name params body

-- True for expressions that may be valid Go statements (have child Exprs that
-- could carry side effects). These are collected into an init() function.
isStmtExpr :: Expr -> Bool
isStmtExpr (Application _ _) = True
isStmtExpr (If _ _ _) = True
isStmtExpr (Sequence _) = True
isStmtExpr (BinaryOp _ _ _) = True
isStmtExpr _ = False

-- Walk the top-level body in source order, emitting declarations and
-- inline init() functions for bare statement expressions.
codegenTopLevel :: Expr -> T.Text
codegenTopLevel (Let n p r i) = codegenDecl n p r <> codegenTopLevel i
codegenTopLevel (Sequence exprs) = T.concat (map codegenTopLevel exprs)
codegenTopLevel e
  | isStmtExpr e = "func init() {\n" <> genStmtBody 1 e <> "}\n"
  | otherwise = ""

codegenGoFile :: FogFile -> T.Text
codegenGoFile (FogFile hdr body) = codegenHeader hdr <> codegenTopLevel body
