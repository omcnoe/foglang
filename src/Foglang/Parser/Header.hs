module Foglang.Parser.Header (header) where

import Foglang.AST (Header (..), ImportDecl (..), PackageDecl (..))
import Foglang.Parser (Parser, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (between, many, optional, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char)

import' :: Parser ImportDecl
import' = do
  alias <- optional (symbol "." <|> ident)
  path <- lexeme $ do
    _ <- char '"'
    path <- takeWhile1P Nothing (/= '"')
    _ <- char '"'
    return path
  return $ ImportDecl alias path

groupedImports :: Parser [ImportDecl]
groupedImports = between (symbol "(") (symbol ")") (many import')

importDecl :: Parser [ImportDecl]
importDecl = do
  _ <- keyword "import"
  (: []) <$> import' <|> groupedImports

packageDecl :: Parser PackageDecl
packageDecl = do
  _ <- keyword "package"
  PackageDecl <$> ident

header :: Parser Header
header = do
  pkg <- packageDecl
  imports <- concat <$> many importDecl
  return $ Header pkg imports
