module Foglang.Parser.Header (header) where

import Foglang.AST (Header (..), Ident (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser (Parser, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (between, many, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char)

import' :: Parser ImportDecl
import' = do
  alias <-
    (Dot <$ symbol ".")
      <|> (toAlias <$> ident)
      <|> return None
  path <- lexeme $ do
    _ <- char '"'
    path <- takeWhile1P Nothing (/= '"')
    _ <- char '"'
    return path
  return $ ImportDecl alias path
  where
    toAlias (Ident "_") = Blank
    toAlias i = Alias i

groupedImports :: Parser [ImportDecl]
groupedImports = between (symbol "(") (symbol ")") (many import')

importDecl :: Parser [ImportDecl]
importDecl = do
  _ <- keyword "import"
  (: []) <$> import' <|> groupedImports

packageClause :: Parser PackageClause
packageClause = do
  _ <- keyword "package"
  PackageClause <$> ident

header :: Parser Header
header = do
  pkg <- packageClause
  imports <- concat <$> many importDecl
  return $ Header pkg imports
