module Foglang.Parser.Header (header) where

import Foglang.AST (Header (..), Ident (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser (Parser, keyword, scn)
import Text.Megaparsec.Char.Lexer qualified as L
import Foglang.Parser.Ident (headerIdent)
import Text.Megaparsec (between, many, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char)

import' :: Parser ImportDecl
import' = do
  alias <-
    (Dot <$ L.symbol scn ".")
      <|> (toAlias <$> headerIdent)
      <|> return None
  path <- L.lexeme scn $ do
    _ <- char '"'
    path <- takeWhile1P Nothing (/= '"')
    _ <- char '"'
    return path
  return $ ImportDecl alias path
  where
    toAlias (Ident "_") = Blank
    toAlias i = Alias i

groupedImports :: Parser [ImportDecl]
groupedImports = between (L.symbol scn "(") (L.symbol scn ")") (many import')

importDecl :: Parser [ImportDecl]
importDecl = do
  _ <- L.lexeme scn (keyword "import")
  (: []) <$> import' <|> groupedImports

packageClause :: Parser PackageClause
packageClause = do
  _ <- L.lexeme scn (keyword "package")
  PackageClause <$> headerIdent

header :: Parser Header
header = do
  pkg <- packageClause
  imports <- concat <$> many importDecl
  return $ Header pkg imports
