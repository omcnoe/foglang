module Foglang.Parser.Header (header) where

import Data.Char (isPrint, isSpace)
import Data.Text qualified as T
import Foglang.AST (Header (..), Ident (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser (Parser, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (between, many, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char)

import' :: Parser ImportDecl
import' = do
  alias <-
    (Dot <$ symbol ".")
      <|> try (toAlias <$> lexeme ident)
      <|> return Default
  path <- lexeme $ do
    _ <- char '"'
    path <- takeWhileP Nothing (/= '"')
    _ <- char '"'
    validateImportPath path
  return $ ImportDecl alias path
  where
    toAlias (Ident "_") = Blank
    toAlias i = Alias i

    validateImportPath :: T.Text -> Parser T.Text
    validateImportPath path
      | T.null path = fail "empty import path"
      | Just c <- T.find (\c -> not (isPrint c) || isSpace c || c `elem` illegalChars) path =
          fail $ "illegal character " ++ show c ++ " in import path"
      | otherwise = return path
      where
        illegalChars :: [Char]
        illegalChars = "!\"#$%&'()*,:;<=>?[\\]^`{|}\xFFFD"

groupedImports :: Parser [ImportDecl]
groupedImports = between (symbol "(") (symbol ")") (many import')

importDecl :: Parser [ImportDecl]
importDecl = do
  _ <- keyword "import"
  (: []) <$> import' <|> groupedImports

packageClause :: Parser PackageClause
packageClause = do
  _ <- keyword "package"
  PackageClause <$> lexeme ident

header :: Parser Header
header = do
  pkg <- packageClause
  imports <- concat <$> many importDecl
  return $ Header pkg imports
