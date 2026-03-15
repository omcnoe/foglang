module Foglang.Parser (isGoLetter, Parser, SC, scn, lexeme, symbol, keyword) where

import Data.Char (isDigit, isLetter)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, notFollowedBy, satisfy, try)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

-- go's `letter` includes underscore, so we wrap Data.Char.isLetter
isGoLetter :: Char -> Bool
isGoLetter c = isLetter c || c == '_'

type Parser = Parsec Void T.Text

-- A space consumer is just a Parser ()
type SC = Parser ()

scn :: SC
scn =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme parser = L.lexeme scn parser

symbol :: T.Text -> Parser T.Text
symbol s = L.symbol scn s

keyword :: T.Text -> Parser T.Text
keyword w = try $ do
  t <- string w
  notFollowedBy (satisfy (\c -> isGoLetter c || isDigit c)) -- disambiguate from a possible Ident prefix
  return t
