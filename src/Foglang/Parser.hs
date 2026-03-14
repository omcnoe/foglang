module Foglang.Parser (Parser, isLetter, keyword, lexeme, symbol, digitSeq, binaryDigits, octalDigits, decimalDigits, hexDigits) where

import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Char qualified as Data.Char
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, chunk, notFollowedBy, satisfy, takeWhile1P)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

-- go's `letter` includes underscore, so we wrap Data.Char.isLetter
isLetter :: Char -> Bool
isLetter c = Data.Char.isLetter c || c == '_'

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

keyword :: T.Text -> Parser T.Text
keyword kw = lexeme $ do
  t <- chunk kw
  notFollowedBy (satisfy (\c -> isLetter c || isDigit c)) -- disambiguate from identifiers that may have keyword as prefix
  return t

{-
decimal_digits = decimal_digit { [ "_" ] decimal_digit } .
binary_digits  = binary_digit { [ "_" ] binary_digit } .
octal_digits   = octal_digit { [ "_" ] octal_digit } .
hex_digits     = hex_digit { [ "_" ] hex_digit } .
-}
digitSeq :: (Char -> Bool) -> Parser T.Text
digitSeq isD = do
  t <- takeWhile1P Nothing (\c -> isD c || c == '_')
  if isD (T.head t) && not ('_' == T.last t) && not ("__" `T.isInfixOf` t)
    then return t
    else fail "'_' must separate successive digits"

decimalDigits :: Parser T.Text
decimalDigits = digitSeq isDigit

binaryDigits :: Parser T.Text
binaryDigits = digitSeq (\c -> c == '0' || c == '1')

octalDigits :: Parser T.Text
octalDigits = digitSeq isOctDigit

hexDigits :: Parser T.Text
hexDigits = digitSeq isHexDigit
