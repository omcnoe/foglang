module Foglang.Parser.Digits (decimalDigits, binaryDigits, octalDigits, hexDigits) where

import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Text qualified as T
import Foglang.Parser (Parser)
import Text.Megaparsec (takeWhile1P)

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
