{-# LANGUAGE OverloadedStrings #-}

module Foglang.IntLit (IntLit (..), intLit) where

import Data.Char (isDigit, isHexDigit, isOctDigit)
import qualified Data.Text as T
import Foglang.Core (Parser)
import Text.Megaparsec (option, satisfy, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (string, string')

{-
int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .

decimal_digits = decimal_digit { [ "_" ] decimal_digit } .
binary_digits  = binary_digit { [ "_" ] binary_digit } .
octal_digits   = octal_digit { [ "_" ] octal_digit } .
hex_digits     = hex_digit { [ "_" ] hex_digit } .
-}

data IntLit = Decimal T.Text | Binary T.Text | Octal T.Text | Hex T.Text
  deriving (Eq, Show)

isBinDigit :: Char -> Bool
isBinDigit c = c == '0' || c == '1'

-- Parse: digit { [ "_" ] digit } given a digit predicate
digitSeq :: (Char -> Bool) -> Parser T.Text
digitSeq isD = do
  t <- takeWhile1P Nothing (\c -> isD c || c == '_')
  if isD (T.head t) && not ('_' == T.last t) && not ("__" `T.isInfixOf` t)
    then return t
    else fail "'_' must separate successive digits"

intLit :: Parser IntLit
intLit = try binaryLit <|> try octalLit <|> try hexLit <|> decimalLit

binaryLit :: Parser IntLit
binaryLit = do
  prefix <- string' "0b"
  sep <- option "" (string "_")
  digits <- digitSeq isBinDigit
  return $ Binary $ prefix <> sep <> digits

octalLit :: Parser IntLit
octalLit = do
  prefix <- (try $ string' "0o") <|> string "0"
  sep <- option "" (string "_")
  digits <- digitSeq isOctDigit
  return $ Octal $ prefix <> sep <> digits

hexLit :: Parser IntLit
hexLit = do
  prefix <- string' "0x"
  sep <- option "" (string "_")
  digits <- digitSeq isHexDigit
  return $ Hex $ prefix <> sep <> digits

decimalLit :: Parser IntLit
decimalLit =
  try parseZero <|> parseDigits
  where
    parseZero = Decimal <$> string "0"
    parseDigits = do
      first <- satisfy (\c -> c >= '1' && c <= '9')
      rest <- option "" $ do
        sep <- option "" (string "_")
        digits <- digitSeq isDigit
        return $ sep <> digits
      return $ Decimal $ T.cons first rest
