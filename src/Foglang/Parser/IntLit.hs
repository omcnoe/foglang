module Foglang.Parser.IntLit (intLit) where

import Data.Text qualified as T
import Foglang.AST (IntLit (..))
import Foglang.Parser (Parser, binaryDigits, decimalDigits, hexDigits, lexeme, octalDigits)
import Text.Megaparsec (option, satisfy, try, (<|>))
import Text.Megaparsec.Char (string, string')

{-
int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .
-}

decimalLit :: Parser IntLit
decimalLit =
  parseZero <|> parseDigits
  where
    parseZero = Decimal <$> string "0"
    parseDigits = do
      first <- satisfy (\c -> c >= '1' && c <= '9')
      rest <- option "" $ do
        sep <- option "" (string "_")
        digits <- decimalDigits
        return $ sep <> digits
      return $ Decimal $ T.cons first rest

binaryLit :: Parser IntLit
binaryLit = do
  prefix <- string' "0b"
  sep <- option "" (string "_")
  digits <- binaryDigits
  return $ Binary $ prefix <> sep <> digits

octalLit :: Parser IntLit
octalLit = do
  prefix <- string' "0o" <|> string "0"
  sep <- option "" (string "_")
  digits <- octalDigits
  return $ Octal $ prefix <> sep <> digits

hexLit :: Parser IntLit
hexLit = do
  prefix <- string' "0x"
  sep <- option "" (string "_")
  digits <- hexDigits
  return $ Hex $ prefix <> sep <> digits

intLit :: Parser IntLit
intLit = lexeme $ try binaryLit <|> try octalLit <|> try hexLit <|> decimalLit
