module Foglang.Parser.FloatLit (floatLit) where

import Data.Text qualified as T
import Foglang.AST (FloatLit (..))
import Foglang.Parser (Parser)
import Foglang.Parser.Digits (decimalDigits, hexDigits)
import Text.Megaparsec (option, satisfy, try, (<|>))
import Text.Megaparsec.Char (string, string')

{-
float_lit         = decimal_float_lit | hex_float_lit .

decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
                    decimal_digits decimal_exponent |
                    "." decimal_digits [ decimal_exponent ] .
decimal_exponent  = ( "e" | "E" ) [ "+" | "-" ] decimal_digits .

hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
hex_mantissa      = [ "_" ] hex_digits "." [ hex_digits ] |
                    [ "_" ] hex_digits |
                    "." hex_digits .
hex_exponent      = ( "p" | "P" ) [ "+" | "-" ] decimal_digits .
-}

decimalFloatLit :: Parser FloatLit
decimalFloatLit = try withDot <|> try digitsExp <|> dotFirst
  where
    decimalExponent = do
      e <- satisfy (\c -> c == 'e' || c == 'E')
      sign <- option "" (string "+" <|> string "-")
      digits <- decimalDigits
      return $ T.cons e (sign <> digits)

    withDot = do
      int <- decimalDigits
      _ <- string "."
      frac <- option "" decimalDigits
      exp' <- option "" decimalExponent
      return $ FloatDecimal $ int <> "." <> frac <> exp'

    digitsExp = do
      int <- decimalDigits
      exp' <- decimalExponent
      return $ FloatDecimal $ int <> exp'

    dotFirst = do
      _ <- string "."
      frac <- decimalDigits
      exp' <- option "" decimalExponent
      return $ FloatDecimal $ "." <> frac <> exp'

hexFloatLit :: Parser FloatLit
hexFloatLit = do
  prefix <- string' "0x"
  mantissa <- try withDot <|> withDigits <|> dotFirst
  exp' <- hexExponent
  return $ FloatHex $ prefix <> mantissa <> exp'
  where
    hexExponent = do
      p <- satisfy (\c -> c == 'p' || c == 'P')
      sign <- option "" (string "+" <|> string "-")
      digits <- decimalDigits
      return $ T.cons p (sign <> digits)

    withDot = do
      sep <- option "" (string "_")
      digits <- hexDigits
      _ <- string "."
      frac <- option "" hexDigits
      return $ sep <> digits <> "." <> frac

    withDigits = do
      sep <- option "" (string "_")
      digits <- hexDigits
      return $ sep <> digits

    dotFirst = do
      _ <- string "."
      digits <- hexDigits
      return $ "." <> digits

floatLit :: Parser FloatLit
floatLit = try hexFloatLit <|> decimalFloatLit
