{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Foglang.Core (Parser, isLetter, lexeme, symbol) where

import Data.Char qualified as Data.Char
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)
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
