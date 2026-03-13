{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Foglang.Ident (Ident, ident) where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Foglang.Core (Parser, isLetter, lexeme)
import Text.Megaparsec (satisfy, takeWhileP)

-- identifier = letter { letter | unicode_digit } .

type Ident = T.Text

reserved :: Set.Set T.Text
reserved =
  Set.fromList
    [ -- golang
      "break",
      "default",
      "func",
      "interface",
      "select",
      "case",
      "defer",
      "go",
      "map",
      "struct",
      "chan",
      "else",
      "goto",
      "package",
      "switch",
      "const",
      "fallthrough",
      "if",
      "range",
      "type",
      "continue",
      "for",
      "import",
      "return",
      "var",
      -- foglang
      "let"
    ]

ident :: Parser Ident
ident = lexeme $ do
  c <- satisfy isLetter
  cs <- takeWhileP Nothing (\ch -> isLetter ch || isDigit ch)
  let identText = T.cons c cs
  if identText `Set.member` reserved
    then fail $ "reserved keyword: " ++ T.unpack identText
    else return identText
