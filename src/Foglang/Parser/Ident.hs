module Foglang.Parser.Ident (ident, qualIdent) where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Foglang.AST (Ident (..), QualIdent (..))
import Foglang.Parser (Parser, isLetter, lexeme, symbol)
import Text.Megaparsec (satisfy, sepBy1, takeWhileP, try)

-- identifier = letter { letter | unicode_digit } .

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
      "let",
      "then"
    ]

ident :: Parser Ident
ident = lexeme $ try $ do
  c <- satisfy isLetter
  cs <- takeWhileP Nothing (\ch -> isLetter ch || isDigit ch)
  let raw = c `T.cons` cs
  if raw `Set.member` reserved
    then fail $ T.unpack $ "reserved keyword: " <> raw
    else return (Ident raw)

qualIdent :: Parser QualIdent
qualIdent = lexeme $ try $ do
  parts <- ident `sepBy1` symbol "."
  return (QualIdent parts)
