module Foglang.Parser.Ident (ident, qualIdent, headerIdent) where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Foglang.AST (Ident (..))
import Foglang.Parser (Parser, isGoLetter, scn)
import Text.Megaparsec (satisfy, sepBy1, takeWhileP, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

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

-- identifier = letter { letter | unicode_digit } .
-- No trailing whitespace consumed.
ident :: Parser Ident
ident = try $ do
  c <- satisfy isGoLetter
  cs <- takeWhileP Nothing (\ch -> isGoLetter ch || isDigit ch)
  let raw = c `T.cons` cs
  if raw `Set.member` reserved
    then fail $ T.unpack $ "reserved keyword: " <> raw
    else return (Ident raw)

-- Dot-qualified identifier, e.g. "fmt.Println". No spaces around dots.
-- No trailing whitespace consumed.
qualIdent :: Parser Ident
qualIdent = try $ do
  parts <- ident `sepBy1` char '.'
  return $ Ident $ T.intercalate "." $ map (\(Ident t) -> t) parts

-- Plain identifier for use in file headers (package names, import aliases).
headerIdent :: Parser Ident
headerIdent = L.lexeme scn ident
