module Foglang.Parser.Ident (ident, qualIdent) where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Foglang.AST (Ident (..))
import Foglang.Parser (Parser, isGoLetter)
import Text.Megaparsec (many, satisfy, takeWhileP, try)
import Text.Megaparsec.Char (char)

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
      "then",
      "match",
      "with"
    ]

-- identifier = letter { letter | unicode_digit } .
-- No trailing whitespace consumed. No try — callers must handle backtracking
-- explicitly (via try) when ident is one of several alternatives.
ident :: Parser Ident
ident = do
  c <- satisfy isGoLetter
  cs <- takeWhileP Nothing (\ch -> isGoLetter ch || isDigit ch)
  let raw = c `T.cons` cs
  if raw `Set.member` reserved
    then fail $ T.unpack $ "reserved keyword: " <> raw
    else return (Ident raw)

-- Dot-qualified identifier, e.g. "fmt.Println". No spaces around dots.
-- Uses try on each dot-separator so it doesn't consume '.' then fail on postfix spread operator: "args...".
-- No trailing whitespace consumed.
qualIdent :: Parser Ident
qualIdent = try $ do
  first <- ident
  rest <- many (try (char '.' *> ident))
  return $ Ident $ T.intercalate "." $ map (\(Ident t) -> t) (first : rest)
