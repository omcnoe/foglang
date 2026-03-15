module Foglang.Parser.Ident (identRaw, ident, qualIdent, headerIdent) where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Foglang.AST (Ident (..))
import Foglang.Parser (Parser, isGoLetter, lexeme, symbol)
import Text.Megaparsec (satisfy, sepBy1, takeWhileP, try, (<|>))
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
      "then"
    ]

-- identifier = letter { letter | unicode_digit } .
-- Raw, no qualification or unit handling. No trailing whitespace consumed.
identRaw :: Parser Ident
identRaw = try $ do
  c <- satisfy isGoLetter
  cs <- takeWhileP Nothing (\ch -> isGoLetter ch || isDigit ch)
  let raw = c `T.cons` cs
  if raw `Set.member` reserved
    then fail $ T.unpack $ "reserved keyword: " <> raw
    else return (Ident raw)

-- Identifier, or unit "()"
ident :: Parser Ident
ident = (Ident "()" <$ symbol "()") <|> (lexeme identRaw)

-- Dot-qualified identifier, e.g. "fmt.Println". No spaces around dots
qualIdent :: Parser Ident
qualIdent = lexeme $ try $ do
  parts <- identRaw `sepBy1` char '.'
  return $ Ident $ T.intercalate "." $ map (\(Ident t) -> t) parts

-- Plain identifier for use in file headers (package names, import aliases). No unit "()" handling
headerIdent :: Parser Ident
headerIdent = lexeme identRaw
