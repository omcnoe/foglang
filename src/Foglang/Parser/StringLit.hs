module Foglang.Parser.StringLit (stringLit) where

import Foglang.AST (StringLit (..))
import Foglang.Parser (Parser, lexeme)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (char)

-- TODO: improve conformance with Go spec (escape sequences, raw string literals, unicode)
stringLit :: Parser StringLit
stringLit = lexeme $ do
  _ <- char '"'
  contents <- takeWhileP Nothing (/= '"')
  _ <- char '"'
  return (StringLit contents)
