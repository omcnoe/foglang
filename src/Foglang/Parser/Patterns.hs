module Foglang.Parser.Patterns (pattern') where

import Foglang.AST (Pattern (..))
import Foglang.Parser (Parser, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Text.Megaparsec (sepBy1, try, (<|>))

pattern' :: Parser Pattern
pattern' =
  try consPattern
    <|> try tuplePattern
    <|> atomicPattern

consPattern :: Parser Pattern
consPattern = do
  hd <- try tuplePattern <|> atomicPattern
  _ <- symbol "::"
  tl <- pattern'
  return $ PtCons hd tl

atomicPattern :: Parser Pattern
atomicPattern =
  try (PtSliceEmpty <$ symbol "[]")
    <|> (PtBoolLit True <$ keyword "true")
    <|> (PtBoolLit False <$ keyword "false")
    <|> try (PtIntLit <$> lexeme intLit)
    <|> try (PtStrLit <$> lexeme stringLit)
    <|> try (PtWildcard <$ keyword "_")
    <|> (PtVar <$> lexeme ident)

tuplePattern :: Parser Pattern
tuplePattern = do
  _ <- symbol "("
  first <- pattern'
  _ <- symbol ","
  rest <- sepBy1 pattern' (symbol ",")
  _ <- symbol ")"
  return $ PtTuple (first : rest)
