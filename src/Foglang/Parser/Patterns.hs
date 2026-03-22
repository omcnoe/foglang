module Foglang.Parser.Patterns (pattern') where

import Foglang.AST (Pattern (..))
import Foglang.Parser (Parser, keyword, scn)
import Foglang.Parser.Ident (ident)
import Foglang.Parser.IntLit (intLit)
import Text.Megaparsec (sepBy1, try, (<|>))
import Text.Megaparsec.Char.Lexer qualified as L (lexeme, symbol)

pattern' :: Parser Pattern
pattern' =
  try consPattern
    <|> try tuplePattern
    <|> atomicPattern

consPattern :: Parser Pattern
consPattern = do
  hd <- try tuplePattern <|> atomicPattern
  _ <- L.symbol scn "::"
  tl <- pattern'
  return $ PCons hd tl

atomicPattern :: Parser Pattern
atomicPattern =
  try (PSliceEmpty <$ L.symbol scn "[]")
    <|> (PBoolLit True <$ L.lexeme scn (keyword "true"))
    <|> (PBoolLit False <$ L.lexeme scn (keyword "false"))
    <|> try (PIntLit <$> L.lexeme scn intLit)
    <|> try (PWildcard <$ L.lexeme scn (keyword "_"))
    <|> (PVar <$> L.lexeme scn ident)

tuplePattern :: Parser Pattern
tuplePattern = do
  _ <- L.symbol scn "("
  first <- pattern'
  _ <- L.symbol scn ","
  rest <- sepBy1 pattern' (L.symbol scn ",")
  _ <- L.symbol scn ")"
  return $ PTuple (first : rest)
