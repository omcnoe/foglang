module Foglang.Parser.Patterns (pattern') where

import Foglang.AST (Pattern (..))
import Foglang.Parser (Parser, SC, keyword)
import Foglang.Parser.Ident (ident)
import Foglang.Parser.IntLit (intLit)
import Text.Megaparsec (sepBy1, try, (<|>))
import Text.Megaparsec.Char.Lexer qualified as L (lexeme, symbol)

pattern' :: SC -> Parser Pattern
pattern' sc' =
  try (consPattern sc')
    <|> try (tuplePattern sc')
    <|> atomicPattern sc'

consPattern :: SC -> Parser Pattern
consPattern sc' = do
  hd <- try (tuplePattern sc') <|> atomicPattern sc'
  _ <- L.symbol sc' "::"
  tl <- pattern' sc'
  return $ PtCons hd tl

atomicPattern :: SC -> Parser Pattern
atomicPattern sc' =
  try (PtSliceEmpty <$ L.symbol sc' "[]")
    <|> (PtBoolLit True <$ L.lexeme sc' (keyword "true"))
    <|> (PtBoolLit False <$ L.lexeme sc' (keyword "false"))
    <|> try (PtIntLit <$> L.lexeme sc' intLit)
    <|> try (PtWildcard <$ L.lexeme sc' (keyword "_"))
    <|> (PtVar <$> L.lexeme sc' ident)

tuplePattern :: SC -> Parser Pattern
tuplePattern sc' = do
  _ <- L.symbol sc' "("
  first <- pattern' sc'
  _ <- L.symbol sc' ","
  rest <- sepBy1 (pattern' sc') (L.symbol sc' ",")
  _ <- L.symbol sc' ")"
  return $ PtTuple (first : rest)
