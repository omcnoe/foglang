module Foglang.Parser.Patterns (pattern') where

import Foglang.AST (Pattern (..))
import Foglang.Parser (Parser, SC, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Foglang.Parser.IntLit (intLit)
import Text.Megaparsec (sepBy1, try, (<|>))

pattern' :: SC -> Parser Pattern
pattern' sc' =
  try (consPattern sc')
    <|> try (tuplePattern sc')
    <|> atomicPattern sc'

consPattern :: SC -> Parser Pattern
consPattern sc' = do
  hd <- try (tuplePattern sc') <|> atomicPattern sc'
  _ <- symbol sc' "::"
  tl <- pattern' sc'
  return $ PtCons hd tl

atomicPattern :: SC -> Parser Pattern
atomicPattern sc' =
  try (PtSliceEmpty <$ symbol sc' "[]")
    <|> (PtBoolLit True <$ lexeme sc' (keyword "true"))
    <|> (PtBoolLit False <$ lexeme sc' (keyword "false"))
    <|> try (PtIntLit <$> lexeme sc' intLit)
    <|> try (PtWildcard <$ lexeme sc' (keyword "_"))
    <|> (PtVar <$> lexeme sc' ident)

tuplePattern :: SC -> Parser Pattern
tuplePattern sc' = do
  _ <- symbol sc' "("
  first <- pattern' sc'
  _ <- symbol sc' ","
  rest <- sepBy1 (pattern' sc') (symbol sc' ",")
  _ <- symbol sc' ")"
  return $ PtTuple (first : rest)
