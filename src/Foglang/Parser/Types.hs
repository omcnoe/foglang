module Foglang.Parser.Types (params, typeExpr) where

import Foglang.AST (Ident (..), Param (..), TypeExpr (..), pattern UnitType)
import Foglang.Parser (Parser, SC, freshTVar, keyword)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (many, optional, try, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L (lexeme, symbol)

-- Parse a function parameter:
--   () for unit,
--   (name : type) / (name : ...type) for typed/variadic,
--   (...) for redundant parens (recursive),
--   name for bare untyped (gets a fresh TVar).
param :: SC -> Parser Param
param sc' = unit <|> try typed <|> try parens <|> bare
  where
    unit = PUnit <$ L.symbol sc' "()"
    typed = do
      _ <- L.symbol sc' "("
      name <- L.lexeme sc' ident
      _ <- L.symbol sc' ":"
      mVariadic <- optional (try (string "..."))
      ty <- typeExpr sc'
      _ <- L.symbol sc' ")"
      return $ case mVariadic of
        Just _ -> PVariadic name ty
        Nothing -> PTyped name ty
    parens = do
      _ <- L.symbol sc' "("
      p <- param sc'
      _ <- L.symbol sc' ")"
      return p
    bare = do
      name <- L.lexeme sc' ident
      t <- freshTVar
      return $ PTyped name t

-- Parse a sequence of function parameters (zero or more), with optional '->'
-- separators between consecutive parameters. sc' controls whitespace/newline
-- consumption between tokens (use scn at block level, the linefold sc' inline).
params :: SC -> Parser [Param]
params sc' = do
  firstParam <- optional (param sc')
  restParams <- case firstParam of
    Nothing -> return []
    Just _ -> many $ optional (L.symbol sc' "->") *> param sc'
  return $ maybe [] (: restParams) firstParam

-- Type expression parameterised on a space consumer. sc' controls how far
-- whitespace is consumed between tokens — use scn at block level, the linefold
-- sc' inline. This ensures type expressions respect line fold indentation.
typeExpr :: SC -> Parser TypeExpr
typeExpr sc' =
  try (mapTypeExpr sc')
    <|> try (sliceTypeExpr sc')
    <|> try (TNamed (Ident "struct{}") <$ L.lexeme sc' (string "struct{}"))
    <|> try (UnitType <$ L.lexeme sc' (string "()"))
    <|> try (TNamed <$> L.lexeme sc' ident)
    <|> funcTypeExpr sc'

-- map[K]V type — spaces allowed between tokens, respecting the space consumer.
mapTypeExpr :: SC -> Parser TypeExpr
mapTypeExpr sc' = do
  _ <- L.lexeme sc' (keyword "map")
  _ <- L.symbol sc' "["
  keyTy <- typeExpr sc'
  _ <- L.symbol sc' "]"
  valTy <- typeExpr sc'
  return $ TMap keyTy valTy

-- []T slice type (distinct from ...T variadic).
sliceTypeExpr :: SC -> Parser TypeExpr
sliceTypeExpr sc' = do
  _ <- L.symbol sc' "["
  _ <- L.symbol sc' "]"
  TSlice <$> typeExpr sc'

-- Parenthesised function type: (T1 -> T2 => Tr)
-- or variadic:                 (T1 -> ...Tv => Tr)
-- or zero-param:               (() => Tr)
funcTypeExpr :: SC -> Parser TypeExpr
funcTypeExpr sc' = do
  _ <- L.symbol sc' "("
  -- Check for zero-param form: after consuming "(", see if next is "() =>"
  mZero <- optional (try (L.symbol sc' "()" >> L.symbol sc' "=>"))
  case mZero of
    Just _ -> do
      retTy <- typeExpr sc'
      _ <- L.symbol sc' ")"
      return $ TFunc [] Nothing retTy
    Nothing -> do
      -- Parse fixed params, then check for variadic ...T before =>
      fixedTys <- many (try (typeExpr sc' <* optional (L.symbol sc' "->")))
      mVarTy <- optional (try (string "..." *> typeExpr sc'))
      case (fixedTys, mVarTy) of
        ([], Nothing) -> fail "function type with no parameters"
        _ -> do
          _ <- L.symbol sc' "=>"
          retTy <- typeExpr sc'
          _ <- L.symbol sc' ")"
          return $ TFunc fixedTys mVarTy retTy
