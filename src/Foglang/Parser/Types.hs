module Foglang.Parser.Types (params, typeExpr) where

import Foglang.AST (Ident (..), Param (..), TypeExpr (..))
import Foglang.Parser (Parser, SC, keyword, scn)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (many, optional, try, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L (lexeme, symbol)

-- Parse a function parameter: () for unit, (name : type), or (name : ...type).
param :: SC -> Parser Param
param sc' =
  (UnitParam <$ L.symbol sc' "()")
    <|> try
      ( do
          _ <- L.symbol sc' "("
          name <- L.lexeme sc' ident
          _ <- L.symbol sc' ":"
          -- Check for explicit ...T variadic syntax (distinct from []T slice type)
          mVariadic <- optional (try (string "..."))
          ty <- typeExpr sc'
          _ <- L.symbol sc' ")"
          return $ case mVariadic of
            Just _ -> VariadicParam name ty
            Nothing -> TypedParam name ty
      )

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
  try (SliceType <$> (string "..." *> typeExpr sc'))
    <|> try (mapTypeExpr sc')
    <|> try (sliceTypeExpr sc')
    <|> try (NamedType (Ident "struct{}") <$ L.lexeme sc' (string "struct{}"))
    <|> try (NamedType (Ident "()") <$ L.lexeme sc' (string "()"))
    <|> (NamedType <$> L.lexeme sc' ident)
    <|> funcTypeExpr sc'

-- map[K]V type — spaces allowed between tokens, respecting the space consumer.
mapTypeExpr :: SC -> Parser TypeExpr
mapTypeExpr sc' = do
  _ <- L.lexeme sc' (keyword "map")
  _ <- L.symbol sc' "["
  keyTy <- typeExpr sc'
  _ <- L.symbol sc' "]"
  valTy <- typeExpr sc'
  return $ MapType keyTy valTy

-- []T slice type (distinct from ...T variadic).
sliceTypeExpr :: SC -> Parser TypeExpr
sliceTypeExpr sc' = do
  _ <- L.symbol sc' "["
  _ <- L.symbol sc' "]"
  SliceType <$> typeExpr sc'

-- Parenthesised function type: (T1 -> T2 -> ... => Tr)
-- or variadic:                 (T1 -> ...Tv => Tr) where ...Tv is SliceType Tv.
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
      return $ FuncType [] Nothing retTy
    Nothing -> do
      tys <- many (try (typeExpr sc' <* optional (L.symbol sc' "->")))
      case tys of
        [] -> fail "function type with no parameters"
        _ -> do
          _ <- L.symbol sc' "=>"
          retTy <- typeExpr sc'
          _ <- L.symbol sc' ")"
          let (fixedTys, mVarTy) = case reverse tys of
                (SliceType t : rest) -> (reverse rest, Just t)
                _ -> (tys, Nothing)
          return $ FuncType fixedTys mVarTy retTy
