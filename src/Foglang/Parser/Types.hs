module Foglang.Parser.Types (params, typeExpr) where

import Foglang.AST (Ident (..), Param (..), TypeExpr (..))
import Foglang.Parser (Parser, SC, scn)
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
          ty <- typeExpr
          _ <- L.symbol sc' ")"
          return $ case ty of
            SliceType innerTy -> VariadicParam name innerTy
            _ -> TypedParam name ty
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

-- Type expression: a named type, a variadic type (...T), struct{}, () (unit), or a parenthesised function type.
typeExpr :: Parser TypeExpr
typeExpr =
  try (SliceType <$> (string "..." *> typeExpr))
    <|> try (NamedType (Ident "struct{}") <$ L.lexeme scn (string "struct{}"))
    <|> try (NamedType (Ident "()") <$ L.lexeme scn (string "()"))
    <|> (NamedType <$> L.lexeme scn ident)
    <|> funcTypeExpr

-- Parenthesised function type: (T1 -> T2 -> ... => Tr)
-- or variadic:                 (T1 -> ...Tv => Tr) where ...Tv is SliceType Tv.
-- or zero-param:               (() => Tr)
funcTypeExpr :: Parser TypeExpr
funcTypeExpr = do
  _ <- L.symbol scn "("
  -- Check for zero-param form: after consuming "(", see if next is "() =>"
  mZero <- optional (try (L.symbol scn "()" >> L.symbol scn "=>"))
  case mZero of
    Just _ -> do
      retTy <- typeExpr
      _ <- L.symbol scn ")"
      return $ FuncType [] Nothing retTy
    Nothing -> do
      tys <- many (try (typeExpr <* optional (L.symbol scn "->")))
      case tys of
        [] -> fail "function type with no parameters"
        _ -> do
          _ <- L.symbol scn "=>"
          retTy <- typeExpr
          _ <- L.symbol scn ")"
          let (fixedTys, mVarTy) = case reverse tys of
                (SliceType t : rest) -> (reverse rest, Just t)
                _ -> (tys, Nothing)
          return $ FuncType fixedTys mVarTy retTy
