module Foglang.Parser.Types (params, typeExpr) where

import Foglang.AST (Ident (..), Param (..), TypeExpr (..), pattern UnitType)
import Foglang.Parser (Parser, freshTVar, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (many, optional, try, (<|>))
import Text.Megaparsec.Char (string)

param :: Parser Param
param = unit <|> try typed <|> try parens <|> bare
  where
    unit = PUnit <$ symbol "()"
    typed = do
      _ <- symbol "("
      name <- lexeme ident
      _ <- symbol ":"
      mVariadic <- optional (try (string "..."))
      ty <- typeExpr
      _ <- symbol ")"
      return $ case mVariadic of
        Just _ -> PVariadic name ty
        Nothing -> PTyped name ty
    parens = do
      _ <- symbol "("
      p <- param
      _ <- symbol ")"
      return p
    bare = do
      name <- lexeme ident
      t <- freshTVar
      return $ PTyped name t

params :: Parser [Param]
params = do
  firstParam <- optional param
  restParams <- case firstParam of
    Nothing -> return []
    Just _ -> many $ optional (symbol "->") *> param
  return $ maybe [] (: restParams) firstParam

typeExpr :: Parser TypeExpr
typeExpr =
  try mapTypeExpr
    <|> try sliceTypeExpr
    <|> try (TNamed (Ident "struct{}") <$ lexeme (string "struct{}"))
    <|> try (UnitType <$ lexeme (string "()"))
    <|> try (TNamed <$> lexeme ident)
    <|> funcTypeExpr

mapTypeExpr :: Parser TypeExpr
mapTypeExpr = do
  _ <- keyword "map"
  _ <- symbol "["
  keyTy <- typeExpr
  _ <- symbol "]"
  valTy <- typeExpr
  return $ TMap keyTy valTy

sliceTypeExpr :: Parser TypeExpr
sliceTypeExpr = do
  _ <- symbol "["
  _ <- symbol "]"
  TSlice <$> typeExpr

funcTypeExpr :: Parser TypeExpr
funcTypeExpr = do
  _ <- symbol "("
  mZero <- optional (try (symbol "()" >> symbol "=>"))
  case mZero of
    Just _ -> do
      retTy <- typeExpr
      _ <- symbol ")"
      return $ TFunc [UnitType] Nothing retTy
    Nothing -> do
      fixedTys <- many (try (typeExpr <* optional (symbol "->")))
      mVarTy <- optional (try (string "..." *> typeExpr))
      case (fixedTys, mVarTy) of
        ([], Nothing) -> fail "function type with no parameters"
        _ -> do
          _ <- symbol "=>"
          retTy <- typeExpr
          _ <- symbol ")"
          return $ TFunc fixedTys mVarTy retTy
