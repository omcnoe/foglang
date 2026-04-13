module Foglang.Parser.Types (params, typeExpr) where

import Foglang.AST (ConcreteShape (..), Ident (..), Param (..), TypeExpr (..), pattern UnitTypeExpr)
import Control.Monad.Reader (asks)
import Foglang.Parser (Parser, continuation, envFoldCol, freshTypeVar, keyword, lexeme, symbol)
import Foglang.Parser.Ident (ident)
import Text.Megaparsec (getSourcePos, many, optional, try, (<|>))
import Text.Megaparsec.Char (string)

param :: Parser (Param TypeExpr)
param = unit <|> try typed <|> try parens <|> bare
  where
    unit = PUnit <$ symbol "()"
    typed = do
      _ <- symbol "("
      name <- lexeme ident
      _ <- symbol ":"
      mVariadic <- optional (symbol "...")
      ty <- typeExpr
      foldCol <- asks envFoldCol
      _ <- continuation foldCol (symbol ")")
      return $ case mVariadic of
        Just _ -> PVariadic name ty
        Nothing -> PTyped name ty
    parens = do
      _ <- symbol "("
      p <- param
      foldCol <- asks envFoldCol
      _ <- continuation foldCol (symbol ")")
      return p
    bare = do
      p <- getSourcePos
      name <- lexeme ident
      t <- freshTypeVar p
      return $ PTyped name t

params :: Parser [Param TypeExpr]
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
    <|> try (TShape (CNamed (Ident "struct{}")) <$ lexeme (string "struct{}"))
    <|> try (UnitTypeExpr <$ lexeme (string "()"))
    <|> try (TShape . CNamed <$> lexeme ident)
    <|> funcTypeExpr

mapTypeExpr :: Parser TypeExpr
mapTypeExpr = do
  _ <- keyword "map"
  _ <- symbol "["
  keyTy <- typeExpr
  foldCol <- asks envFoldCol
  _ <- continuation foldCol (symbol "]")
  valTy <- typeExpr
  return $ TShape (CMap keyTy valTy)

sliceTypeExpr :: Parser TypeExpr
sliceTypeExpr = do
  _ <- symbol "["
  _ <- symbol "]"
  TShape . CSlice <$> typeExpr

funcTypeExpr :: Parser TypeExpr
funcTypeExpr = do
  _ <- symbol "("
  mZero <- optional (try (symbol "()" >> symbol "=>"))
  case mZero of
    Just _ -> do
      retTy <- typeExpr
      foldCol <- asks envFoldCol
      _ <- continuation foldCol (symbol ")")
      return $ TShape (CFunc [UnitTypeExpr] Nothing retTy)
    Nothing -> do
      fixedTys <- many (try (typeExpr <* optional (symbol "->")))
      mVarTy <- optional (try (symbol "..." *> typeExpr))
      case (fixedTys, mVarTy) of
        ([], Nothing) -> fail "function type with no parameters"
        _ -> do
          _ <- symbol "=>"
          retTy <- typeExpr
          foldCol <- asks envFoldCol
          _ <- continuation foldCol (symbol ")")
          return $ TShape (CFunc fixedTys mVarTy retTy)
