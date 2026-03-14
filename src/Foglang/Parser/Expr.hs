module Foglang.Parser.Expr (expr) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Foglang.AST (Expr (..))
import Foglang.Parser (Parser, keyword, symbol)
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident)
import Foglang.Parser.IntLit (intLit)
import Text.Megaparsec (many, some, try, (<|>))

expr :: Parser Expr
expr = makeExprParser atom operatorTable
  where
    atom =
      try letExpr
        <|> try ifExpr
        <|> try (FloatLit <$> floatLit)
        <|> try (IntLit <$> intLit)
        <|> paren
        <|> (Ident <$> ident)

    argAtom =
      try (FloatLit <$> floatLit)
        <|> try (IntLit <$> intLit)
        <|> paren
        <|> (Ident <$> ident)

    paren = do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e

    letExpr = do
      _ <- keyword "let"
      i <- ident
      p <- many ident
      _ <- symbol "="
      e <- expr
      return (Let i p e)

    ifExpr = do
      _ <- keyword "if"
      cond <- expr
      _ <- keyword "then"
      thenBranch <- expr
      _ <- keyword "else"
      elseBranch <- expr
      return (If cond thenBranch elseBranch)

    binaryOpExpr :: Text -> Operator Parser Expr
    binaryOpExpr s = InfixL $ do
      _ <- symbol s
      return (\e1 e2 -> BinaryOp e1 s e2)

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some argAtom
      return (\f -> Application f args)

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [applicationExpr],
        [binaryOpExpr "*", binaryOpExpr "/"],
        [binaryOpExpr "+", binaryOpExpr "-"]
      ]
