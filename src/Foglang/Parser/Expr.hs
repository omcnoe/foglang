module Foglang.Parser.Expr (expr) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Expr (..))
import Foglang.Parser (Parser, keyword, lexeme, symbol)
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Text.Megaparsec (chunk, many, notFollowedBy, some, try, (<|>))

expr :: Parser Expr
expr = makeExprParser atom operatorTable
  where
    atom =
      try letExpr
        <|> try ifExpr
        <|> try (FloatLit <$> floatLit)
        <|> try (IntLit <$> intLit)
        <|> try (Var <$> qualIdent)
        <|> paren

    argAtom =
      try (FloatLit <$> floatLit)
        <|> try (IntLit <$> intLit)
        <|> try (Var <$> qualIdent)
        <|> paren

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

    binaryOpExpr :: T.Text -> Operator Parser Expr
    binaryOpExpr op = InfixL $ lexeme $ do
      _ <- chunk op
      return (\e1 e2 -> BinaryOp e1 op e2)

    -- Like binaryOpExpr, but fails (without consuming) if followed by the given
    -- char. Used to disambiguate operators sharing a prefix: & vs &&, | vs ||.
    binaryOpExprNotFollowedBy :: T.Text -> T.Text -> Operator Parser Expr
    binaryOpExprNotFollowedBy op notFollowedBy' = InfixL $ try $ lexeme $ do
      _ <- chunk op
      notFollowedBy (chunk notFollowedBy')
      return (\e1 e2 -> BinaryOp e1 op e2)

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some argAtom
      return (\f -> Application f args)

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [applicationExpr],
        [binaryOpExpr "*", binaryOpExpr "/", binaryOpExpr "%", binaryOpExpr "<<", binaryOpExpr ">>", binaryOpExpr "&^", binaryOpExprNotFollowedBy "&" "&"],
        [binaryOpExpr "+", binaryOpExpr "-", binaryOpExprNotFollowedBy "|" "|", binaryOpExpr "^"],
        [binaryOpExpr "==", binaryOpExpr "!=", binaryOpExpr ">=", binaryOpExpr ">", binaryOpExpr "<=", binaryOpExpr "<"],
        [binaryOpExpr "&&"],
        [binaryOpExpr "||"]
      ]
