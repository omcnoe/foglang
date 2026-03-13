{-# LANGUAGE OverloadedStrings #-}

module Foglang.Expr (Expr(..), expr) where

import Data.Functor ((<&>))
import Foglang.Core (Parser, lexeme, symbol)
import Foglang.Ident (Ident, ident)
import Foglang.IntLit (IntLit, intLit)
import Text.Megaparsec (try, (<|>))

data Expr
  = Ident Ident
  | IntLit IntLit
  | Let (Ident, Expr)
  deriving (Eq, Show)

expr :: Parser Expr
expr =
  let let' = do
        _ <- symbol "let"
        i <- ident
        _ <- symbol "="
        e <- expr
        return (i, e)
   in (try ident <&> Ident)
        <|> (try intLit <&> IntLit)
        <|> (try let' <&> Let)
