module Foglang.Parser (isGoLetter, Parser, SC(..), scn, lexeme, symbol, keyword, freshTVar, freshConstrained) where

import Control.Monad.State.Strict (State, get, put)
import Data.Char (isDigit, isLetter)
import Data.Text qualified as T
import Data.Void (Void)
import Foglang.AST (TypeExpr (..), TypeSet)
import Text.Megaparsec (ParsecT, notFollowedBy, satisfy, try)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

-- go's `letter` includes underscore, so we wrap Data.Char.isLetter
isGoLetter :: Char -> Bool
isGoLetter c = isLetter c || c == '_'

type Parser = ParsecT Void T.Text (State Int)

-- A space consumer: controls how far whitespace is consumed between tokens.
-- Newtype-wrapped to prevent accidentally using an arbitrary Parser () where
-- a space consumer is expected. Constructed only at sites with known
-- indentation semantics (scn, indentedScn, lineFoldExpr's fold-aware sc').
newtype SC = SC { runSC :: Parser () }

scn :: SC
scn = SC $
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- Wrappers around megaparsec's lexeme/symbol that accept our SC newtype.
lexeme :: SC -> Parser a -> Parser a
lexeme sc = L.lexeme (runSC sc)

symbol :: SC -> T.Text -> Parser T.Text
symbol sc = L.symbol (runSC sc)

keyword :: T.Text -> Parser T.Text
keyword w = try $ do
  t <- string w
  notFollowedBy (satisfy (\c -> isGoLetter c || isDigit c)) -- disambiguate from a possible Ident prefix
  return t

-- Mint a fresh type variable with a globally unique ID.
-- Uses State as the inner monad so IDs never backtrack on parse failures.
freshTVar :: Parser TypeExpr
freshTVar = do
  n <- get
  put (n + 1)
  return (TVar n)

-- Mint a fresh constrained type variable (for numeric literals).
freshConstrained :: TypeSet -> Parser TypeExpr
freshConstrained s = do
  n <- get
  put (n + 1)
  return (TConstrained n s)
