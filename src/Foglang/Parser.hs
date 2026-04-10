module Foglang.Parser
  ( -- Types
    Parser,
    SC (..),
    LineIndent (..),
    -- Running
    runParse,
    scn,
    -- Fresh type vars
    freshTVar,
    freshTConstrained,
    -- Vocabulary (using envSC from ReaderT)
    keyword,
    operator,
    symbol,
    lexeme,
    runEnvSC,
    isGoLetter,
    -- Combinators
    fold,
    continuation,
    childBlock,
    resolveFoldCol,
    withSemicolons,
    withoutSemicolons,
    -- Indentation
    getCol,
    guardColGT,
    envFoldCol,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (Reader, asks, local, runReader)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT, gets)
import Data.Char (isDigit, isLetter)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Void (Void)
import Foglang.AST (TypeExpr (..), TypeSet)
import Text.Megaparsec (ParseErrorBundle, ParsecT, Pos, choice, getSourcePos, lookAhead, notFollowedBy, optional, runParserT, satisfy, try, unPos, (<|>))
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (mkPos, sourceColumn, sourceLine)

-- ----------------------------------------------------------------------------
-- Core types

-- | Parser environment.
-- envSC: ambient space consumer (set by `fold`, read by keyword/symbol/lexeme).
-- envSemi: True inside delimiters (parens/brackets) where `;` separates items.
-- envFoldCol: the fold column of the enclosing context (set by `fold`).
-- envFoldLine: the line where the enclosing fold started.
data ParserEnv = ParserEnv
  { envSC :: SC,
    envSemi :: Bool,
    envFoldCol :: LineIndent,
    envFoldLine :: Pos
  }

-- | Parser state.
-- stateNextTVar: next fresh TVar ID.
data ParserState = ParserState
  { stateNextTVar :: Int }

-- | Main parser type.
-- StateT ParserState: mutable parser state. Outermost so that custom state
-- rewinds when megaparsec backtracks.
-- ParsecT: megaparsec parser layer.
-- Reader ParserEnv: carries ambient space consumer, fold column, and
-- semicolon context.
type Parser = StateT ParserState (ParsecT Void T.Text (Reader ParserEnv))

-- | Space consumer: controls how far whitespace is consumed between tokens.
-- Newtype-wrapped to prevent accidentally using an arbitrary Parser ()
-- where a space consumer is expected.
newtype SC = SC {runSC :: Parser ()}

-- | Column of the first non-whitespace character on a physical line.
-- LineIndent 0 means "no constraint" (col > 0 is always true since
-- megaparsec columns start at 1).
newtype LineIndent = LineIndent Int
  deriving (Eq, Ord)

-- ----------------------------------------------------------------------------
-- Running

-- | Unconstrained space consumer: eats all whitespace including newlines,
-- plus line comments (//) and block comments (/* */).
scn :: SC
scn =
  SC $
    L.space
      space1
      (L.skipLineComment "//")
      (L.skipBlockComment "/*" "*/")

-- | Run a parser to completion. Initial ambient SC is scn (unconstrained).
runParse :: Parser a -> String -> T.Text -> Either (ParseErrorBundle T.Text Void) a
runParse p name input =
  runReader (runParserT (evalStateT p initState) name input) initEnv
  where
    initState = ParserState { stateNextTVar = 0 }
    initEnv = ParserEnv { envSC = scn, envSemi = False, envFoldCol = LineIndent 0, envFoldLine = mkPos 1 }

-- ----------------------------------------------------------------------------
-- Fresh type variables

-- | Mint a fresh type variable.
freshTVar :: Parser TypeExpr
freshTVar = do
  n <- gets stateNextTVar
  put ParserState { stateNextTVar = n + 1 }
  return (TVar n)

-- | Mint a fresh constrained type variable (for literals etc.)
freshTConstrained :: TypeSet -> Parser TypeExpr
freshTConstrained s = do
  TVar n <- freshTVar
  return (TConstrained n s)

-- ----------------------------------------------------------------------------
-- Vocabulary
--
-- keyword, symbol, lexeme all read the ambient SC from the ReaderT and
-- consume trailing whitespace using it. Within a `fold`, the SC is
-- fold-aware (only accepts continuation lines). Outside a fold (or at
-- top level), the SC is `scn` (unconstrained).

-- | Consume trailing whitespace using the ambient SC.
-- Uses `try` so that failing to consume whitespace (e.g. at the end of
-- a fold) doesn't cause the token parse to fail.
runEnvSC :: Parser ()
runEnvSC = do
  sc <- asks envSC
  try (runSC sc) <|> pure ()

-- | Go-style letter: includes underscore (used for identifier boundaries).
isGoLetter :: Char -> Bool
isGoLetter c = isLetter c || c == '_'

-- | Keyword: parse a reserved word with word-boundary check and trailing whitespace.
keyword :: T.Text -> Parser T.Text
keyword w = try (string w <* notFollowedBy (satisfy (\c -> isGoLetter c || isDigit c))) <* runEnvSC

-- | Operator: parse an operator string with operator-boundary check and trailing whitespace.
-- Ensures we don't match a prefix of a longer operator (e.g. ">" must not match ">=").
operator :: T.Text -> Parser T.Text
operator op = try (string op <* notFollowedBy (satisfy isOpChar)) <* runEnvSC
  where isOpChar c = c `elem` ("=<>&|^:!+-*/%" :: [Char])

-- | Symbol (exact string match) with (optional) trailing whitespace. No boundary check.
symbol :: T.Text -> Parser T.Text
symbol s = string s <* runEnvSC

-- | Lexeme: run a parser, then consume (optional) trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* runEnvSC

-- ----------------------------------------------------------------------------
-- Indentation helpers

-- | Get the current column as a LineIndent.
getCol :: Parser LineIndent
getCol = LineIndent . unPos . sourceColumn <$> getSourcePos

-- | Guard that actual column > expected.
guardColGT :: LineIndent -> LineIndent -> Parser ()
guardColGT (LineIndent a) (LineIndent e) =
  when (a <= e) $ fail $ "incorrect indentation (got " ++ show a ++ ", should be > " ++ show e ++ ")"

-- | Guard that actual column >= expected.
guardColGE :: LineIndent -> LineIndent -> Parser ()
guardColGE (LineIndent a) (LineIndent e) =
  when (a < e) $ fail $ "incorrect indentation (got " ++ show a ++ ", should be >= " ++ show e ++ ")"

-- ----------------------------------------------------------------------------
-- FOLD and CONTINUATION and CHILD
--
-- FOLD: a region where continuation lines must be at col > foldCol
--   (or at col == foldCol with an unambiguous infix operator).
--   Used for expression continuation and construct headers.
--   Sets envFoldCol and envFoldLine for child constructs.
--
-- CONTINUATION: content at col >= lineIndent (same level as parent).
--   Used for then/else, with, match arms, let-in items, closing delimiters.
--   Does NOT set envFoldCol - compound constructs open their own folds.
--
-- CHILD blocks (col > envFoldCol guard + sequence parsing) are handled
-- by childBlock in Expr.hs, using the indent helpers from this module.

-- | Resolve the fold column for an item at the current position.
-- On a new line (past envFoldLine): the item's own column defines the fold.
-- On the start line at top level (envFoldCol = 0): use LineIndent 1.
-- On the start line with a parent constraint: inherit envFoldCol.
resolveFoldCol :: Parser LineIndent
resolveFoldCol = do
  foldCol <- asks envFoldCol
  foldLine <- asks envFoldLine
  curLine <- sourceLine <$> getSourcePos
  curCol <- getCol
  return $
    if curLine > foldLine
      then curCol
    else if foldCol == LineIndent 0
      then LineIndent 1
    else foldCol

-- | FOLD: set the ambient SC to a fold-aware consumer for foldCol.
-- All keyword/symbol/lexeme calls inside automatically use fold-aware
-- whitespace. Also sets envFoldCol and envFoldLine so that child
-- constructs can resolve their own fold columns via resolveFoldCol.
-- The previous SC is restored when the fold ends.
fold :: LineIndent -> Pos -> Parser a -> Parser a
fold foldCol foldLine =
  local $ \env -> env { envSC = foldSC, envFoldCol = foldCol, envFoldLine = foldLine }
  where
    foldSC = SC $ try $ do
      runSC scn
      col <- getCol
      if col > foldCol
        then pure ()
      else if col == foldCol
        then startsWithUnambiguousInfix
      else fail "not a continuation line"

    startsWithUnambiguousInfix = do
      _ <-
        lookAhead $
          choice $
            map
              (try . string)
              [ "|||", "&&&", "^^^", "<<<", ">>>",
                "||", "&&", "==", "!=", "<=", ">=", "<", ">",
                "::", "*", "/", "%", "|"
              ]
      pure ()

-- | CONTINUATION: consume whitespace (+ optional ";" if envSemi),
-- check column, then run parser. Uses scn directly to escape folds.
-- With semicolon: col > envFoldCol (semicolons relax alignment).
-- Without semicolon: col >= li (alignment with first squence item).
continuation :: LineIndent -> Parser a -> Parser a
continuation li p = do
  semi <- asks envSemi
  runSC scn
  let consumeSemi = isJust <$> optional (try (string ";" *> runSC scn))
  gotSemi <- if semi then consumeSemi else pure False
  col <- getCol
  if gotSemi
    then do
      foldCol <- asks envFoldCol
      guardColGT col foldCol
    else guardColGE col li
  p

-- | CHILD block: guard col > envFoldCol, then run parser.
-- Used for indented sub-blocks (let RHS, func body, match arms, etc.).
childBlock :: Parser a -> Parser a
childBlock p = do
  foldCol <- asks envFoldCol
  col <- getCol
  guardColGT col foldCol
  p

-- | Run a parser in a semicolon-aware context (inside delimiters).
withSemicolons :: Parser a -> Parser a
withSemicolons = local $ \env -> env { envSemi = True }

-- | Run a parser with semicolons disabled (e.g. let RHS).
withoutSemicolons :: Parser a -> Parser a
withoutSemicolons = local $ \env -> env { envSemi = False }
