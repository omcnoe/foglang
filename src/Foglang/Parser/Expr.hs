module Foglang.Parser.Expr (sequence') where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), ExprAnn (..), Ident (..), MatchArm (..), TypeExpr (..), pattern UnitType, exprPos, tsInt, tsFloat)
import Foglang.Parser (Parser, SC(..), freshConstrained, freshTVar, keyword, lexeme, symbol, scn, LineIndent(..), unLineIndent)
import Text.Megaparsec.Char.Lexer (incorrectIndent, indentGuard)
import Foglang.Parser.Patterns (pattern')
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Foglang.Parser.Types (params, typeExpr)
import Text.Megaparsec (Pos, SourcePos, choice, getSourcePos, lookAhead, many, mkPos, notFollowedBy, optional, satisfy, sepBy, some, try, unPos, (<|>))
import Text.Megaparsec.Pos (sourceLine, sourceColumn)
import Text.Megaparsec.Char (string)

-- Consume whitespace between sequence items, enforcing indentation if a
-- parent column is set.
indentedScn :: Maybe LineIndent -> SC
indentedScn Nothing = scn
indentedScn (Just li) = SC $ indentGuard (runSC scn) GT (unLineIndent li) *> pure ()

-- Core sequence parser: parses one or more expressions in order, with items
-- at column > the parent's column. parentLi = Nothing for top-level
-- or inside parens (no constraint). startLine = the line number of the
-- introducing keyword, used to detect mid-line expressions.
sequence' :: Maybe LineIndent -> Pos -> Parser Expr
sequence' parentLi startLine = do
  p <- getSourcePos
  case parentLi of
    Nothing -> pure ()
    Just li -> do
      let col = sourceColumn p
      when (col <= unLineIndent li) $ incorrectIndent GT (unLineIndent li) col
  e <- sequenceItem parentLi startLine
  es <- many $ try $ do
    runSC (indentedScn parentLi)
    sequenceItem parentLi startLine
  case (e : es) of
    [x] -> return x
    xs -> do
      t <- freshTVar
      return $ ESequence ExprAnn { pos = p, ty = t, isStmt = False } xs

-- A sequence item is either:
-- a let binding (which absorbs any subsequent expressions as its in-expression)
-- a line-folded expression
--
-- Computes the fold column for this item: if we're on a new line (past the
-- start line), the item's own column defines the fold. If we're still on the
-- start line (mid-line), inherit the parent's column.
sequenceItem :: Maybe LineIndent -> Pos -> Parser Expr
sequenceItem parentLi startLine = do
  p <- getSourcePos
  let col = sourceColumn p
      foldCol
        | sourceLine p > startLine = LineIndent col
        | Just li <- parentLi      = li
        | otherwise                = LineIndent col
  try (letExpr parentLi foldCol) <|> lineFoldExpr foldCol

-- A line folded expression: parsed as a single logical line that may span
-- multiple physical lines via indentation (line folding).
--
-- The fold's sc' succeeds if EITHER:
-- (a) the next token is indented past foldCol (Rule 1: child), OR
-- (b) the next token is at foldCol AND starts with an unambiguously infix
--     operator (Rule 2: same-column infix exception).
lineFoldExpr :: LineIndent -> Parser Expr
lineFoldExpr foldCol = do
  let foldColPos = unLineIndent foldCol
      sc' = SC $ try $ do
        runSC scn
        col <- sourceColumn <$> getSourcePos
        if col > foldColPos
          then pure ()                      -- Rule 1: child (continuation)
          else if col == foldColPos
            then startsWithUnambiguousInfix  -- Rule 2: same-column infix exception
            else fail "not a continuation line"
  exprWith sc' foldCol

-- Check (via lookAhead) that the next token starts with an unambiguously
-- infix operator. These are operators that can ONLY be infix, never prefix.
-- Ambiguous operators (- and +) are excluded — they follow Rule 2 normally.
startsWithUnambiguousInfix :: Parser ()
startsWithUnambiguousInfix = do
  _ <- lookAhead $ choice $ map (try . string) unambiguousInfixOps
  pure ()
  where
    unambiguousInfixOps :: [T.Text]
    unambiguousInfixOps =
      -- Triple-char operators first (before their shorter prefixes)
      [ "|||", "&&&", "^^^", "<<<", ">>>"
      -- Two-char operators
      , "||", "&&", "==", "!=", "<=", ">=", "::"
      -- Single-char operators (< and > are comparisons, not shifts)
      , "<", ">", "*", "/", "%"
      ]

letExpr :: Maybe LineIndent -> LineIndent -> Parser Expr
letExpr parentLi letCol = do
  p <- getSourcePos
  let letLine = sourceLine p
  _ <- lexeme scn (keyword "let")
  name <- lexeme scn ident

  ps <- params scn

  -- Dispatch on what follows the params to determine the type annotation.
  typeAnno <- case ps of
    [] -> do
      -- No params: either `: type =` (value with explicit type) or `=` (value with inferred type)
      mColon <- optional (try (symbol scn ":"))
      case mColon of
        Just _ -> typeExpr scn
        Nothing -> freshTVar
    _ -> do
      -- Has params: either `=> type =` (function with explicit return type) or `=` (inferred return type)
      mArrow <- optional (try (symbol scn "=>"))
      case mArrow of
        Just _ -> typeExpr scn
        Nothing -> freshTVar
  _ <- symbol scn "="

  rhs <- sequence' (Just letCol) letLine

  seqPos <- getSourcePos
  contItems <- many $ try $ do
    runSC (indentedScn parentLi)
    sequenceItem parentLi letLine
  mtin <- case contItems of
        [] -> return Nothing
        [x] -> return (Just x)
        xs -> do
          t <- freshTVar
          return $ Just (ESequence ExprAnn { pos = seqPos, ty = t, isStmt = False } xs)

  t <- freshTVar
  return $ ELet ExprAnn { pos = p, ty = t, isStmt = True } name (Binding ps typeAnno rhs) mtin

-- Core match arm parser: accepts any arm at column >= the match's line-indent.
matchArms :: LineIndent -> Parser [MatchArm]
matchArms matchCol = do
  let matchColPos = unLineIndent matchCol
  col <- sourceColumn <$> getSourcePos
  when (col < matchColPos) $ incorrectIndent GT (mkPos (unPos matchColPos - 1)) col
  firstArm <- matchArmBody matchCol
  restArms <- many $ try $ do
    runSC scn
    armCol <- sourceColumn <$> getSourcePos
    when (armCol < matchColPos) $ incorrectIndent GT (mkPos (unPos matchColPos - 1)) armCol
    matchArmBody matchCol
  return (firstArm : restArms)

-- Parse a single arm: | pattern => body
-- The arm body is scoped to the match's line-indent, not the pipe's token
-- column. This is correct both when | is on its own line (line-indent
-- equals the match's) and when | is mid-line after "with" (the pipe's
-- column would be too high).
matchArmBody :: LineIndent -> Parser MatchArm
matchArmBody matchCol = do
  p <- getSourcePos
  let pipeLine = sourceLine p
  _ <- symbol scn "|"
  pat <- pattern' scn
  _ <- symbol scn "=>"
  body <- sequence' (Just matchCol) pipeLine
  return $ MatchArm p pat body


-- Expression parser parameterised on a space consumer.
-- sc' controls how far whitespace is consumed between tokens.
-- lineIndent is the column of the first non-whitespace character on the
-- physical line where parsing started (from lineFoldExpr's foldCol).
-- func/if/match use it for their body sequences instead of their own token
-- column, so that mid-line constructs (e.g. `let x = if ...`) scope correctly.
exprWith :: SC -> LineIndent -> Parser Expr
exprWith sc' lineIndent = makeExprParser atom operatorTable
  where
    -- Indentation aware versions of lexeme/symbol/keyword
    lexeme' :: Parser a -> Parser a
    lexeme' p = p <* (try (runSC sc') <|> pure ())

    symbol' :: T.Text -> Parser T.Text
    symbol' s = lexeme' (string s)

    keyword' :: T.Text -> Parser T.Text
    keyword' w = lexeme' (keyword w)

    -- Raw qualified identifier (no trailing whitespace); wrapped by lexeme' below.
    atom :: Parser Expr
    atom =
      try funcExpr
        <|> try matchExpr
        <|> try ifExpr
        <|> try (do p <- getSourcePos; t <- freshConstrained tsFloat; EFloatLit ExprAnn { pos = p, ty = t, isStmt = False } <$> lexeme' floatLit)
        <|> try (do p <- getSourcePos; t <- freshConstrained tsInt; EIntLit ExprAnn { pos = p, ty = t, isStmt = False } <$> lexeme' intLit)
        <|> try (do p <- getSourcePos; EStrLit ExprAnn { pos = p, ty = TNamed (Ident "string"), isStmt = False } <$> lexeme' stringLit)
        <|> try sliceLit
        <|> try (do p <- getSourcePos; t <- freshTVar; EMapLit ExprAnn { pos = p, ty = t, isStmt = False } <$ symbol' "{}")
        <|> try indexableVar
        <|> try (do p <- getSourcePos; EUnitLit ExprAnn { pos = p, ty = UnitType, isStmt = False } <$ symbol' "()")
        <|> indexableParen

    -- Parse zero or more [expr] index suffixes, then consume trailing whitespace.
    withIndexSuffix :: Expr -> Parser Expr
    withIndexSuffix base = do
      idxs <- many (try $ do p <- getSourcePos; t <- freshTVar; idx <- string "[" *> exprWith sc' lineIndent <* symbol' "]"; return (p, t, idx))
      try (runSC sc') <|> pure ()
      return $ foldl (\b (p, t, idx) -> EIndex ExprAnn { pos = p, ty = t, isStmt = False } b idx) base idxs

    -- Parse an identifier, then check for immediate [expr] index suffix.
    -- The identifier is parsed WITHOUT trailing whitespace first, so we can
    -- distinguish foo[x] (index, no space) from foo [x] (application with slice literal).
    indexableVar :: Parser Expr
    indexableVar = do
      p <- getSourcePos
      t <- freshTVar
      EVar ExprAnn { pos = p, ty = t, isStmt = False } <$> qualIdent >>= withIndexSuffix

    indexableParen :: Parser Expr
    indexableParen = do
      _ <- string "("
      runSC scn
      parenLine <- sourceLine <$> getSourcePos
      inner <- sequence' Nothing parenLine
      runSC scn
      _ <- string ")"
      withIndexSuffix inner

    funcExpr :: Parser Expr
    funcExpr = do
      p <- getSourcePos
      let funcLine = sourceLine p
      _ <- keyword' "func"
      ps <- params sc'
      -- Optional return type annotation: `=> type` or inferred
      mArrow <- optional (try (symbol' "=>"))
      typeAnno <- case mArrow of
        Just _ -> typeExpr sc'
        Nothing -> freshTVar
      _ <- symbol' "="
      -- Body: try scn to break out of fold, then parse as Sequence with
      -- line-indent = lineIndent (the physical line's leading indent, not
      -- the func token's column). This ensures mid-line func expressions
      -- (e.g. `let f = func (x) => () = ...`) scope correctly.
      body <- try (runSC scn *> sequence' (Just lineIndent) funcLine) <|> exprWith sc' lineIndent
      t <- freshTVar
      return $ ELambda ExprAnn { pos = p, ty = t, isStmt = False } (Binding ps typeAnno body)

    -- Match expression. Scrutinee is parsed within the fold (sc').
    -- "with" is reached via scn (breaking out of fold), allowing it at
    -- the same column as "match" or indented. Arms follow on new lines
    -- at column >= lineIndent (the physical line's leading indent).
    matchExpr :: Parser Expr
    matchExpr = do
      p <- getSourcePos
      _ <- keyword' "match"
      scrut <- exprWith sc' lineIndent
      runSC scn
      _ <- keyword "with"
      runSC scn
      arms <- matchArms lineIndent
      t <- freshTVar
      return $ EMatch ExprAnn { pos = p, ty = t, isStmt = False } scrut arms

    sliceLit :: Parser Expr
    sliceLit = do
      p <- getSourcePos
      _ <- symbol' "["
      exprs <- sepBy (exprWith sc' lineIndent) (symbol' ",")
      _ <- symbol' "]"
      t <- freshTVar
      return $ ESliceLit ExprAnn { pos = p, ty = t, isStmt = False } exprs

    -- If/then/else: condition, then-branch, and else-branch are each Sequences
    -- with line-indent = lineIndent (the physical line's leading indent).
    -- "else if" is a compound keyword that recurses with the same lineIndent
    -- to avoid the staircase problem.
    ifExpr :: Parser Expr
    ifExpr = do
      p <- getSourcePos
      let ifLine = sourceLine p
      _ <- keyword' "if"
      parseIfChain p ifLine

    parseIfChain :: SourcePos -> Pos -> Parser Expr
    parseIfChain p ifLine = do
      let lineIndentPos = unLineIndent lineIndent
      cond <- sequence' (Just lineIndent) ifLine
      runSC scn
      thenCol <- sourceColumn <$> getSourcePos
      when (thenCol < lineIndentPos) $ incorrectIndent GT (mkPos (unPos lineIndentPos - 1)) thenCol
      _ <- keyword "then"
      runSC scn
      thenBranch <- sequence' (Just lineIndent) ifLine
      runSC scn
      elseCol <- sourceColumn <$> getSourcePos
      when (elseCol < lineIndentPos) $ incorrectIndent GT (mkPos (unPos lineIndentPos - 1)) elseCol
      mElse <- optional (keyword "else")
      case mElse of
        Nothing -> do
          seqPos <- getSourcePos
          t1 <- freshTVar
          t2 <- freshTVar
          return (EIf ExprAnn { pos = p, ty = t1, isStmt = False } cond thenBranch (ESequence ExprAnn { pos = seqPos, ty = t2, isStmt = False } []))
        Just _ -> do
          runSC scn
          mIf <- optional (keyword "if")
          case mIf of
            Just _ -> do
              runSC scn
              t <- freshTVar
              elseIfPos <- getSourcePos
              elseBranch <- parseIfChain elseIfPos ifLine
              return (EIf ExprAnn { pos = p, ty = t, isStmt = False } cond thenBranch elseBranch)
            Nothing -> do
              t <- freshTVar
              elseBranch <- sequence' (Just lineIndent) ifLine
              return (EIf ExprAnn { pos = p, ty = t, isStmt = False } cond thenBranch elseBranch)

    -- Parse an infix operator, ensuring it's not a prefix of a longer operator.
    -- e.g. ">" must not match the start of ">=" or ">>>".
    opParser :: T.Text -> Parser (Expr -> Expr -> Expr)
    opParser op = lexeme' $ do
      _ <- string op
      notFollowedBy (satisfy isOpChar)
      t <- freshTVar
      return (\e1 e2 -> EInfixOp ExprAnn { pos = exprPos e1, ty = t, isStmt = False } e1 op e2)

    isOpChar :: Char -> Bool
    isOpChar c = c `elem` ("=<>&|^:!+-*/%" :: [Char])

    infixLOpExpr :: T.Text -> Operator Parser Expr
    infixLOpExpr op = InfixL (opParser op)

    infixROpExpr :: T.Text -> Operator Parser Expr
    infixROpExpr op = InfixR (opParser op)

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some $ do
        e <- atom
        -- TODO implement variadic spread ... as real operator rather than magic logic inside the application expression
        (do t <- freshTVar; EVariadicSpread ExprAnn { pos = exprPos e, ty = t, isStmt = False } e <$ lexeme' (string "...")) <|> pure e
      t <- freshTVar
      return (\f -> EApplication ExprAnn { pos = exprPos f, ty = t, isStmt = True } f args)

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [applicationExpr],
        [infixLOpExpr "*", infixLOpExpr "/", infixLOpExpr "%", infixLOpExpr "<<<", infixLOpExpr ">>>", infixLOpExpr "&&&"],
        [infixLOpExpr "+", infixLOpExpr "-", infixLOpExpr "|||", infixLOpExpr "^^^"],
        [infixROpExpr "::"],
        [infixLOpExpr "==", infixLOpExpr "!=", infixLOpExpr ">=", infixLOpExpr ">", infixLOpExpr "<=", infixLOpExpr "<"],
        [infixLOpExpr "&&"],
        [infixLOpExpr "||"]
      ]
