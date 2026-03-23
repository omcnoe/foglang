module Foglang.Parser.Expr (sequence') where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), Ident (..), MatchArm (..), TypeExpr (..), TypeSet (..), exprPos)
import Foglang.Parser (Parser, SC, freshConstrained, freshTVar, keyword, scn)
import Foglang.Parser.Patterns (pattern')
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Foglang.Parser.Types (params, typeExpr)
import Text.Megaparsec (Pos, SourcePos, choice, getSourcePos, lookAhead, many, mkPos, notFollowedBy, optional, satisfy, sepBy, some, try, unPos, (<|>))
import Text.Megaparsec.Pos (sourceLine, sourceColumn)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L (incorrectIndent, indentGuard, lexeme, symbol)

-- Consume whitespace between sequence items, enforcing indentation if a
-- parent column is set.
indentedScn :: Maybe Pos -> SC
indentedScn Nothing = scn
indentedScn (Just minCol) = L.indentGuard scn GT minCol *> pure ()

-- Parse zero or more continuation items at the current indentation level.
sequenceCont :: Maybe Pos -> Pos -> Parser [Expr]
sequenceCont parentCol startLine =
  many $ try $ do
    indentedScn parentCol
    sequenceItem parentCol startLine

-- Core sequence parser: parses one or more expressions in order, with items
-- at column > the parent's column. parentCol = Nothing for top-level
-- or inside parens (no constraint). startLine = the line number of the
-- introducing keyword, used to detect mid-line expressions.
sequence' :: Maybe Pos -> Pos -> Parser Expr
sequence' parentCol startLine = do
  pos <- getSourcePos
  case parentCol of
    Nothing -> pure ()
    Just minCol -> do
      let col = sourceColumn pos
      when (col <= minCol) $ L.incorrectIndent GT minCol col
  e <- sequenceItem parentCol startLine
  es <- sequenceCont parentCol startLine
  case (e : es) of
    [x] -> return x
    xs -> do
      t <- freshTVar
      return $ ESequence pos t xs

-- A sequence item is either:
-- a let binding (which absorbs any subsequent expressions as its in-expression)
-- a line-folded expression
--
-- Computes the fold column for this item: if we're on a new line (past the
-- start line), the item's own column defines the fold. If we're still on the
-- start line (mid-line), inherit the parent's column.
sequenceItem :: Maybe Pos -> Pos -> Parser Expr
sequenceItem parentCol startLine = do
  pos <- getSourcePos
  let col = sourceColumn pos
      foldCol
        | sourceLine pos > startLine = col
        | Just minCol <- parentCol   = minCol
        | otherwise                  = col
  try (letExpr parentCol) <|> lineFoldExpr foldCol

-- A line folded expression: parsed as a single logical line that may span
-- multiple physical lines via indentation (line folding).
--
-- The fold's sc' succeeds if EITHER:
-- (a) the next token is indented past foldCol (Rule 1: child), OR
-- (b) the next token is at foldCol AND starts with an unambiguously infix
--     operator (Rule 2: same-column infix exception).
lineFoldExpr :: Pos -> Parser Expr
lineFoldExpr foldCol = do
  let sc' = try $ do
        scn
        col <- sourceColumn <$> getSourcePos
        if col > foldCol
          then pure ()                      -- Rule 1: child (continuation)
          else if col == foldCol
            then startsWithUnambiguousInfix  -- Rule 2: same-column infix exception
            else fail "not a continuation line"
  exprWith sc'

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

letExpr :: Maybe Pos -> Parser Expr
letExpr parentCol = do
  pos <- getSourcePos
  let letLine = sourceLine pos
      letCol = sourceColumn pos
  _ <- L.lexeme scn (keyword "let")
  name <- L.lexeme scn ident

  ps <- params scn

  -- Dispatch on what follows the params to determine the type annotation.
  typeAnno <- case ps of
    [] -> do
      -- No params: either `: type =` (value with explicit type) or `=` (value with inferred type)
      mColon <- optional (try (L.symbol scn ":"))
      case mColon of
        Just _ -> typeExpr scn
        Nothing -> freshTVar
    _ -> do
      -- Has params: either `=> type =` (function with explicit return type) or `=` (inferred return type)
      mArrow <- optional (try (L.symbol scn "=>"))
      case mArrow of
        Just _ -> typeExpr scn
        Nothing -> freshTVar
  _ <- L.symbol scn "="

  rhs <- sequence' (Just letCol) letLine

  seqPos <- getSourcePos
  contItems <- sequenceCont parentCol letLine
  mtin <- case contItems of
        [] -> return Nothing
        [x] -> return (Just x)
        xs -> do
          t <- freshTVar
          return $ Just (ESequence seqPos t xs)

  t <- freshTVar
  return $ ELet pos t name (Binding ps typeAnno rhs) mtin

-- Core match arm parser: accepts any arm at column >= matchCol.
matchArms :: Pos -> Parser [MatchArm]
matchArms matchCol = do
  col <- sourceColumn <$> getSourcePos
  when (col < matchCol) $ L.incorrectIndent GT (mkPos (unPos matchCol - 1)) col
  firstArm <- matchArmBody
  restArms <- many $ try $ do
    scn
    armCol <- sourceColumn <$> getSourcePos
    when (armCol < matchCol) $ L.incorrectIndent GT (mkPos (unPos matchCol - 1)) armCol
    matchArmBody
  return (firstArm : restArms)

-- Parse a single arm: | pattern => body
matchArmBody :: Parser MatchArm
matchArmBody = do
  pos <- getSourcePos
  let pipeLine = sourceLine pos
      pipeCol = sourceColumn pos
  _ <- L.symbol scn "|"
  pat <- pattern' scn
  _ <- L.symbol scn "=>"
  body <- sequence' (Just pipeCol) pipeLine
  return $ MatchArm pos pat body


-- Expression parser parameterised on a space consumer.
-- sc' controls how far whitespace is consumed between tokens.
exprWith :: SC -> Parser Expr
exprWith sc' = makeExprParser atom operatorTable
  where
    -- Indentation aware versions of lexeme/symbol/keyword
    lexeme' :: Parser a -> Parser a
    lexeme' p = p <* (try sc' <|> pure ())

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
        <|> try (do pos <- getSourcePos; t <- freshConstrained TSFloat; EFloatLit pos t <$> lexeme' floatLit)
        <|> try (do pos <- getSourcePos; t <- freshConstrained TSInt; EIntLit pos t <$> lexeme' intLit)
        <|> try (do pos <- getSourcePos; EStrLit pos (TNamed (Ident "string")) <$> lexeme' stringLit)
        <|> try sliceLit
        <|> try (do pos <- getSourcePos; t <- freshTVar; EMapLit pos t <$ symbol' "{}")
        <|> try indexableVar
        <|> try (do pos <- getSourcePos; EUnitLit pos <$ symbol' "()")
        <|> indexableParen

    -- Parse zero or more [expr] index suffixes, then consume trailing whitespace.
    withIndexSuffix :: Expr -> Parser Expr
    withIndexSuffix base = do
      idxs <- many (try $ do pos <- getSourcePos; t <- freshTVar; idx <- string "[" *> exprWith sc' <* symbol' "]"; return (pos, t, idx))
      try sc' <|> pure ()
      return $ foldl (\b (pos, t, idx) -> EIndex pos t b idx) base idxs

    -- Parse an identifier, then check for immediate [expr] index suffix.
    -- The identifier is parsed WITHOUT trailing whitespace first, so we can
    -- distinguish foo[x] (index, no space) from foo [x] (application with slice literal).
    indexableVar :: Parser Expr
    indexableVar = do
      pos <- getSourcePos
      t <- freshTVar
      EVar pos t <$> qualIdent >>= withIndexSuffix

    indexableParen :: Parser Expr
    indexableParen = do
      _ <- string "("
      scn
      parenLine <- sourceLine <$> getSourcePos
      inner <- sequence' Nothing parenLine
      scn
      _ <- string ")"
      withIndexSuffix inner

    funcExpr :: Parser Expr
    funcExpr = do
      pos <- getSourcePos
      let funcLine = sourceLine pos
          funcCol = sourceColumn pos
      _ <- keyword' "func"
      ps <- params sc'
      -- Optional return type annotation: `=> type` or inferred
      mArrow <- optional (try (symbol' "=>"))
      typeAnno <- case mArrow of
        Just _ -> typeExpr sc'
        Nothing -> freshTVar
      _ <- symbol' "="
      -- Body: try scn to break out of fold, then parse as Sequence with
      -- line-indent = funcCol. For same-line bodies, scn just consumes spaces
      -- and sequence parses the single expression. For multi-line bodies, scn
      -- crosses the newline and line-indent limits scope.
      body <- try (scn *> sequence' (Just funcCol) funcLine) <|> exprWith sc'
      t <- freshTVar
      return $ ELambda pos t (Binding ps typeAnno body)

    -- Match expression. Scrutinee is parsed within the fold (sc').
    -- "with" is reached via scn (breaking out of fold), allowing it at
    -- the same column as "match" or indented. Arms follow on new lines.
    matchExpr :: Parser Expr
    matchExpr = do
      pos <- getSourcePos
      let matchCol = sourceColumn pos
      _ <- keyword' "match"
      scrut <- exprWith sc'
      scn
      _ <- keyword "with"
      scn
      arms <- matchArms matchCol
      t <- freshTVar
      return $ EMatch pos t scrut arms

    sliceLit :: Parser Expr
    sliceLit = do
      pos <- getSourcePos
      _ <- symbol' "["
      exprs <- sepBy (exprWith sc') (symbol' ",")
      _ <- symbol' "]"
      t <- freshTVar
      return $ ESliceLit pos t exprs

    -- If/then/else: condition, then-branch, and else-branch are each Sequences
    -- with line-indent = ifCol. "else if" is a compound keyword that recurses
    -- with the same ifCol to avoid the staircase problem.
    ifExpr :: Parser Expr
    ifExpr = do
      pos <- getSourcePos
      let ifLine = sourceLine pos
          ifCol = sourceColumn pos
      _ <- keyword' "if"
      parseIfChain pos ifCol ifLine

    parseIfChain :: SourcePos -> Pos -> Pos -> Parser Expr
    parseIfChain pos ifCol ifLine = do
      cond <- sequence' (Just ifCol) ifLine
      scn
      thenCol <- sourceColumn <$> getSourcePos
      when (thenCol < ifCol) $ L.incorrectIndent GT (mkPos (unPos ifCol - 1)) thenCol
      _ <- keyword "then"
      scn
      thenBranch <- sequence' (Just ifCol) ifLine
      scn
      elseCol <- sourceColumn <$> getSourcePos
      when (elseCol < ifCol) $ L.incorrectIndent GT (mkPos (unPos ifCol - 1)) elseCol
      mElse <- optional (keyword "else")
      case mElse of
        Nothing -> do
          seqPos <- getSourcePos
          t1 <- freshTVar
          t2 <- freshTVar
          return (EIf pos t1 cond thenBranch (ESequence seqPos t2 []))
        Just _ -> do
          scn
          mIf <- optional (keyword "if")
          case mIf of
            Just _ -> do
              scn
              t <- freshTVar
              elseIfPos <- getSourcePos
              elseBranch <- parseIfChain elseIfPos ifCol ifLine
              return (EIf pos t cond thenBranch elseBranch)
            Nothing -> do
              t <- freshTVar
              elseBranch <- sequence' (Just ifCol) ifLine
              return (EIf pos t cond thenBranch elseBranch)

    -- Parse an infix operator, ensuring it's not a prefix of a longer operator.
    -- e.g. ">" must not match the start of ">=" or ">>>".
    opParser :: T.Text -> Parser (Expr -> Expr -> Expr)
    opParser op = lexeme' $ do
      _ <- string op
      notFollowedBy (satisfy isOpChar)
      t <- freshTVar
      return (\e1 e2 -> EInfixOp (exprPos e1) t e1 op e2)

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
        (do t <- freshTVar; EVariadicSpread (exprPos e) t e <$ lexeme' (string "...")) <|> pure e
      t <- freshTVar
      return (\f -> EApplication (exprPos f) t f args)

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
