module Foglang.Parser.Expr (sequence') where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), MatchArm (..))
import Foglang.Parser (Parser, SC, keyword, scn)
import Foglang.Parser.Patterns (pattern')
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Foglang.Parser.Types (params, typeExpr)
import Text.Megaparsec (Pos, choice, getSourcePos, lookAhead, many, mkPos, optional, sepBy, some, try, unPos, (<|>))
import Text.Megaparsec.Pos (sourceLine)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L (incorrectIndent, indentGuard, indentLevel, lexeme, symbol)

-- Core sequence parser: parses one or more expressions in order, with items
-- at column > the parent's line-indent. parentIndent = Nothing for top-level
-- or inside parens (no constraint). keywordLine = the line number of the
-- introducing keyword, used to detect mid-line expressions.
sequence' :: Maybe Pos -> Pos -> Parser Expr
sequence' parentIndent keywordLine = do
  case parentIndent of
    Nothing -> pure ()
    Just f -> do
      col <- L.indentLevel
      when (col <= f) $ L.incorrectIndent GT f col
  e <- sequenceItem parentIndent keywordLine
  es <- many $ try $ do
    case parentIndent of
      Nothing -> scn
      Just f -> do
        _ <- L.indentGuard scn GT f
        pure ()
    sequenceItem parentIndent keywordLine
  return $ case (e : es) of
    [x] -> x
    xs -> ESequence xs

-- A sequence item is either:
-- a let binding (which absorbs any subsequent expressions as its in-expression)
-- a line-folded expression
--
-- Computes the line-indent for this item: if we're on the same line as the
-- keyword that introduced the sequence, use the parent's line-indent (we're
-- mid-line). Otherwise, our column IS the line-indent.
sequenceItem :: Maybe Pos -> Pos -> Parser Expr
sequenceItem parentIndent keywordLine = do
  currentLine <- sourceLine <$> getSourcePos
  pos <- L.indentLevel
  let lineIndent
        | currentLine > keywordLine = pos
        | Just p <- parentIndent    = p
        | otherwise                 = pos
  try (letExpr parentIndent) <|> lineFoldExpr lineIndent

-- A line folded expression: parsed as a single logical line that may span
-- multiple physical lines via indentation (line folding).
--
-- The fold's sc' succeeds if EITHER:
-- (a) the next token is indented past the line-indent (Rule 1: child), OR
-- (b) the next token is at the same column as the line-indent AND starts
--     with an unambiguously infix operator (Rule 2: line-indent exception).
lineFoldExpr :: Pos -> Parser Expr
lineFoldExpr lineIndent = do
  let sc' = try $ do
        scn
        level <- L.indentLevel
        if level > lineIndent
          then pure ()                      -- Rule 1: child (continuation)
          else if level == lineIndent
            then startsWithUnambiguousInfix  -- Rule 2: line-indent exception
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
letExpr parentIndent = do
  letLine <- sourceLine <$> getSourcePos
  letCol <- L.indentLevel
  _ <- L.lexeme scn (keyword "let")
  name <- L.lexeme scn ident

  ps <- params scn

  typeAnno <- case ps of
    [] -> L.symbol scn ":" *> typeExpr scn
    _ -> L.symbol scn "=>" *> typeExpr scn
  _ <- L.symbol scn "="

  rhs <- sequence' (Just letCol) letLine

  inExprs <- many $ try $ do
    case parentIndent of
      Nothing -> scn
      Just f -> do
        _ <- L.indentGuard scn GT f
        pure ()
    sequenceItem parentIndent letLine
  let inExpr = case inExprs of
        [] -> ESequence []
        [x] -> x
        xs -> ESequence xs

  return $ ELet name (Binding ps typeAnno rhs) inExpr

-- Core match arm parser: accepts any arm at column >= matchCol.
matchArms :: Pos -> Parser [MatchArm]
matchArms matchCol = do
  col <- L.indentLevel
  when (col < matchCol) $ L.incorrectIndent GT (mkPos (unPos matchCol - 1)) col
  firstArm <- matchArmBody
  restArms <- many $ try $ do
    scn
    armCol <- L.indentLevel
    when (armCol < matchCol) $ L.incorrectIndent GT (mkPos (unPos matchCol - 1)) armCol
    matchArmBody
  return (firstArm : restArms)

-- Parse a single arm: | pattern => body
matchArmBody :: Parser MatchArm
matchArmBody = do
  pipeLine <- sourceLine <$> getSourcePos
  pipeCol <- L.indentLevel
  _ <- L.symbol scn "|"
  pat <- pattern'
  _ <- L.symbol scn "=>"
  body <- sequence' (Just pipeCol) pipeLine
  return $ MatchArm pat body


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
        <|> try (EFloatLit <$> lexeme' floatLit)
        <|> try (EIntLit <$> lexeme' intLit)
        <|> try (EStrLit <$> lexeme' stringLit)
        <|> try sliceLit
        <|> try (EMapLit <$ symbol' "{}")
        <|> try indexableVar
        <|> try (EUnitLit <$ symbol' "()")
        <|> indexableParen

    -- Parse zero or more [expr] index suffixes, then consume trailing whitespace.
    withIndexSuffix :: Expr -> Parser Expr
    withIndexSuffix base = do
      idxs <- many (try $ string "[" *> exprWith sc' <* symbol' "]")
      try sc' <|> pure ()
      return $ foldl EIndex base idxs

    -- Parse an identifier, then check for immediate [expr] index suffix.
    -- The identifier is parsed WITHOUT trailing whitespace first, so we can
    -- distinguish foo[x] (index, no space) from foo [x] (application with slice literal).
    indexableVar :: Parser Expr
    indexableVar = EVar <$> qualIdent >>= withIndexSuffix

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
      funcLine <- sourceLine <$> getSourcePos
      funcCol <- L.indentLevel
      _ <- keyword' "func"
      ps <- params sc'
      typeAnno <- symbol' "=>" *> typeExpr sc'
      _ <- symbol' "="
      -- Body: try scn to break out of fold, then parse as Sequence with
      -- line-indent = funcCol. For same-line bodies, scn just consumes spaces
      -- and sequence parses the single expression. For multi-line bodies, scn
      -- crosses the newline and line-indent limits scope.
      body <- try (scn *> sequence' (Just funcCol) funcLine) <|> exprWith sc'
      return $ ELambda (Binding ps typeAnno body)

    -- Match expression. Scrutinee is parsed within the fold (sc').
    -- "with" is reached via scn (breaking out of fold), allowing it at
    -- the same column as "match" or indented. Arms follow on new lines.
    matchExpr :: Parser Expr
    matchExpr = do
      matchCol <- L.indentLevel
      _ <- keyword' "match"
      scrut <- exprWith sc'
      scn
      _ <- keyword "with"
      scn
      arms <- matchArms matchCol
      return $ EMatch scrut arms

    sliceLit :: Parser Expr
    sliceLit = do
      _ <- symbol' "["
      exprs <- sepBy (exprWith sc') (symbol' ",")
      _ <- symbol' "]"
      return $ ESliceLit exprs

    -- If/then/else: condition, then-branch, and else-branch are each Sequences
    -- with line-indent = ifCol. "else if" is a compound keyword that recurses
    -- with the same ifCol to avoid the staircase problem.
    ifExpr :: Parser Expr
    ifExpr = do
      ifLine <- sourceLine <$> getSourcePos
      ifCol <- L.indentLevel
      _ <- keyword' "if"
      parseIfChain ifCol ifLine

    parseIfChain :: Pos -> Pos -> Parser Expr
    parseIfChain ifCol ifLine = do
      cond <- sequence' (Just ifCol) ifLine
      scn
      thenCol <- L.indentLevel
      when (thenCol < ifCol) $ L.incorrectIndent GT (mkPos (unPos ifCol - 1)) thenCol
      _ <- keyword "then"
      scn
      thenBranch <- sequence' (Just ifCol) ifLine
      scn
      elseCol <- L.indentLevel
      when (elseCol < ifCol) $ L.incorrectIndent GT (mkPos (unPos ifCol - 1)) elseCol
      mElse <- optional (keyword "else")
      case mElse of
        Nothing -> return (EIf cond thenBranch (ESequence []))
        Just _ -> do
          scn
          mIf <- optional (keyword "if")
          case mIf of
            Just _ -> do
              scn
              elseBranch <- parseIfChain ifCol ifLine
              return (EIf cond thenBranch elseBranch)
            Nothing -> do
              elseBranch <- sequence' (Just ifCol) ifLine
              return (EIf cond thenBranch elseBranch)

    infixLOpExpr :: T.Text -> Operator Parser Expr
    infixLOpExpr op = InfixL $ lexeme' $ do
      _ <- string op
      return (\e1 e2 -> EInfixOp e1 op e2)

    infixROpExpr :: T.Text -> Operator Parser Expr
    infixROpExpr op = InfixR $ lexeme' $ do
      _ <- string op
      return (\e1 e2 -> EInfixOp e1 op e2)

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some $ do
        e <- atom
        (EVariadicSpread e <$ lexeme' (string "...")) <|> pure e
      return (\f -> EApplication f args)

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
