module Foglang.Parser.Expr (childBlockExprSequence) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Reader (asks)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), ExprAnn (..), Ident (..), MatchArm (..), TypeExpr (..), exprPos, tsFloat, tsInt, pattern UnitType)
import Foglang.Parser
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.Patterns (pattern')
import Foglang.Parser.StringLit (stringLit)
import Foglang.Parser.Types (params, typeExpr)
import Text.Megaparsec (Pos, getSourcePos, many, optional, some, try, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Pos (sourceLine)

-- | Indented expression sub-block (let RHS, func body, then/else, etc.).
-- Simple shorthand helper
childBlockExprSequence :: Parser Expr
childBlockExprSequence = childBlock (withoutSemicolons exprSequence)

-- | Parse one or more items at the current position. Assumes the caller
-- has validated that this position is valid (childBlock guards col,
-- delimiters use fold SC, let in-expr uses continuation). Subsequent
-- items must align with the first (col >= firstItemCol).
exprSequence :: Parser Expr
exprSequence = do
  let item = do
        foldCol <- resolveFoldCol
        itemLine <- sourceLine <$> getSourcePos
        try (letExpr foldCol) <|> foldExpr foldCol itemLine

  p <- getSourcePos
  itemLi <- getCol
  e <- item
  es <- many $ try $ continuation itemLi item
  case (e : es) of
    [x] -> return x
    xs -> do
      t <- freshTVar
      return $ ESequence ExprAnn {pos = p, ty = t, isStmt = False} xs

-- | A folded expression: enter a fold context, parse an expression.
foldExpr :: LineIndent -> Pos -> Parser Expr
foldExpr foldCol startLine = fold foldCol startLine expr

-- ----------------------------------------------------------------------------
-- Let
--
-- Structure:
--   FOLD(let name params [: type] =)
--   CHILD(rhs)
--   CONTINUATION(in-exprs)
--
-- name, params, type annotation, and = are inline tokens within the fold.
-- The fold's col > foldCol rule allows them to wrap onto continuation lines.

letExpr :: LineIndent -> Parser Expr
letExpr itemLi = do
  p <- getSourcePos
  let letLine = sourceLine p

  (name, ps, typeAnno, rhs) <- fold itemLi letLine $ do
    _ <- keyword "let"
    name <- lexeme ident
    ps <- params
    let sep = if null ps then symbol ":" else symbol "=>"
    typeAnno <- do
      mSep <- optional (try sep)
      case mSep of
        Just _ -> typeExpr
        Nothing -> freshTVar
    _ <- symbol "="

    rhs <- childBlockExprSequence
    return (name, ps, typeAnno, rhs)

  -- CONTINUATION: in-expression (sequence at same indent as the let).
  -- Outside the fold so envFoldCol is the enclosing context's, not the let's.
  mtin <- optional $ try $ continuation itemLi exprSequence

  t <- freshTVar
  return $ ELet ExprAnn {pos = p, ty = t, isStmt = True} name (Binding ps typeAnno rhs) mtin

-- ----------------------------------------------------------------------------
-- Expression parser
--
-- Parse an expression in context of an enclosing fold.
--
-- Two-layer makeExprParser (hierarchy: atom < term < expr), so tight
-- postfixes bind tighter than application (`f a[0]` = `f (a[0])`):
--   * atomOpTable - tight atom postfixes (eg. indexing)
--   * termOpTable - application + infix operators

expr :: Parser Expr
expr = makeExprParser term termOpTable
  where
    term :: Parser Expr
    term = lexeme $ makeExprParser atom atomOpTable

    atomOpTable :: [[Operator Parser Expr]]
    atomOpTable = [[indexOp], [spreadOp]]

    termOpTable :: [[Operator Parser Expr]]
    termOpTable =
      [ [applicationExpr],
        [InfixL (infixOp "*"), InfixL (infixOp "/"), InfixL (infixOp "%"), InfixL (infixOp "<<<"), InfixL (infixOp ">>>"), InfixL (infixOp "&&&")],
        [InfixL (infixOp "+"), InfixL (infixOp "-"), InfixL (infixOp "|||"), InfixL (infixOp "^^^")],
        [InfixR (infixOp "::")],
        [InfixL (infixOp "=="), InfixL (infixOp "!="), InfixL (infixOp ">="), InfixL (infixOp ">"), InfixL (infixOp "<="), InfixL (infixOp "<")],
        [InfixL (infixOp "&&")],
        [InfixL (infixOp "||")]
      ]

    -- Basic atom parser.
    -- Does NOT consume trailing whitespace as tight binding indexOp is
    -- leading whitespace sensitive, to distinguish eg. `f [0]` (application)
    -- from `f[0]` (indexing).
    atom :: Parser Expr
    atom =
        floatLitExpr -- must come before intLitExpr to resolve ambigous prefix case: 123.456 parses as valid int 123 leaving dangling .456
        <|> intLitExpr
        <|> stringLitExpr
        <|> sliceLit
        <|> mapLitExpr -- TODO need proper map lit impl
        <|> varExpr
        <|> unitLitExpr -- must come before parenExpr to resolve ambigous empty paren case: () is EUnitLit not empty ESequence
        <|> parenExpr
        <|> funcExpr
        <|> matchExpr
        <|> ifExpr

    -- Indexing: `xs[i]`.
    -- Leading hitespace sensitive - `xs [i]` is application with slice lit.
    -- `some` chains multiple `[...]` suffixes in one Postfix invocation,
    -- makeExprParser's Postfix only fires once per term otherwise.
    -- withoutSemicolons stops the closing `]`'s continuation from eating
    -- a stray `;` when we're inside a paren's semicolon-aware context.
    indexOp :: Operator Parser Expr
    indexOp = Postfix $ withoutSemicolons $ do
      idxs <- some $ do
        p <- getSourcePos
        _ <- string "["
        t <- freshTVar
        runEnvSC
        idx <- expr
        foldCol <- asks envFoldCol
        _ <- continuation foldCol (string "]")
        return (p, t, idx)
      return $ \base ->
        foldl' (\b (p, t, idx) -> EIndex ExprAnn {pos = p, ty = t, isStmt = False} b idx) base idxs

    -- Variadic spread: `xs...` or `xs ...`.
    -- Unlike indexOp, not leading whitespace sensitive. Only valid inside
    -- variadic function application - checked during type inference.
    spreadOp :: Operator Parser Expr
    spreadOp = Postfix $ try $ do
      runEnvSC
      _ <- string "..."
      t <- freshTVar
      return $ \base -> EVariadicSpread ExprAnn {pos = exprPos base, ty = t, isStmt = False} base

    -- Parse an infix operator with boundary check.
    infixOp :: T.Text -> Parser (Expr -> Expr -> Expr)
    infixOp op = do
      _ <- operator op
      t <- freshTVar
      return (\e1 e2 -> EInfixOp ExprAnn {pos = exprPos e1, ty = t, isStmt = False} e1 op e2)

    floatLitExpr  = do p <- getSourcePos; lit <- floatLit;  t <- freshTConstrained tsFloat;      pure $ EFloatLit ExprAnn {pos = p, ty = t, isStmt = False} lit
    intLitExpr    = do p <- getSourcePos; lit <- intLit;    t <- freshTConstrained tsInt;        pure $ EIntLit   ExprAnn {pos = p, ty = t, isStmt = False} lit
    stringLitExpr = do p <- getSourcePos; lit <- stringLit; t <- pure $ TNamed $ Ident "string"; pure $ EStrLit   ExprAnn {pos = p, ty = t, isStmt = False} lit

    -- Slice literal: items are folded expressions (no let absorption),
    -- separated by `;` or newlines. Delimited by [ ].
    -- Content obeys the enclosing fold's indentation rules.
    sliceLit :: Parser Expr
    sliceLit = do
      p <- getSourcePos
      _ <- string "["
      runEnvSC
      items <- withSemicolons $ do
        let oneItem = do
              foldCol <- resolveFoldCol
              itemLine <- sourceLine <$> getSourcePos
              foldExpr foldCol itemLine
        itemLi <- getCol
        mFirst <- optional oneItem
        case mFirst of
          Nothing -> pure []
          Just first -> do
            rest <- many $ try $ continuation itemLi oneItem
            return (first : rest)
      foldCol <- asks envFoldCol
      _ <- continuation foldCol (string "]")
      t <- freshTVar
      return $ ESliceLit ExprAnn {pos = p, ty = t, isStmt = False} items

    mapLitExpr  = do p <- getSourcePos; _  <- string "{}"; t <- freshTVar;     pure $ EMapLit  ExprAnn {pos = p, ty = t, isStmt = False}
    varExpr     = do p <- getSourcePos; qi <- qualIdent;   t <- freshTVar;     pure $ EVar     ExprAnn {pos = p, ty = t, isStmt = False} qi
    unitLitExpr = do p <- getSourcePos; _  <- string "()"; t <- pure UnitType; pure $ EUnitLit ExprAnn {pos = p, ty = t, isStmt = False}

    -- Parens: delimited by ( ), semicolons are valid separators inside.
    -- Content obeys the enclosing fold's indentation rules.
    parenExpr :: Parser Expr
    parenExpr = do
      _ <- string "("
      runEnvSC
      inner <- withSemicolons exprSequence
      foldCol <- asks envFoldCol
      _ <- continuation foldCol (string ")")
      return inner

    -- func: resolve own fold, open fold for header, childBlock for body.
    -- FOLD(func params [=> type] =)
    -- CHILD(body)
    funcExpr :: Parser Expr
    funcExpr = do
      p <- getSourcePos
      let funcLine = sourceLine p
      funcCol <- resolveFoldCol
      fold funcCol funcLine $ do
        _ <- keyword "func"
        ps <- params
        mArrow <- optional (try (symbol "=>"))
        typeAnno <- case mArrow of
          Just _ -> typeExpr
          Nothing -> freshTVar
        _ <- symbol "="
        body <- childBlockExprSequence
        t <- freshTVar
        return $ ELambda ExprAnn {pos = p, ty = t, isStmt = False} (Binding ps typeAnno body)

    -- match: resolve own fold for header, continuation for arms.
    -- FOLD(match scrutinee)
    -- CONTINUATION(with arms...)
    matchExpr :: Parser Expr
    matchExpr = do
      p <- getSourcePos
      matchCol <- resolveFoldCol
      scrut <- fold matchCol (sourceLine p) $ do
        _ <- keyword "match"
        expr
      arms <- continuation matchCol $ do
        _ <- keyword "with"
        matchArms
      t <- freshTVar
      return $ EMatch ExprAnn {pos = p, ty = t, isStmt = False} scrut arms
      where
        -- Match arms: one or more, strictly indented past the match keyword.
        -- Strictly indented resolves issues with ambiguous nested matches.
        -- Uses childBlock to enforce col > matchCol, then sequences arms
        -- at the first arm's column, same pattern as exprSequence.
        matchArms :: Parser [MatchArm]
        matchArms = childBlock $ do
          armLi <- getCol
          firstArm <- matchArm
          restArms <- many $ try $ continuation armLi matchArm
          return (firstArm : restArms)

        -- Single arm: resolve own fold for header, childBlock for body.
        -- FOLD(| pattern =>)
        -- CHILD(body)
        matchArm :: Parser MatchArm
        matchArm = do
          p <- getSourcePos
          let armLine = sourceLine p
          armCol <- resolveFoldCol
          fold armCol armLine $ do
            _ <- symbol "|"
            pat <- pattern'
            _ <- symbol "=>"
            body <- childBlockExprSequence
            return $ MatchArm p pat body

    -- if cond then body [else body]
    -- "else if" is just else with an if expression inside -- no special case needed.
    ifExpr :: Parser Expr
    ifExpr = do
      p <- getSourcePos
      let ifLine = sourceLine p
      ifCol <- resolveFoldCol
      cond <- fold ifCol ifLine (keyword "if" *> expr)
      thenBranch <- continuation ifCol $ do
        branchLine <- sourceLine <$> getSourcePos
        fold ifCol branchLine (keyword "then" *> childBlockExprSequence)
      mElseBranch <- optional $ try $ continuation ifCol $ do
        branchLine <- sourceLine <$> getSourcePos
        fold ifCol branchLine (keyword "else" *> childBlockExprSequence)
      t <- freshTVar
      let elseBranch = case mElseBranch of
            Just e  -> e
            -- empty else branch defaults to () value
            Nothing -> EUnitLit ExprAnn {pos = p, ty = UnitType, isStmt = False}
      return $ EIf ExprAnn {pos = p, ty = t, isStmt = False} cond thenBranch elseBranch

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some term
      t <- freshTVar
      return (\f -> EApplication ExprAnn {pos = exprPos f, ty = t, isStmt = True} f args)
