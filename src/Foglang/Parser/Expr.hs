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
import Text.Megaparsec (Pos, SourcePos, getSourcePos, many, optional, some, try, (<|>))
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
-- The inner loop of a fold: atoms + operators using the ambient fold SC.
-- Compound constructs (func, match, if) nest child blocks within the fold.

expr :: Parser Expr
expr = makeExprParser atom operatorTable
  where
    atom :: Parser Expr
    atom =
      try funcExpr
        <|> try matchExpr
        <|> try ifExpr
        <|> try (do p <- getSourcePos; t <- freshTConstrained tsFloat; EFloatLit ExprAnn {pos = p, ty = t, isStmt = False} <$> lexeme floatLit)
        <|> try (do p <- getSourcePos; t <- freshTConstrained tsInt; EIntLit ExprAnn {pos = p, ty = t, isStmt = False} <$> lexeme intLit)
        <|> try (do p <- getSourcePos; EStrLit ExprAnn {pos = p, ty = TNamed (Ident "string"), isStmt = False} <$> lexeme stringLit)
        <|> try sliceLit
        <|> try (do p <- getSourcePos; t <- freshTVar; EMapLit ExprAnn {pos = p, ty = t, isStmt = False} <$ symbol "{}")
        <|> try indexableVar
        <|> try (do p <- getSourcePos; EUnitLit ExprAnn {pos = p, ty = UnitType, isStmt = False} <$ symbol "()")
        <|> indexableParen

    -- Parse zero or more [expr] index suffixes, then consume trailing
    -- whitespace. The identifier/paren is parsed WITHOUT trailing whitespace
    -- first, so we can distinguish foo[x] (index) from foo [x] (application).
    withIndexSuffix :: Expr -> Parser Expr
    withIndexSuffix base = do
      idxs <- many (try $ do p <- getSourcePos; t <- freshTVar; idx <- string "[" *> expr <* symbol "]"; return (p, t, idx))
      runEnvSC
      return $ foldl (\b (p, t, idx) -> EIndex ExprAnn {pos = p, ty = t, isStmt = False} b idx) base idxs

    indexableVar :: Parser Expr
    indexableVar = do
      p <- getSourcePos
      t <- freshTVar
      EVar ExprAnn {pos = p, ty = t, isStmt = False} <$> qualIdent >>= withIndexSuffix

    -- Parens: delimited by ( ), semicolons are valid separators inside.
    -- Content obeys the enclosing fold's indentation rules.
    indexableParen :: Parser Expr
    indexableParen = do
      _ <- string "("
      runEnvSC
      inner <- withSemicolons exprSequence
      foldCol <- asks envFoldCol
      -- string not symbol: no trailing whitespace so (e)[x] (index) vs (e) [x] (application)
      _ <- continuation foldCol (string ")")
      withIndexSuffix inner

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
      _ <- continuation foldCol (symbol "]")
      t <- freshTVar
      return $ ESliceLit ExprAnn {pos = p, ty = t, isStmt = False} items

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

    -- Parse an infix operator with boundary check.
    opParser :: T.Text -> Parser (Expr -> Expr -> Expr)
    opParser op = do
      _ <- operator op
      t <- freshTVar
      return (\e1 e2 -> EInfixOp ExprAnn {pos = exprPos e1, ty = t, isStmt = False} e1 op e2)

    applicationExpr :: Operator Parser Expr
    applicationExpr = Postfix $ do
      args <- some $ do
        e <- atom
        (do t <- freshTVar; EVariadicSpread ExprAnn {pos = exprPos e, ty = t, isStmt = False} e <$ symbol "...") <|> pure e
      t <- freshTVar
      return (\f -> EApplication ExprAnn {pos = exprPos f, ty = t, isStmt = True} f args)

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [applicationExpr],
        [InfixL (opParser "*"), InfixL (opParser "/"), InfixL (opParser "%"), InfixL (opParser "<<<"), InfixL (opParser ">>>"), InfixL (opParser "&&&")],
        [InfixL (opParser "+"), InfixL (opParser "-"), InfixL (opParser "|||"), InfixL (opParser "^^^")],
        [InfixR (opParser "::")],
        [InfixL (opParser "=="), InfixL (opParser "!="), InfixL (opParser ">="), InfixL (opParser ">"), InfixL (opParser "<="), InfixL (opParser "<")],
        [InfixL (opParser "&&")],
        [InfixL (opParser "||")]
      ]
