module Foglang.Parser.Expr (exprBlock) where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..))
import Foglang.Parser (Parser, SC, keyword, scn)
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (ident, qualIdent)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Foglang.Parser.Types (params, typeExpr)
import Text.Megaparsec (Pos, many, notFollowedBy, some, try, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L (incorrectIndent, indentGuard, indentLevel, lexeme, lineFold, symbol)

-- Entrypoint for all expression parsing, parses a block of one or more expressions.
exprBlock :: Parser Expr
exprBlock = do
  refPos <- L.indentLevel
  e <- exprBlockItem refPos
  es <- many (try $ L.indentGuard scn EQ refPos *> exprBlockItem refPos)
  _ <- scn
  return $ case (e : es) of
    [x] -> x
    xs -> Sequence xs

-- An expression block item is either:
-- a let binding (which absorbs any subsequent indented expression block)
-- a line-folded expression
exprBlockItem :: Pos -> Parser Expr
exprBlockItem refPos = try (letExpr refPos) <|> lineFoldExpr refPos

-- A line folded expression: parsed as a single logical line that may span
-- multiple physical lines via indentation (line folding).
lineFoldExpr :: Pos -> Parser Expr
lineFoldExpr refPos = L.lineFold scn (\sc' -> exprWith sc' refPos)

letExpr :: Pos -> Parser Expr
letExpr refPos = do
  letCol <- L.indentLevel
  _ <- L.lexeme scn (keyword "let")
  name <- L.lexeme scn ident

  ps <- params scn

  typeAnno <- case ps of
    [] -> L.symbol scn ":" *> typeExpr
    _ -> L.symbol scn "=>" *> typeExpr
  _ <- L.symbol scn "="

  bodyCol <- L.indentLevel
  when (bodyCol <= letCol) $ L.incorrectIndent GT letCol bodyCol
  rhs <- exprBlock

  inExprs <- many (try $ L.indentGuard scn EQ refPos *> exprBlockItem refPos)
  let inExpr = case inExprs of
        [] -> Sequence []
        [x] -> x
        xs -> Sequence xs

  return $ Let name (Binding ps typeAnno rhs) inExpr

-- Expression parser parameterised on a space consumer and a block reference
-- position. sc' controls how far whitespace is consumed between tokens.
-- blockRefPos is the enclosing block's indentation column, used to validate
-- that 'else' keywords are not outdented past the block boundary.
exprWith :: SC -> Pos -> Parser Expr
exprWith sc' blockRefPos = makeExprParser atom operatorTable
  where
    -- Indentation aware versions of lexeme/symbol/keyword
    lexeme' :: Parser a -> Parser a
    lexeme' p = p <* (try sc' <|> pure ())

    symbol' :: T.Text -> Parser T.Text
    symbol' s = lexeme' (string s)

    keyword' :: T.Text -> Parser T.Text
    keyword' w = lexeme' (keyword w)

    -- Skip newlines then assert current column >= minCol.
    -- Used by ifExpr to validate that then/else are not outdented relative
    -- to the if keyword.
    atLeast :: Pos -> Parser ()
    atLeast minCol = do
      scn
      col <- L.indentLevel
      when (col < minCol) $ L.incorrectIndent EQ minCol col

    -- Raw qualified identifier (no trailing whitespace); wrapped by lexeme' below.
    atom :: Parser Expr
    atom =
      try funcExpr
        <|> try ifExpr
        <|> try (FloatLit <$> lexeme' floatLit)
        <|> try (IntLit <$> lexeme' intLit)
        <|> try (StrLit <$> lexeme' stringLit)
        <|> try (Var <$> lexeme' qualIdent)
        <|> try (UnitLit <$ symbol' "()")
        <|> paren

    argAtom :: Parser Expr
    argAtom = do
      e <-
        try (FloatLit <$> lexeme' floatLit)
          <|> try (IntLit <$> lexeme' intLit)
          <|> try (StrLit <$> lexeme' stringLit)
          <|> try (Var <$> lexeme' qualIdent)
          <|> try (UnitLit <$ lexeme' (string "()"))
          <|> paren
      (VariadicSpread e <$ lexeme' (string "...")) <|> pure e

    paren :: Parser Expr
    paren = do
      _ <- symbol' "("
      e <- exprWith sc' blockRefPos
      _ <- symbol' ")"
      return e

    funcExpr :: Parser Expr
    funcExpr = do
      _ <- keyword' "func"
      ps <- params sc'
      typeAnno <- symbol' "=>" *> typeExpr
      _ <- symbol' "="
      body <- exprWith sc' blockRefPos
      return $ Lambda (Binding ps typeAnno body)

    -- Anchor bodies to the 'if'/'else if'/'else' column to prevent outdenting.
    -- For else-if chains, ifBody is called recursively with the the new anchor,
    -- then/then-body/else of an else-if are measured from the else keyword,
    -- not from the nested 'if' keyword which may be pushed right.
    ifExpr :: Parser Expr
    ifExpr = do
      ifCol <- L.indentLevel
      ifBody ifCol
      where
        ifBody anchorCol = do
          _ <- keyword' "if"
          cond <- exprWith sc' blockRefPos
          _ <- atLeast anchorCol *> keyword' "then"
          _ <- atLeast anchorCol
          thenBranch <- exprWith sc' blockRefPos
          elseCol <- atLeast anchorCol *> L.indentLevel
          _ <- keyword' "else"
          elseBranch <- try (ifBody elseCol) <|> exprWith sc' blockRefPos
          return (If cond thenBranch elseBranch)

    binaryOpExpr :: T.Text -> Operator Parser Expr
    binaryOpExpr op = InfixL $ lexeme' $ do
      _ <- string op
      return (\e1 e2 -> BinaryOp e1 op e2)

    -- Like binaryOpExpr, but fails (without consuming) if followed by the given
    -- char. Used to disambiguate operators sharing a prefix: & vs &&, | vs ||.
    binaryOpExprNotFollowedBy :: T.Text -> T.Text -> Operator Parser Expr
    binaryOpExprNotFollowedBy op notFollowedBy' = InfixL $ try $ lexeme' $ do
      _ <- string op
      notFollowedBy (string notFollowedBy')
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
