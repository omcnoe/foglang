module Foglang.Parser.Expr (exprBlock) where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text qualified as T
import Foglang.AST (Expr (..), Ident (..), Param (..), TypeExpr (..))
import Foglang.Parser (Parser, SC, keyword, lexeme, scn, symbol)
import Foglang.Parser.FloatLit (floatLit)
import Foglang.Parser.Ident (identRaw)
import Foglang.Parser.IntLit (intLit)
import Foglang.Parser.StringLit (stringLit)
import Text.Megaparsec (Pos, many, notFollowedBy, optional, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (incorrectIndent, indentGuard, indentLevel, lineFold)

-- Entrypoint for all expression parsing, parses a block of one or more expressions.
exprBlock :: Parser Expr
exprBlock = do
  refPos <- indentLevel
  e <- exprBlockItem refPos
  es <- many (try $ indentGuard scn EQ refPos *> exprBlockItem refPos)
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
lineFoldExpr refPos = lineFold scn (\sc' -> exprWith sc' refPos)

letExpr :: Pos -> Parser Expr
letExpr refPos = do
  _ <- lexeme (keyword "let")
  name <- lexeme identRaw

  params <- do
    firstParam <- optional param
    restParams <- case firstParam of
      Nothing -> return []
      Just _ -> many $ optional (symbol "->") *> param
    return $ maybe [] (: restParams) firstParam

  typeAnno <- case params of
    [] -> symbol ":" *> typeExpr
    _ -> symbol "=>" *> typeExpr
  _ <- symbol "="

  rhs <- exprBlock

  inExprs <- many (try $ indentGuard scn EQ refPos *> exprBlockItem refPos)
  let inExpr = case inExprs of
        [] -> Sequence []
        [x] -> x
        xs -> Sequence xs

  return $ Let name params typeAnno rhs inExpr
  where
    param :: Parser Param
    param =
      (UnitParam <$ symbol "()")
        <|> try
          ( do
              _ <- symbol "("
              name <- lexeme identRaw
              _ <- symbol ":"
              ty <- typeExpr
              _ <- symbol ")"
              return $ TypedParam name ty
          )

    typeExpr :: Parser TypeExpr
    typeExpr = (NamedType <$> lexeme identRaw) <|> funcTypeExpr

    funcTypeExpr :: Parser TypeExpr
    funcTypeExpr = do
      _ <- symbol "("
      paramTys <- typeExpr `sepBy1` symbol "->"
      ret <- optional $ try $ symbol "=>" *> typeExpr
      _ <- symbol ")"
      case (paramTys, ret) of
        ([t], Nothing) -> return t
        (ts, Just r) -> return $ FuncType ts r
        _ -> fail "function type requires => return type"

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
      col <- indentLevel
      when (col < minCol) $ incorrectIndent EQ minCol col

    -- Raw qualified identifier (no trailing whitespace); wrapped by lexeme' below.
    qualIdentRaw :: Parser Ident
    qualIdentRaw = try $ do
      parts <- identRaw `sepBy1` char '.'
      return $ Ident $ T.intercalate "." $ map (\(Ident t) -> t) parts

    atom :: Parser Expr
    atom =
      try ifExpr
        <|> try (FloatLit <$> lexeme' floatLit)
        <|> try (IntLit <$> lexeme' intLit)
        <|> try (StrLit <$> lexeme' stringLit)
        <|> try (Var <$> lexeme' qualIdentRaw)
        <|> paren

    argAtom :: Parser Expr
    argAtom =
      try (FloatLit <$> lexeme' floatLit)
        <|> try (IntLit <$> lexeme' intLit)
        <|> try (StrLit <$> lexeme' stringLit)
        <|> try (Var <$> lexeme' qualIdentRaw)
        <|> paren

    paren :: Parser Expr
    paren = do
      _ <- symbol' "("
      e <- exprWith sc' blockRefPos
      _ <- symbol' ")"
      return e

    ifExpr :: Parser Expr
    ifExpr = do
      ifCol <- indentLevel
      _ <- keyword' "if"
      cond <- exprWith sc' blockRefPos
      _ <- atLeast ifCol *> keyword' "then"
      thenBranch <- exprWith sc' blockRefPos
      _ <- atLeast blockRefPos *> keyword' "else" -- blockRefPos: allows else-if chains
      elseBranch <- exprWith sc' blockRefPos
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
