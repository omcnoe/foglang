module Foglang.AST (Ident, IntLit (..), FloatLit (..), Expr (..)) where

import Data.Text qualified as T

type Ident = T.Text

data IntLit = Decimal T.Text | Binary T.Text | Octal T.Text | Hex T.Text
  deriving (Eq, Show)

data FloatLit = DecimalFloat T.Text | HexFloat T.Text
  deriving (Eq, Show)

data Expr
  = Ident Ident
  | IntLit IntLit
  | FloatLit FloatLit
  | Let Ident [Ident] Expr
  | If Expr Expr Expr
  | BinaryOp Expr T.Text Expr
  | Application Expr [Expr]
  deriving (Eq, Show)
