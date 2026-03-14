module Foglang.AST
  ( Ident,
    IntLit (..),
    FloatLit (..),
    Expr (..),
    PackageDecl (..),
    ImportDecl (..),
    Header (..),
    GoFile (..),
  )
where

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

newtype PackageDecl = PackageDecl Ident
  deriving (Eq, Show)

-- The optional Text is the import alias: Nothing = normal, Just "." = dot import,
-- Just "_" = blank import, Just name = named alias.
-- TODO consider replacing this with discriminated union
data ImportDecl = ImportDecl (Maybe T.Text) T.Text
  deriving (Eq, Show)

data Header = Header PackageDecl [ImportDecl]
  deriving (Eq, Show)

-- TODO proper design for package/module/namespace system, and how that interacts with Go's package system.
-- For now we just generate a single Go file with a single package.
data GoFile = GoFile Header [Expr]
  deriving (Eq, Show)
