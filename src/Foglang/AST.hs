module Foglang.AST
  ( Ident (..),
    QualIdent (..),
    unitIdent,
    IntLit (..),
    FloatLit (..),
    Expr (..),
    PackageClause (..),
    ImportAlias (..),
    ImportDecl (..),
    Header (..),
    FogFile (..),
  )
where

import Data.String (IsString (..))
import Data.Text qualified as T

data Ident = Ident T.Text
  deriving (Eq, Show)

-- TODO golang only allows one level of qualification package.name, do we have different requirements? Leave as is for now.
data QualIdent = QualIdent [Ident]
  deriving (Eq, Show)

-- TODO maybe QualIdent should be a sum type? Anyway it's not important right now.
unitIdent :: QualIdent
unitIdent = QualIdent [Ident "()"]

instance IsString Ident where
  fromString = Ident . T.pack

instance IsString QualIdent where
  fromString = QualIdent . map Ident . T.splitOn "." . T.pack

data IntLit
  = Decimal T.Text
  | Binary T.Text
  | Octal T.Text
  | Hex T.Text
  deriving (Eq, Show)

data FloatLit
  = DecimalFloat T.Text
  | HexFloat T.Text
  deriving (Eq, Show)

data Expr
  = Var QualIdent
  | IntLit IntLit
  | FloatLit FloatLit
  | Let Ident [Ident] Expr
  | If Expr Expr Expr
  | BinaryOp Expr T.Text Expr
  | Application Expr [Expr]
  deriving (Eq, Show)

newtype PackageClause = PackageClause Ident
  deriving (Eq, Show)

data ImportAlias
  = None -- import qualified by package name
  | Alias Ident -- import qualified by alias
  | Dot -- import without qualifiers
  | Blank -- import for side effects only
  deriving (Eq, Show)

data ImportDecl = ImportDecl ImportAlias T.Text
  deriving (Eq, Show)

data Header = Header PackageClause [ImportDecl]
  deriving (Eq, Show)

-- TODO proper design for package/module/namespace system, and how that interacts with Go's package system.
-- For now we just generate a single Go file with a single package.
data FogFile = FogFile Header [Expr]
  deriving (Eq, Show)
