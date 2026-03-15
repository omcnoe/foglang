module Foglang.AST
  ( Ident (..),
    IntLit (..),
    FloatLit (..),
    StringLit (..),
    TypeExpr (..),
    Param (..),
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

instance IsString Ident where
  fromString = Ident . T.pack

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

newtype StringLit = StringLit T.Text
  deriving (Eq, Show)

data TypeExpr
  = NamedType Ident -- named type, e.g. int, float64, bool, unit
  | FuncType [TypeExpr] TypeExpr -- function type: (T1 -> T2 -> ... => Tr)
  deriving (Eq, Show)

data Param
  = UnitParam -- ()
  | TypedParam Ident TypeExpr -- (name : type)
  deriving (Eq, Show)

data Expr
  = Var Ident
  | IntLit IntLit
  | FloatLit FloatLit
  | StrLit StringLit
  | Let Ident [Param] TypeExpr Expr Expr -- name, params, type annotation, rhs, inExpr
  | If Expr Expr Expr
  | BinaryOp Expr T.Text Expr
  | Application Expr [Expr]
  | Sequence [Expr]
  deriving (Eq, Show)

newtype PackageClause = PackageClause Ident
  deriving (Eq, Show)

data ImportAlias
  = None        -- qualified by package name
  | Alias Ident
  | Dot         -- without qualifiers (dangerous)
  | Blank       -- side effects only
  deriving (Eq, Show)

data ImportDecl = ImportDecl ImportAlias T.Text
  deriving (Eq, Show)

data Header = Header PackageClause [ImportDecl]
  deriving (Eq, Show)

-- TODO proper design for package/module/namespace system, and how that interacts with Go's package system.
-- For now we just generate a single Go file with a single package.
data FogFile = FogFile Header Expr
  deriving (Eq, Show)
