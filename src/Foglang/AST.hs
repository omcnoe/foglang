module Foglang.AST
  ( Ident (..),
    IntLit (..),
    FloatLit (..),
    StringLit (..),
    TypeExpr (..),
    Param (..),
    Binding (..),
    Expr (..),
    MatchArm (..),
    Pattern (..),
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
  deriving (Eq, Ord, Show)

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
  | SliceType TypeExpr -- slice type, e.g. []int; also the type of a variadic parameter var inside a function body
  | MapType TypeExpr TypeExpr -- map type, e.g. map[int][]int
  | FuncType [TypeExpr] (Maybe TypeExpr) TypeExpr -- fixed param types, optional variadic param type, return type
  deriving (Eq, Show)

data Param
  = UnitParam -- ()
  | TypedParam Ident TypeExpr -- (name : type)
  | VariadicParam Ident TypeExpr -- (name : ...type), must be the final param
  deriving (Eq, Show)

-- The common shape of a let-binding and a lambda.
-- params ([] = value, [...] = function), value/function return type, and rhs.
data Binding = Binding [Param] TypeExpr Expr
  deriving (Eq, Show)

-- AST for expressions, directly built from user source code. No name resolution or complete types yet.
-- Any TypeExpr here is user provided.
data Expr
  = EVar Ident
  | EIntLit IntLit
  | EFloatLit FloatLit
  | EStrLit StringLit
  | EUnitLit -- the () literal; not a function call, just the unit value
  | ELet Ident Binding Expr -- name, binding, inExpr
  | ELambda Binding -- anonymous "binding" (no name, no inExpr)
  | EIf Expr Expr Expr
  | EInfixOp Expr T.Text Expr
  | EApplication Expr [Expr]
  | EIndex Expr Expr -- expr[expr] — slice/map indexing
  | ESliceLit [Expr] -- slice literal: [], [x], [x, y, z]
  | EMapLit -- empty map literal: {} TODO support map contents
  | ESequence [Expr]
  | EVariadicSpread Expr -- expr... unpacks []T into a variadic slot at a call site
  | EMatch Expr [MatchArm] -- match scrutinee with | pattern => body
  deriving (Eq, Show)

data MatchArm = MatchArm Pattern Expr
  deriving (Eq, Show)

data Pattern
  = PWildcard -- _
  | PVar Ident -- variable binding
  | PIntLit IntLit -- integer literal
  | PBoolLit Bool -- true, false
  | PSliceEmpty -- []
  | PCons Pattern Pattern -- x :: rest
  | PTuple [Pattern] -- (a, b)
  deriving (Eq, Show)

newtype PackageClause = PackageClause Ident
  deriving (Eq, Show)

data ImportAlias
  = Default -- import qualified by package name (e.g. import "fmt" -> fmt.Println)
  | Alias Ident -- import with custom qualifier (e.g. import f "fmt" -> f.Println)
  | Dot -- import without qualifier (dangerous) (e.g. import . "fmt" -> Println)
  | Blank -- import for side effects only (e.g. import _ "net/http/pprof")
  deriving (Eq, Show)

data ImportDecl = ImportDecl ImportAlias T.Text
  deriving (Eq, Show)

data Header = Header PackageClause [ImportDecl]
  deriving (Eq, Show)

-- TODO proper design for package/module/namespace system, and how that interacts with Go's package system.
-- For now we just generate a single Go file with a single package.
data FogFile = FogFile Header Expr
  deriving (Eq, Show)
