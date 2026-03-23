module Foglang.AST
  ( Ident (..),
    IntLit (..),
    FloatLit (..),
    StringLit (..),
    TypeSet (..),
    TypeExpr (..),
    pattern UnitType,
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
    exprPos,
    exprType,
    exprTypes,
    paramType,
    bindingType,
  )
where

import Data.String (IsString (..))
import Data.Text qualified as T
import Text.Megaparsec.Pos (SourcePos)

data Ident = Ident T.Text
  deriving (Eq, Ord, Show)

instance IsString Ident where
  fromString = Ident . T.pack

data IntLit
  = IntDecimal T.Text
  | IntBinary T.Text
  | IntOctal T.Text
  | IntHex T.Text
  deriving (Eq, Show)

data FloatLit
  = FloatDecimal T.Text
  | FloatHex T.Text
  deriving (Eq, Show)

newtype StringLit = StringLit T.Text
  deriving (Eq, Show)

-- Constraint sets for type variables introduced by literals.
data TypeSet
  = TSInt    -- int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64, uintptr, byte, rune
  | TSFloat  -- float32, float64
  deriving (Eq, Show)

data TypeExpr
  = TNamed Ident -- named type, e.g. int, float64, bool, unit
  | TSlice TypeExpr -- slice type, e.g. []int; also the type of a variadic parameter var inside a function body
  | TMap TypeExpr TypeExpr -- map type, e.g. map[int][]int
  | TFunc [TypeExpr] (Maybe TypeExpr) TypeExpr -- fixed param types, optional variadic param type, return type
  | TVar Int -- type variable, resolved during type inference
  | TConstrained Int TypeSet -- type variable constrained to a set of numeric types
  deriving (Eq, Show)

pattern UnitType :: TypeExpr
pattern UnitType = TNamed (Ident "()")

data Param
  = PUnit -- ()
  | PTyped Ident TypeExpr -- (name : type)
  | PVariadic Ident TypeExpr -- (name : ...type), must be the final param
  deriving (Eq, Show)

paramType :: Param -> TypeExpr
paramType PUnit = UnitType
paramType (PTyped _ ty) = ty
paramType (PVariadic _ ty) = ty

-- The common shape of a let-binding and a lambda.
-- params ([] = value, [...] = function), value/function return type, and rhs.
data Binding = Binding [Param] TypeExpr Expr
  deriving (Eq, Show)

-- Build the full type for a binding given its params and declared return type.
-- Value bindings (no params) return retTy directly.
-- A sole anonymous PUnit is a zero-param function: TFunc [] Nothing retTy.
-- Otherwise, each param contributes to fixedTys/mVarTy.
bindingType :: [Param] -> TypeExpr -> TypeExpr
bindingType [] retTy = retTy
bindingType [PUnit] retTy = TFunc [] Nothing retTy
bindingType ps retTy = TFunc fixedTys mVarTy retTy
  where
    fixedTys = [paramType p | p <- ps, not (isVariadic p)]
    mVarTy = case reverse ps of
      (PVariadic _ ty : _) -> Just ty
      _ -> Nothing
    isVariadic (PVariadic {}) = True
    isVariadic _ = False

-- AST for expressions. Every constructor carries a SourcePos and a TypeExpr.
-- The parser fills in placeholder types; inference resolves them.
-- EUnitLit has no TypeExpr field because its type is always TNamed "()" .
data Expr
  = EVar SourcePos TypeExpr Ident
  | EIntLit SourcePos TypeExpr IntLit
  | EFloatLit SourcePos TypeExpr FloatLit
  | EStrLit SourcePos TypeExpr StringLit
  | EUnitLit SourcePos -- the () literal; not a function call, just the unit value
  | ELet SourcePos TypeExpr Ident Binding (Maybe Expr) -- type, name, binding, optional continuation
  | ELambda SourcePos TypeExpr Binding -- TFunc of the lambda
  | EIf SourcePos TypeExpr Expr Expr Expr
  | EInfixOp SourcePos TypeExpr Expr T.Text Expr
  | EApplication SourcePos TypeExpr Expr [Expr] -- result type of the application
  | EIndex SourcePos TypeExpr Expr Expr -- expr[expr] — slice/map indexing
  | ESliceLit SourcePos TypeExpr [Expr] -- slice literal
  | EMapLit SourcePos TypeExpr -- empty map literal
  | ESequence SourcePos TypeExpr [Expr]
  | EVariadicSpread SourcePos TypeExpr Expr -- expr... unpacks []T into a variadic slot at a call site
  | EMatch SourcePos TypeExpr Expr [MatchArm] -- match expression
  deriving (Eq, Show)

-- Extract the SourcePos from any Expr node.
exprPos :: Expr -> SourcePos
exprPos (EVar p _ _) = p
exprPos (EIntLit p _ _) = p
exprPos (EFloatLit p _ _) = p
exprPos (EStrLit p _ _) = p
exprPos (EUnitLit p) = p
exprPos (ELet p _ _ _ _) = p
exprPos (ELambda p _ _) = p
exprPos (EIf p _ _ _ _) = p
exprPos (EInfixOp p _ _ _ _) = p
exprPos (EApplication p _ _ _) = p
exprPos (EIndex p _ _ _) = p
exprPos (ESliceLit p _ _) = p
exprPos (EMapLit p _) = p
exprPos (ESequence p _ _) = p
exprPos (EVariadicSpread p _ _) = p
exprPos (EMatch p _ _ _) = p

-- The type of the Expr node itself.
exprType :: Expr -> TypeExpr
exprType (EVar _ t _) = t
exprType (EIntLit _ t _) = t
exprType (EFloatLit _ t _) = t
exprType (EStrLit _ t _) = t
exprType (EUnitLit _) = UnitType
exprType (ELet _ t _ _ _) = t
exprType (ELambda _ t _) = t
exprType (EIf _ t _ _ _) = t
exprType (EInfixOp _ t _ _ _) = t
exprType (EApplication _ t _ _) = t
exprType (EIndex _ t _ _) = t
exprType (ESliceLit _ t _) = t
exprType (EMapLit _ t) = t
exprType (ESequence _ t _) = t
exprType (EVariadicSpread _ t _) = t
exprType (EMatch _ t _ _) = t

-- All TypeExprs directly attached to this node: the node's own type plus
-- any types embedded in Binding/Lambda (param types and return type).
-- Does not include types from child Expr nodes.
exprTypes :: Expr -> [TypeExpr]
exprTypes (ELambda _ t (Binding params retTy _)) = t : retTy : map paramType params
exprTypes (ELet _ t _ (Binding params retTy _) _) = t : retTy : map paramType params
exprTypes expr = [exprType expr]

data MatchArm = MatchArm SourcePos Pattern Expr
  deriving (Eq, Show)

data Pattern
  = PtWildcard -- _
  | PtVar Ident -- variable binding
  | PtIntLit IntLit -- integer literal
  | PtBoolLit Bool -- true, false
  | PtSliceEmpty -- []
  | PtCons Pattern Pattern -- x :: rest
  | PtTuple [Pattern] -- (a, b)
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
