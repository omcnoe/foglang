module Foglang.AST
  ( Ident (..),
    IntLit (..),
    FloatLit (..),
    StringLit (..),
    TypeSet (..),
    TypeExpr (..),
    pattern UnitType,
    ExprAnn (..),
    Param (..),
    Binding (..),
    Coercion (..),
    Expr (..),
    MatchArm (..),
    Pattern (..),
    PackageClause (..),
    ImportAlias (..),
    ImportDecl (..),
    Header (..),
    FogFile (..),
    exprAnn,
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
paramType (PTyped _ t) = t
paramType (PVariadic _ t) = t

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
      (PVariadic _ t : _) -> Just t
      _ -> Nothing
    isVariadic (PVariadic {}) = True
    isVariadic _ = False

-- Annotation carried by every Expr node. The parser populates pos and ty
-- (with TVars for unknown types); inference resolves ty; a post-inference
-- pass computes isStmt.
data ExprAnn = ExprAnn
  { pos    :: SourcePos
  , ty     :: TypeExpr
  , isStmt :: Bool
  } deriving (Eq, Show)

-- The kind of implicit coercion applied at a type boundary.
data Coercion
  = FuncVoidCoerce -- unit<->struct{} mismatch in function return types
  deriving (Eq, Show)

-- AST for expressions. Every constructor carries an ExprAnn.
-- The parser fills in placeholder types; inference resolves them.
data Expr
  = EVar ExprAnn Ident
  | EIntLit ExprAnn IntLit
  | EFloatLit ExprAnn FloatLit
  | EStrLit ExprAnn StringLit
  | EUnitLit ExprAnn
  | ELet ExprAnn Ident Binding (Maybe Expr) -- name, binding, optional continuation
  | ELambda ExprAnn Binding -- TFunc of the lambda
  | EIf ExprAnn Expr Expr Expr
  | EInfixOp ExprAnn Expr T.Text Expr
  | EApplication ExprAnn Expr [Expr] -- result type of the application
  | EIndex ExprAnn Expr Expr -- expr[expr] — slice/map indexing
  | ESliceLit ExprAnn [Expr] -- slice literal
  | EMapLit ExprAnn -- empty map literal
  | ESequence ExprAnn [Expr]
  | EVariadicSpread ExprAnn Expr -- expr... unpacks []T into a variadic slot at a call site
  | EMatch ExprAnn Expr [MatchArm] -- match expression
  | ECoerce ExprAnn Coercion Expr -- coerce inner expr to target type (ExprAnn ty)
  deriving (Eq, Show)

-- Extract the ExprAnn from any Expr node.
exprAnn :: Expr -> ExprAnn
exprAnn (EVar a _) = a
exprAnn (EIntLit a _) = a
exprAnn (EFloatLit a _) = a
exprAnn (EStrLit a _) = a
exprAnn (EUnitLit a) = a
exprAnn (ELet a _ _ _) = a
exprAnn (ELambda a _) = a
exprAnn (EIf a _ _ _) = a
exprAnn (EInfixOp a _ _ _) = a
exprAnn (EApplication a _ _) = a
exprAnn (EIndex a _ _) = a
exprAnn (ESliceLit a _) = a
exprAnn (EMapLit a) = a
exprAnn (ESequence a _) = a
exprAnn (EVariadicSpread a _) = a
exprAnn (EMatch a _ _) = a
exprAnn (ECoerce a _ _) = a

-- Extract the SourcePos from any Expr node.
exprPos :: Expr -> SourcePos
exprPos = pos . exprAnn

-- The type of the Expr node itself.
exprType :: Expr -> TypeExpr
exprType = ty . exprAnn

-- All TypeExprs directly attached to this node: the node's own type plus
-- any types embedded in Binding/Lambda (param types and return type).
-- Does not include types from child Expr nodes.
exprTypes :: Expr -> [TypeExpr]
exprTypes (ELambda ExprAnn{ty = t} (Binding params retTy _)) = t : retTy : map paramType params
exprTypes (ELet ExprAnn{ty = t} _ (Binding params retTy _) _) = t : retTy : map paramType params
exprTypes (ECoerce ExprAnn{ty = t} _ inner) = t : [exprType inner]
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
