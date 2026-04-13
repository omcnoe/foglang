{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}

module Foglang.AST
  ( Ident (..),
    IntLit (..),
    FloatLit (..),
    StringLit (..),
    TypeSet (..),
    tsInt,
    tsFloat,
    -- Inference-time type representation
    ConcreteShape (..),
    TypeExpr (..),
    tvarPos,
    pattern UnitTypeExpr,
    pattern OpaqueTypeExpr,
    isUnitLikeShape,
    isWildcardShape,
    -- Ground type representation (codegen input)
    GroundType (..),
    pattern UnitType,
    pattern OpaqueType,
    isUnitLike,
    isWildcard,
    -- Expr and friends, parametric over type payload
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
    -- Accessors
    exprAnn,
    exprPos,
    exprType,
    paramType,
    bindingType,
    bindingTypeExpr,
  )
where

import Data.String (IsString (..))
import Data.Set qualified as Set
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
data TypeSet = TypeSet
  { tsMembers :: Set.Set Ident,
    tsDefault :: Ident
  }
  deriving (Eq, Show)

tsInt :: TypeSet
tsInt = TypeSet
  { tsMembers = Set.fromList $ map Ident
      ["int", "int8", "int16", "int32", "int64",
       "uint", "uint8", "uint16", "uint32", "uint64",
       "uintptr", "byte", "rune"],
    tsDefault = Ident "int"
  }

tsFloat :: TypeSet
tsFloat = TypeSet
  { tsMembers = Set.fromList $ map Ident ["float32", "float64"],
    tsDefault = Ident "float64"
  }

-- | A concrete structural type: never a bare variable at its root. Children
-- (and grandchildren) may still be TypeExpr (which can be variables).
-- Used on Expr annotations during inference and as the RConcrete payload in
-- the substitution's union-find forest. Constraints (numeric / indexable)
-- are NOT shapes - they live only in substitution Roots, never on tree nodes.
data ConcreteShape
  = CNamed Ident
  | CSlice TypeExpr
  | CMap   TypeExpr TypeExpr
  | CFunc  [TypeExpr] (Maybe TypeExpr) TypeExpr
  deriving (Eq, Show)

-- | Inference-time type annotation on Expr nodes.
-- Either a pointer into the substitution (resolved via Foglang.Subst.find)
-- or a fully concrete shape. Absence of variable variants carrying
-- constraints is deliberate: constraints live in Subst Roots so that the
-- invariant "a tree annotation is a resolvable reference or a shape" is
-- enforced by the type system rather than by discipline.
--
-- The SourcePos on TVar is the position where the variable was minted
-- (parser or inference). Used by resolveType to report CannotInferType at
-- the exact hole. The manual Eq instance ignores this position so that
-- structural comparison in tests is position-independent.
data TypeExpr
  = TVar   SourcePos Int
  | TShape ConcreteShape
  deriving (Show)
instance Eq TypeExpr where -- Eq ignores SourcePos
  TVar _ n1  == TVar _ n2  = n1 == n2
  TShape c1  == TShape c2  = c1 == c2
  _          == _          = False

-- | Position where a TVar was minted. Only defined for TVar; callers that
-- need a fallback for TShape should supply their own.
tvarPos :: TypeExpr -> Maybe SourcePos
tvarPos (TVar p _) = Just p
tvarPos (TShape _) = Nothing

pattern UnitTypeExpr :: TypeExpr
pattern UnitTypeExpr = TShape (CNamed (Ident "()"))

-- | The opaque-any sentinel: fog's escape hatch for types it can't
-- express (Go builtins, qualified imports, tuple destructuring, etc.).
-- Unifies freely; defaults for unconstrained variables resolve to this.
pattern OpaqueTypeExpr :: TypeExpr
pattern OpaqueTypeExpr = TShape (CNamed (Ident "opaque"))

-- | Is a concrete shape unit-like (() or struct{})?
isUnitLikeShape :: ConcreteShape -> Bool
isUnitLikeShape (CNamed (Ident "()"))       = True
isUnitLikeShape (CNamed (Ident "struct{}")) = True
isUnitLikeShape _                           = False

-- | Wildcard shapes unify freely in inference (`opaque`, `any`).
isWildcardShape :: ConcreteShape -> Bool
isWildcardShape (CNamed (Ident "opaque")) = True
isWildcardShape (CNamed (Ident "any"))    = True
isWildcardShape _                         = False

-- | Ground type: codegen's input. No variables, no constraints.
data GroundType
  = TyNamed Ident
  | TySlice GroundType
  | TyMap   GroundType GroundType
  | TyFunc  [GroundType] (Maybe GroundType) GroundType
  deriving (Eq, Show)

pattern UnitType :: GroundType
pattern UnitType = TyNamed (Ident "()")

-- | Ground-type counterpart to `OpaqueTypeExpr`.
pattern OpaqueType :: GroundType
pattern OpaqueType = TyNamed (Ident "opaque")

isUnitLike :: GroundType -> Bool
isUnitLike (TyNamed (Ident "()"))       = True
isUnitLike (TyNamed (Ident "struct{}")) = True
isUnitLike _                            = False

isWildcard :: GroundType -> Bool
isWildcard (TyNamed (Ident "opaque")) = True
isWildcard (TyNamed (Ident "any"))    = True
isWildcard _                          = False

-- | Parameter. Parametric over the type payload: during inference it's
-- TypeExpr; after resolution it's GroundType.
data Param t
  = PUnit
  | PTyped    Ident t
  | PVariadic Ident t
  deriving (Eq, Show, Functor, Foldable, Traversable)

paramType :: Param GroundType -> GroundType
paramType PUnit            = UnitType
paramType (PTyped _ t)     = t
paramType (PVariadic _ t)  = t

-- | Binding: parameters, declared return type, RHS expression.
data Binding t = Binding [Param t] t (Expr t)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Expression annotation.
data ExprAnn t = ExprAnn
  { pos    :: SourcePos,
    ty     :: t,
    isStmt :: Bool
  } deriving (Show, Functor, Foldable, Traversable)
instance Eq t => Eq (ExprAnn t) where -- Eq ignores SourcePos
  ExprAnn _ t1 s1 == ExprAnn _ t2 s2 = (t1, s1) == (t2, s2)

-- | Implicit-coercion kind applied at a type boundary.
data Coercion
  = FuncVoidCoerce -- unit<->struct{} mismatch in function return types
  deriving (Eq, Show)

-- | Expression AST. Parametric over the type payload carried by annotations
-- and nested Bindings. Parser produces `Expr TypeExpr`; inference resolves
-- it to `Expr GroundType` before codegen consumes it.
data Expr t
  = EVar            (ExprAnn t) Ident
  | EIntLit         (ExprAnn t) IntLit
  | EFloatLit       (ExprAnn t) FloatLit
  | EStrLit         (ExprAnn t) StringLit
  | EUnitLit        (ExprAnn t)
  | ELet            (ExprAnn t) Ident (Binding t) (Maybe (Expr t))
  | ELambda         (ExprAnn t) (Binding t)
  | EIf             (ExprAnn t) (Expr t) (Expr t) (Expr t)
  | EInfixOp        (ExprAnn t) (Expr t) T.Text (Expr t)
  | EApplication    (ExprAnn t) (Expr t) [Expr t]
  | EIndex          (ExprAnn t) (Expr t) (Expr t)
  | ESliceLit       (ExprAnn t) [Expr t]
  | EMapLit         (ExprAnn t)
  | ESequence       (ExprAnn t) [Expr t]
  | EVariadicSpread (ExprAnn t) (Expr t)
  | EMatch          (ExprAnn t) (Expr t) [MatchArm t]
  | ECoerce         (ExprAnn t) Coercion (Expr t)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data MatchArm t = MatchArm SourcePos Pattern (Expr t)
  deriving (Show, Functor, Foldable, Traversable)
instance Eq t => Eq (MatchArm t) where -- Eq ignores SourcePos
  MatchArm _ p1 b1 == MatchArm _ p2 b2 = (p1, b1) == (p2, b2)

data Pattern
  = PtWildcard
  | PtVar      Ident
  | PtIntLit   IntLit
  | PtStrLit   StringLit
  | PtBoolLit  Bool
  | PtSliceEmpty
  | PtCons     Pattern Pattern
  | PtTuple    [Pattern]
  deriving (Eq, Show)

exprAnn :: Expr t -> ExprAnn t
exprAnn (EVar a _)            = a
exprAnn (EIntLit a _)         = a
exprAnn (EFloatLit a _)       = a
exprAnn (EStrLit a _)         = a
exprAnn (EUnitLit a)          = a
exprAnn (ELet a _ _ _)        = a
exprAnn (ELambda a _)         = a
exprAnn (EIf a _ _ _)         = a
exprAnn (EInfixOp a _ _ _)    = a
exprAnn (EApplication a _ _)  = a
exprAnn (EIndex a _ _)        = a
exprAnn (ESliceLit a _)       = a
exprAnn (EMapLit a)           = a
exprAnn (ESequence a _)       = a
exprAnn (EVariadicSpread a _) = a
exprAnn (EMatch a _ _)        = a
exprAnn (ECoerce a _ _)       = a

exprPos :: Expr t -> SourcePos
exprPos = pos . exprAnn

exprType :: Expr t -> t
exprType = ty . exprAnn

-- | Build the full ground type for a binding given its params and declared
-- return type. Value bindings (no params) return retTy directly.
bindingType :: [Param GroundType] -> GroundType -> GroundType
bindingType [] retTy = retTy
bindingType ps retTy = TyFunc fixedTys mVarTy retTy
  where
    fixedTys = [paramType p | p <- ps, not (isVariadic p)]
    mVarTy = case reverse ps of
      (PVariadic _ t : _) -> Just t
      _                   -> Nothing
    isVariadic (PVariadic {}) = True
    isVariadic _              = False

-- | Like bindingType but on the inference-time TypeExpr representation.
-- Used by inference to construct the function-type annotation for a binding
-- before resolution.
bindingTypeExpr :: [Param TypeExpr] -> TypeExpr -> TypeExpr
bindingTypeExpr [] retTy = retTy
bindingTypeExpr ps retTy = TShape (CFunc fixedTys mVarTy retTy)
  where
    fixedTys = [paramTypeExpr p | p <- ps, not (isVariadic p)]
    mVarTy = case reverse ps of
      (PVariadic _ t : _) -> Just t
      _                   -> Nothing
    isVariadic (PVariadic {}) = True
    isVariadic _              = False
    paramTypeExpr PUnit           = UnitTypeExpr
    paramTypeExpr (PTyped _ t)    = t
    paramTypeExpr (PVariadic _ t) = t

newtype PackageClause = PackageClause Ident
  deriving (Eq, Show)

data ImportAlias
  = Default
  | Alias Ident
  | Dot
  | Blank
  deriving (Eq, Show)

data ImportDecl = ImportDecl ImportAlias T.Text
  deriving (Eq, Show)

data Header = Header PackageClause [ImportDecl]
  deriving (Eq, Show)

-- | Parsed fog file, parametric over the type payload.
data FogFile t = FogFile Header (Expr t)
  deriving (Show, Functor, Foldable, Traversable)
