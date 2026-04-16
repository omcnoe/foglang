-- | Foglang type system
-- Type representations, and the union-find substitution used by inference.
--
-- Three type representations flow through the pipeline:
--   * `UnresolvedType` - parser output/inference input. Either a reference
--     into the substitution forest (`UTyVar`) or a WHNF type
--     (`UTyHeadResolved`).
--   * `HeadResolvedType` - inference internal, weak head normal form of a
--     type or type constraint. Head resolved, but may contain `UnresolvedType`
--     children. Used inside union find substitution forest roots.
--   * `ResolvedType` - inference output/codegen input. Fully concrete type
--     that can be mapped 1:1 to Go source output.
--
-- Type inference's responsibility is to transform every `UnresolvedType`
-- into a `ResolvedType` using union-find.
--
-- Why union find?
-- Original design had simple `Map Int (Type/TypeVar)`, and eagerly rewrote
-- the AST until all TypeVar were removed. The union-find approach has two
-- key advantages:
--   1. Lazy substitution forest, building up unifying trees instead of
--      repeatedly rewriting the AST. Only a single AST rewrite at the end,
--      once all types are resolved.
--   2. Path compression on a substitution tree lookups - traversing the
--      tree anyway, cheap to rewrite it at the same time.
--
-- Each tree in the forest represents all type vars that must unify together
-- to pass type checking.
-- Parallel trees can exist with same value stored in their root nodes without
-- being merged, if type usages don't intersect.
--
-- During inference root holds WHNF of the type (`HeadResolvedType`).
-- At resolution, the whole tree collapses to one ResolvedType value shared
-- by every member.
--
-- WHNF? Only "head" of the type is resolved eg. `HRTySlice` holding an
-- `UnresolvedType`. These unresolved parts are pointers to nodes/roots of
-- other trees.
--
-- TODO(perf): consider union-by-rank union find.

module Foglang.Types
  ( TypeVarPtr (..),
    UnresolvedType (..),
    HeadResolvedType (..),
    ResolvedType (..),

    intLitTypes,
    floatLitTypes,
    intLitDefault,
    floatLitDefault,

    Subst, -- intentionally not exporting ctor, must interact through helpers
    substEmpty,
    substFind,
    substBind,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Text.Megaparsec.Pos (SourcePos)
import Foglang.AST (Ident (..))

-- ----------------------------------------------------------------------------
-- Type representations

newtype TypeVarPtr = TypeVarPtr Int
  deriving (Eq, Show)

-- | Parse/inference-input type annotation on AST nodes.
-- Carries `SourcePos` of where it was minted.
data UnresolvedType
  = UTyVar          SourcePos TypeVarPtr       -- ^ Pointer into the substitution forest.
  | UTyHeadResolved SourcePos HeadResolvedType -- ^ Head resolved, may contain `UTyVar` children
  deriving (Show)
instance Eq UnresolvedType where -- Eq ignores SourcePos
  UTyVar          _ tv1 == UTyVar          _ tv2 = tv1 == tv2
  UTyHeadResolved _ rh1 == UTyHeadResolved _ rh2 = rh1 == rh2
  _                     == _                     = False

-- | A WHNF type, used inside type inference as union find substitution forest roots.
-- Head is resolved, but children may still be `UTyVar`.
-- Either a terminal, or a constraint that may be narrowed later.
data HeadResolvedType
  = HRTerminal   TerminalType
  | HRConstraint ConstraintType

-- Head resolved terminal type: unresolved children, can't narrow
data TerminalType
  = TyNamed       Ident
  | TySlice       UnresolvedType
  | TyMap         UnresolvedType UnresolvedType
  | TyFunc        [UnresolvedType] (Maybe UnresolvedType) UnresolvedType
  | TyUnit        -- ^ `()`
  | TyEmptyStruct -- ^ `struct{}`
  | TyAny         -- ^ any/interface{}
  | TyOpaque      -- ^ Fog type-system gap! Bypass fog type checking.
  deriving (Eq, Show)

-- Head resolved constraint type: unresolved children, can narrow
data ConstraintType
  = ConIntLit      -- ^ narrows to member of `intLitTypes`
  | ConFloatLit    -- ^ narrows to member of `floatLitTypes`
  | ConIndexable   UnresolvedType UnresolvedType -- ^ narrows to indexable instance (slice/map/string) when key shape is concrete
  deriving (Eq, Show)

-- | Complete, concrete type, ready for codegen.
data ResolvedType
  = RTyNamed       Ident
  | RTySlice       ResolvedType
  | RTyMap         ResolvedType ResolvedType
  | RTyFunc        [ResolvedType] (Maybe ResolvedType) ResolvedType
  | RTyUnit
  | RTyEmptyStruct
  | RTyAny
  | RTyOpaque
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Numeric literals

-- | Named types an integer literal is allowed to unify with.
intLitTypes :: Set.Set Ident
intLitTypes = Set.fromList $ map Ident
  ["int", "int8", "int16", "int32", "int64",
   "uint", "uint8", "uint16", "uint32", "uint64",
   "uintptr", "byte", "rune"]

-- | Named types a float literal is allowed to unify with.
floatLitTypes :: Set.Set Ident
floatLitTypes = Set.fromList $ map Ident ["float32", "float64"]

-- | Default ResolvedType an unresolved `ConIntLit` takes
intLitDefault :: Ident
intLitDefault = Ident "int"

-- | Default ResolvedType an unresolved `ConFloatLit` takes
floatLitDefault :: Ident
floatLitDefault = Ident "float64"

-- ----------------------------------------------------------------------------
-- Substitution: union find forest

-- | The substitution. Newtype over `IntMap` so that read/write must happen via
-- helpers.
newtype Subst = Subst { unSubst :: IntMap.IntMap UnresolvedType }
  deriving (Show)

substEmpty :: Subst
substEmpty = Subst { unSubst = IntMap.empty }

-- | Walk the substitution forest from `curPtr` to its tree root.
-- Path-compresses on the way back: every link visited is rewritten to
-- point directly at the root.
--
-- Returns (rootPtr, rootValue, updatedSubst):
--   * `rootPtr`      - id of the root reached.
--   * `rootValue`    - `Nothing` if the root is unbound (no entry in the map);
--                      `Just hr` if the root is bound to that head-resolved type.
--   * `updatedSubst` - updated subst with path compression applied.
substFind :: TypeVarPtr -> Subst -> (TypeVarPtr, Maybe HeadResolvedType, Subst)
substFind curPtr@(TypeVarPtr curPtrInt) subst =
  case IntMap.lookup curPtrInt (unSubst subst) of
    Nothing ->
      -- curPtr has no entry (dangling); it is itself an (unbound) tree root.
      (curPtr, Nothing, subst)
    Just (UTyHeadResolved _ hr) ->
      -- curPtr is the tree root, bound to hr.
      (curPtr, Just hr, subst)
    Just (UTyVar linkPos nxtPtr) ->
      -- curPtr is a link in chain, chase rest of chain to root, then path compress by rewriting curPtr to point directly at root.
      let (rootPtr, mRootHr, Subst subst') = substFind nxtPtr subst -- recursively path compresses along whole chain
      in (rootPtr, mRootHr, Subst (IntMap.insert curPtrInt (UTyVar linkPos rootPtr) subst'))
      -- end result: `A -> B -> C -> ROOT` becomes `A -> ROOT; B -> ROOT; C -> ROOT}

-- Bind the TypeVarPtr n to an UnresolvedType. Forming either a UTyVar link or UTyHeadResolved head in the substitution forest.
substBind :: TypeVarPtr -> UnresolvedType -> Subst -> Subst
substBind (TypeVarPtr n) (UTyVar _ (TypeVarPtr m)) s | n == m = s -- self link is no-op
substBind (TypeVarPtr n) t (Subst s) = Subst $ IntMap.insert n t s
