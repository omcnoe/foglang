{-# LANGUAGE DeriveTraversable #-}

-- | Foglang type system
-- Type representations for various stages of compiler, and union-find for
-- inference.

module Foglang.Types
  ( TypeVar (..),
    UnresolvedType (..),
    ResolvedType (..),
    Terminal (..),
    Constraint (..),

    ForestRoot (..),
    ForestEntry (..),
    Subst (..),
    ResolutionSubst (..),
    substFind,
    substBindLink,
    substBindRoot,
    substResolve,

    isIntLitTerminal,
    isFloatLitTerminal,
    conDefault
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Text.Megaparsec.Pos (SourcePos)
import Foglang.AST (Ident (..))

-- ----------------------------------------------------------------------------
-- Core types

-- | Pointer into a unifying tree in the substitution forest.
newtype TypeVar = TypeVar Int
  deriving (Eq, Ord, Show)

-- | An unresolved type. Terminal or constraint always from source code type
-- annotation, or a fresh minted type var.
data UnresolvedType
  = UTyVar        SourcePos TypeVar
  | UTyTerminal   SourcePos (Terminal UnresolvedType)
  | UTyConstraint SourcePos (Constraint UnresolvedType)
  deriving (Show)
instance Eq UnresolvedType where -- Eq ignores SourcePos
  UTyVar        _ a == UTyVar        _ b = a == b
  UTyTerminal   _ a == UTyTerminal   _ b = a == b
  UTyConstraint _ a == UTyConstraint _ b = a == b
  _                 == _                 = False

-- | Complete, terminal type, ready for codegen. Fully materialized type tree.
newtype ResolvedType = ResolvedType (Terminal ResolvedType)
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Parameterised building blocks
--
-- Parameterized by `t` so "shape" is reusable across Parse, Inference, Codegen.
-- `t` will be an actual type, or pointer into union-find forest.

-- | Terminal type: a fully-chosen type constructor.
data Terminal t
  = TNamed       Ident
  | TSlice       t
  | TMap         t t
  | TFunc        [t] (Maybe t) t
  | TUnit        -- ^ `()`
  | TEmptyStruct -- ^ `struct{}`
  | TAny         -- ^ any/interface{}
  | TOpaque      -- ^ Fog type-system gap! Bypass fog type checking.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Narrowing constraint.
-- Will narrow to a `Terminal` if unification has enough information,
-- or can narrow to its default `Terminal` value at resolve time.
data Constraint t
  = ConIntLit
  | ConFloatLit
  | ConIndexable t t
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- ----------------------------------------------------------------------------
-- Substitution: union-find forest

-- | Root payload of a substitution forest unifying tree.
data ForestRoot
  = FRTerminal   (Terminal TypeVar)
  | FRConstraint (Constraint TypeVar)
  deriving (Show)

-- | Entry in the substitution forest.
data ForestEntry
  = FELink SourcePos TypeVar
  | FERoot SourcePos ForestRoot
  deriving (Show)

-- | Inference-phase substitution state: union-find forest plus fresh TypeVar counter.
data Subst = Subst
  { sForest      :: IntMap.IntMap ForestEntry,
    sNextTypeVar :: Int }
  deriving (Show)

-- | Resolution phase substitution state: forest including memoized resolved type values.
data ResolutionSubst = ResolutionSubst
  { rForest :: IntMap.IntMap (Either ForestEntry ResolvedType) }
  deriving (Show)

-- | Walk the substitution forest from `curId` to its tree root.
-- Path-compresses on the way back: every link visited is rewritten to
-- point directly at the root (keeping its original bind `SourcePos`).
--
-- Returns (rootId, rootValue, updatedSubst):
--   * `rootId`       - id of the root reached.
--   * `rootValue`    - `Nothing` if the root is unbound (no entry in the map);
--                      `Just (pos, r)` if bound, with `pos` the bind site.
--   * `updatedSubst` - subst with path compression applied.
substFind :: TypeVar -> Subst -> (TypeVar, Maybe (SourcePos, ForestRoot), Subst)
substFind curId@(TypeVar curIdInt) subst =
  case IntMap.lookup curIdInt subst.sForest of
    Nothing ->
      -- curId has no entry; it is itself an unbound root.
      (curId, Nothing, subst)
    Just (FERoot pos root) ->
      -- cur is the tree root.
      (curId, Just (pos, root), subst)
    Just (FELink linkPos nxtPtr) ->
      -- cur is a link in the chain; chase to the root, then path
      -- compress by rewriting cur to point directly at the root but
      -- preserving original SourcePos.
      let (rootId, mRoot, subst') = substFind nxtPtr subst
          compressed = subst' { sForest = IntMap.insert curIdInt (FELink linkPos rootId) subst'.sForest }
      in (rootId, mRoot, compressed)
      -- net effect: `A -> B -> C -> ROOT` becomes `A -> ROOT; B -> ROOT; C -> ROOT`.

-- | Link `src` to `tgt`, making `src` a non-root entry that redirects to
-- `tgt`'s tree. Self-link (`src == tgt`) is a no-op.
substBindLink :: SourcePos -> TypeVar -> TypeVar -> Subst -> Subst
substBindLink _   (TypeVar src) (TypeVar tgt) subst | src == tgt = subst
substBindLink pos (TypeVar src) (TypeVar tgt) subst =
  subst { sForest = IntMap.insert src (FELink pos (TypeVar tgt)) subst.sForest }

-- | Bind `rootId` as a root with the given `ForestRoot` payload.
substBindRoot :: SourcePos -> TypeVar -> ForestRoot -> Subst -> Subst
substBindRoot pos (TypeVar rootId) root subst =
  subst { sForest = IntMap.insert rootId (FERoot pos root) subst.sForest }

-- | Walk the resolution forest from the given `TypeVar` to its root, resolving
-- and memoizing intermediate roots on the way back up (path-compression-style).
-- Returns `Nothing` if a root on the path is unbound or otherwise unresolvable.
-- TODO impl (later)
-- TODO needs to cycle detect, need expanded return type?
substResolve :: TypeVar -> ResolutionSubst -> (Maybe ResolvedType, ResolutionSubst)
substResolve = undefined

-- ----------------------------------------------------------------------------
-- Constraint defaulting

-- | Is this terminal a valid narrowing target for `ConIntLit`?
isIntLitTerminal :: Terminal t -> Bool
isIntLitTerminal (TNamed i) = i `Set.member` intLitIdents
  where
    intLitIdents = Set.fromList $ map Ident
      ["int", "int8", "int16", "int32", "int64",
      "uint", "uint8", "uint16", "uint32", "uint64",
      "uintptr", "byte", "rune"]
isIntLitTerminal _ = False

-- | Is this terminal a valid narrowing target for `ConFloatLit`?
isFloatLitTerminal :: Terminal t -> Bool
isFloatLitTerminal (TNamed i) = i `Set.member` floatLitIdents
  where
    floatLitIdents = Set.fromList $ map Ident ["float32", "float64"]
isFloatLitTerminal _ = False

-- | Default a constraint to a concrete ResolvedType. Nothing if ambiguous.
conDefault :: Constraint ResolvedType -> Maybe ResolvedType
conDefault con = case con of
  ConIntLit        -> Just (ResolvedType (TNamed (Ident "int")))
  ConFloatLit      -> Just (ResolvedType (TNamed (Ident "float64")))
  ConIndexable k v -> case k of
    ResolvedType t | isIntLitTerminal t -> Just (ResolvedType (TSlice v))
    ResolvedType (TNamed _) -> Just (ResolvedType (TMap k v))
    ResolvedType TOpaque    -> Just (ResolvedType (TMap k v))
    ResolvedType TAny       -> Just (ResolvedType (TMap k v))
    _                       -> Nothing
