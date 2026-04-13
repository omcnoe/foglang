-- | Union-find substitution for fog's type inference.
--
-- Each type variable is either unbound, a forwarding pointer (Link) to
-- another variable, or bound to a Root carrying either a concrete shape or
-- a constraint. Looking up a variable's meaning means chasing the Link
-- chain to a Root (or to an unbound variable).
--
-- The classical TypeExpr-as-map-value approach stored shapes that could
-- themselves contain type variables, forming a directed graph that made
-- naive applySubst O(chain-depth) per occurrence: quadratic overall on
-- deeply-nested chains (e.g. indexable-keyed-by-indexable). Union-find
-- plus path compression makes this amortized near-linear.
--
-- TODO(perf): consider union-by-rank on `Root` nodes to get the
-- inverse-Ackermann bound. Currently only path compression is implemented,
-- giving amortized O(log n) - fine for any fog program anyone would write
-- by hand, but revisit if a benchmark regresses.
module Foglang.Subst
  ( -- Constraints (only ever appear in Roots, never on tree nodes)
    Constraint (..),
    -- Union-find forest
    RootContent (..),
    UFEntry (..),
    Subst,
    emptySubst,
    -- Queries
    FindResult (..),
    find,
    -- Mutations (only three legitimate bind shapes - raw `bind` is
    -- intentionally module-private so the Link / Root distinction can't
    -- be bypassed accidentally).
    bindLink,
    bindConcrete,
    bindConstraint,
  ) where

import Data.IntMap.Strict qualified as IntMap
import Foglang.AST (ConcreteShape, TypeExpr, TypeSet)

-- | A constraint: partial information about a type that is not yet
-- concrete. Constraints are meaningful only when associated with an
-- equivalence-class representative (a Root). They cannot appear on Expr
-- tree nodes.
data Constraint
  = CNumeric   !TypeSet
  -- ^ Numeric literal: must be some member of a numeric type set (tsInt or
  -- tsFloat). Defaults to the set's default (int / float64) when resolution
  -- forces a choice.
  | CIndexable !TypeExpr !TypeExpr
  -- ^ Indexable: the variable's type must support `x[k]` with the key and
  -- value TypeExpr given. Resolves to TSlice / TMap / string / ... when
  -- the key shape is concrete at resolution time.
  deriving (Eq, Show)

-- | Contents of an equivalence-class representative. Either a fully concrete
-- shape (narrowed to its final structure) or a constraint (still partial).
data RootContent
  = RConcrete   !ConcreteShape
  | RConstraint !Constraint
  deriving (Eq, Show)

-- | A substitution map entry. A TypeVar ID that doesn't appear in the map
-- is unbound (still freshly minted, not yet constrained).
data UFEntry
  = Link !Int          -- ^ forwarding pointer - ask the referenced ID
  | Root !RootContent  -- ^ this ID is a representative; its meaning is here
  deriving (Eq, Show)

-- | The substitution. A newtype over IntMap so that all mutation happens
-- through the module's API; there's no raw-map escape hatch for callers to
-- bypass path compression.
newtype Subst = Subst { unSubst :: IntMap.IntMap UFEntry }
  deriving (Eq, Show)

emptySubst :: Subst
emptySubst = Subst IntMap.empty

-- | Result of resolving a TypeVar ID.
--
-- `FoundUnbound n`: the chain terminated at an ID with no entry in the
-- map. That ID is the representative of the equivalence class.
--
-- `FoundRoot n rc`: the chain terminated at a Root with the given content.
-- `n` is the representative's ID (useful when linking another variable to
-- this one).
data FindResult
  = FoundUnbound !Int
  | FoundRoot    !Int !RootContent
  deriving (Eq, Show)

-- | Chase the chain starting at `n`, compressing as we unwind. Every
-- intermediate Link visited is rewritten to point directly at the final
-- representative, flattening the chain for future lookups.
find :: Int -> Subst -> (FindResult, Subst)
find n (Subst m0) = case IntMap.lookup n m0 of
  Nothing         -> (FoundUnbound n, Subst m0)
  Just (Root rc)  -> (FoundRoot n rc, Subst m0)
  Just (Link p)   ->
    let (result, Subst m1) = find p (Subst m0)
        finalId = resultId result
        -- Skip rewriting when we'd create n -> Link n, a self-loop.
        m2 = if finalId == n
               then m1
               else IntMap.insert n (Link finalId) m1
    in (result, Subst m2)

-- | The representative ID of a find result.
resultId :: FindResult -> Int
resultId (FoundUnbound i)  = i
resultId (FoundRoot    i _) = i

-- | Internal: set an entry directly. Callers use one of the three
-- `bindLink` / `bindConcrete` / `bindConstraint` helpers below.
bind :: Int -> UFEntry -> Subst -> Subst
bind n entry (Subst m) = Subst (IntMap.insert n entry m)

-- | Link variable `n` to variable `m`. `n` becomes an alias for `m`'s
-- equivalence class. Skipped when `n == m` (a self-link would be useless).
bindLink :: Int -> Int -> Subst -> Subst
bindLink n m s
  | n == m    = s
  | otherwise = bind n (Link m) s

-- | Bind `n` to a concrete shape. `n` becomes a Root holding the shape;
-- any variable linking to `n` resolves to this shape.
bindConcrete :: Int -> ConcreteShape -> Subst -> Subst
bindConcrete n c = bind n (Root (RConcrete c))

-- | Bind `n` to a constraint. `n` becomes a Root holding the constraint;
-- any variable linking to `n` resolves to this constraint.
bindConstraint :: Int -> Constraint -> Subst -> Subst
bindConstraint n c = bind n (Root (RConstraint c))
