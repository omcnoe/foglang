# Type Inference

## Overview

Foglang uses constraint-based type inference with union-find substitution. Type annotations are optional everywhere — on local bindings, function parameters, return types, and top-level definitions. When a type is omitted, the compiler assigns a fresh type variable and resolves it from context.

```
let add x y = x + y + 1       // inferred: int -> int => int
let greeting = "hello"        // inferred: string
let double (x : int) = x * 2  // param annotated, return type inferred
```

When the user provides an annotation, the compiler uses it as a constraint. Annotations are never ignored — they anchor inference and produce better error messages.

Inference is monomorphic — there is no let-polymorphism or generalisation. Each binding gets a single concrete type. Polymorphism will be introduced later alongside explicit generics.

## Source positions

Every AST node carries a source position (row and column). This is required for meaningful error messages from type inference — "type mismatch: int vs string at line 12, column 5".

## Type representation

The type representation is layered into three data types:

### `TypeExpr` — surface / inference-time

At parse time and during inference, every expression's `ty` slot holds a `TypeExpr`:

```haskell
data TypeExpr
  = TVar !Int                -- variable; resolve via Subst.find
  | TShape !ConcreteShape    -- fully concrete structural type
```

`ConcreteShape` is structural-only — it cannot be a bare variable at its root, though its children may still be `TypeExpr`s (which can in turn be variables):

```haskell
data ConcreteShape
  = CNamed !Ident
  | CSlice !TypeExpr
  | CMap   !TypeExpr !TypeExpr
  | CFunc  ![TypeExpr] !(Maybe TypeExpr) !TypeExpr
```

Crucially, neither `TypeExpr` nor `ConcreteShape` carries any notion of a constraint. Constraints live exclusively in the substitution (see below). This structural separation is enforced by the type system — there is no way to write a constrained variable at an `Expr` annotation site.

### `Type` — ground / codegen input

After inference, every `Expr` node's `ty` is a fully ground `Type`:

```haskell
data Type
  = TyNamed !Ident
  | TySlice !Type
  | TyMap   !Type !Type
  | TyFunc  ![Type] !(Maybe Type) !Type
```

`Type` has no variable constructor. An `Expr Type` (the codegen input) cannot possibly contain an unresolved variable — the type system prevents it.

### `Subst` — union-find substitution

The substitution is a union-find forest:

```haskell
data UFEntry
  = Link !Int                -- forward to another variable
  | Root !RootContent        -- representative of an equivalence class

data RootContent
  = RConcrete   !ConcreteShape
  | RConstraint !Constraint

data Constraint
  = CNumeric   !TypeSet            -- numeric literal (int-ish or float-ish)
  | CIndexable !TypeExpr !TypeExpr -- must support `x[k]` with key/value
```

A TypeVar ID absent from the map is unbound. Present with a `Link` points to another ID. Present with a `Root` is the representative of its equivalence class — either bound to a concrete shape, or carrying a constraint that hasn't been narrowed to a shape yet.

`find` walks the Link chain to the representative, with path compression on each traversal (intermediate nodes get rewritten to point directly at the root). This keeps lookup amortised near-linear across the whole inference — the map-based substitution's O(N²) failure mode on deeply-nested indexable chains is gone.

### `Expr` — parametric over type payload

`Expr`, `Binding`, `ExprAnn`, `MatchArm`, `Param`, and `FogFile` are all parametric over the type payload:

```haskell
data ExprAnn t = ExprAnn { pos :: SourcePos, ty :: !t, isStmt :: !Bool }
data Expr    t = EVar (ExprAnn t) Ident | ...
```

Parser produces `Expr TypeExpr`; inference returns `Expr Type`; codegen consumes `Expr Type`. The pipeline's phase transition is the signature of `inferAndResolve :: (Expr TypeExpr, ParserState) -> Either [InferError] (Expr Type)`.

## Parameter syntax

Parameters can be written in three forms:

```
let f (x : int) (y : int) = x + y    // fully annotated
let f (x) (y) = x + y                // parenthesised, type omitted
let f x y = x + y                    // bare identifier
```

All three produce the same AST structure. Unannotated params get a fresh `TVar` in the type slot.

### Bare parameter parsing

After parsing `let name`, the parser greedily consumes bare identifiers and parenthesized params. Then it dispatches on the next token:

- `=>` — function with explicit return type: `let f x y => int = x + y`
- `=` with params collected — function with inferred return type: `let f x y = x + y`
- `=` with no params collected — value binding with inferred type: `let x = 5`
- `:` — value binding with explicit type (no params): `let x : int = 5`

The `=` sign is the unambiguous boundary between params and body. The presence or absence of collected params before `=` distinguishes functions from value bindings.

Bare and annotated params can be mixed: `let f x (y : int) = x + y`.

## Inference pipeline

Inference has two phases, implemented in `Foglang.Inference.inferAndResolve`:

### Phase 1: Constraint generation + unification

Walk the `Expr TypeExpr` tree. At every node, call `unifyM` to unify expected vs inferred types. `unifyM` modifies the shared `Subst` in the `Infer` monad state.

The parser minted a fresh `TVar` for each literal; `inferExpr` attaches constraints when it first visits the node:

- `EIntLit a _`   — binds the annotation's TVar to `Root (RConstraint (CNumeric tsInt))`.
- `EFloatLit a _` — binds to `Root (RConstraint (CNumeric tsFloat))`.
- `EStrLit`       — parser already assigns `TShape (CNamed "string")` directly (only one string type).
- `EUnitLit`      — parser assigns `UnitTypeExpr = TShape (CNamed "()")`.
- `EIndex`        — mints a fresh TVar bound to `Root (RConstraint (CIndexable keyTy valTy))`, then unifies the container expression against that TVar.
- `EVar`          — looks up the environment's type and unifies.

Unification dispatches on the current `TypeView` of each side (shape / unbound var / constrained var):

| LHS | RHS | Action |
|---|---|---|
| wildcard shape | anything | succeed, no binding |
| `VVarUnbound a` | `VVarUnbound b` | `bindLink a b` |
| `VVarUnbound a` | `VVarConstraint b c` | `bindLink a b` (preserve the constraint on b) |
| `VVarUnbound a` | `VShape c` | occurs-check, then `bindConcrete a c` |
| `VVarConstraint a k1` | `VVarConstraint b k2` | unify constraints, `bindLink b a` |
| `VVarConstraint a k` | `VShape c` | promote: `unifyConstraintShape` — if c satisfies k, `bindConcrete a c`; else error |
| `VShape c1` | `VShape c2` | head-to-head `unifyShapes`, recursing into children |

Constraint–shape promotion:
- `CNumeric ts` vs `CNamed name` — if `name ∈ ts`, bind; else `TypeMismatch`.
- `CIndexable k v` vs `CSlice elem` — unify `k ~ int`, `v ~ elem`, bind to `CSlice`.
- `CIndexable k v` vs `CMap mk mv` — unify `k ~ mk`, `v ~ mv`, bind to `CMap`.
- `CIndexable k v` vs `CNamed "string"` — unify `k ~ int`, `v ~ byte`, bind to `CNamed "string"`.
- `CIndexable` vs anything else — `NotAnIndexable`.

#### Occurs check

Before binding `TVar n` to a shape, walk the shape (through the substitution, via `find`) to see whether `n` reappears transitively. If it does, the binding would create a cyclic type, so raise `InfiniteType`. This does not affect user-defined recursive types (future ADTs), since those recurse through named types, not through TypeVars.

#### Operators

| Operator | Constraints |
|---|---|
| `x + y`, `x - y`, `x * y`, `x / y`, `x % y` | `x ~ y`, result ~ type of `x` |
| `x == y`, `!=`, `<`, `>`, `<=`, `>=` | `x ~ y`, result ~ `bool` |
| `x && y`, `x \|\| y` | `x ~ y`, result ~ `bool` |
| `x \|\|\| y`, `&&&`, `^^^`, `<<<`, `>>>` | `x ~ y`, result ~ type of `x` |
| `x :: xs` | `xs ~ TSlice(type of x)`, result ~ type of `xs` |

Foglang does not enforce numeric constraints on arithmetic operators at the inference level — the Go compiler catches operand type mismatches during compilation. This is acceptable because inference guarantees operand types match each other.

#### Pattern matching

| Pattern | Constraint |
|---|---|
| `_` | none |
| `x` (variable) | binds `x` with type of scrutinee |
| `42` (int literal) | scrutinee ~ fresh `TVar` constrained with `CNumeric tsInt` |
| `true` / `false` | scrutinee ~ `bool` |
| `[]` | scrutinee ~ `TShape (CSlice (TVar n))` |
| `hd :: tl` | scrutinee ~ `TShape (CSlice (TVar n))`, `hd` bound as `TVar n`, `tl` bound as scrutinee type |
| `(a, b)` | tuple components bound with fresh TVars (constrained by usage in arm body) |

All arm bodies must have the same type (the result type of the match expression).

#### Variadic functions

Variadic parameters (`(args : ...T)`) generate constraints: each argument in the variadic position must unify with `T`. Spread expressions (`xs...`) constrain `xs ~ TShape (CSlice T)`.

## Opaque and any types

Qualified names (e.g. `fmt.Println`) and Go builtins (`len`, `append`) have opaque types. The `any` type (Go's empty interface) behaves identically to opaque for unification. Both unify freely — the wildcard check short-circuits unification without recording a binding.

Future work: parse Go stdlib source or query the Go compiler to obtain real type signatures, replacing opaque with concrete types.

## Unit and struct{} coercion

`()` and `struct{}` are distinct named types that unify successfully during inference. This is necessary because fog uses `()` as the unit type while Go represents it as `struct{}`. Without this, passing a `struct{}`-returning Go function's result to a fog function expecting `()` would produce a false type error.

The coercion between `()` and `struct{}` in generated Go code is inserted by `insertCoercions` after resolution, using `ECoerce FuncVoidCoerce`.

### Phase 2: Resolution (with folded defaulting)

`resolveExpr` walks the `Expr TypeExpr` tree once, converting every `TypeExpr` annotation to a ground `Type`. Threaded through the walk is the `Subst` — path-compression continues to help during resolution.

`resolveType`:

- `TShape c` — recurse into the shape's children.
- `TVar n` → `find n`:
  - `FoundUnbound` — default to `OpaqueType` (`TyNamed "opaque"`).
  - `FoundRoot _ (RConcrete c)` — recurse into `c`.
  - `FoundRoot _ (RConstraint (CNumeric ts))` — default to `TyNamed (tsDefault ts)` (e.g. `int` for `tsInt`).
  - `FoundRoot _ (RConstraint (CIndexable k v))` — recursively resolve `k` and `v` first, then decide:
    - key resolves to a named int-ish type → `TySlice v`
    - key resolves to any other named type (including opaque) → `TyMap k v`
    - key resolves to something non-named (e.g. `TySlice` — only possible when a sibling indexable defaulted to slice) → `CannotInferType` error

This structural recursion replaces the old iterative `defaultLoop`. Cascade dependencies resolve naturally via bottom-up walk: an indexable's key is resolved before the indexable itself, so by the time the indexable decides slice vs map, its key is already concrete.

Failure short-circuits: the `Either [InferError]` monad stops at the first `CannotInferType`, so a pathologically deep unresolvable chain produces exactly one error, not O(N²) of them.

### Phase 3: `isStmt` annotation + coercion insertion

After resolution, two post-passes over `Expr Type`:

- `computeIsStmt` — bottom-up walk that marks each node with whether it or any child can stand as a Go statement. Used by codegen.
- `insertCoercions` — inserts `ECoerce FuncVoidCoerce` nodes at type boundaries where function return types differ only in the `unit ↔ struct{}` dimension.

## Recursive functions

A recursive function references itself in its own body. To handle this:

1. The parser assigns a fresh `TVar` to the function's type.
2. `inferLet` adds the function to the environment with that `TVar` *before* inferring the body.
3. Inferring the body generates constraints involving the `TVar`.
4. Unification resolves the `TVar` to the function's actual type.

This is the same mechanism as any other binding — the recursion is not special-cased.

## Interaction with codegen

Codegen consumes `Expr Type`. Since `Type` has no variable constructor, it is structurally impossible for codegen to encounter an unresolved type — the phase transition happens at the signature boundary.

## State and fresh variables

The parser maintains a `pNextTypeVarId :: Int` counter to generate unique TypeVar IDs. `inferAndResolve` seeds inference's counter at `pNextTypeVarId` so fresh vars minted during inference can't collide with parser-minted ones.

The inference monad is `StateT InferState (Either InferError)`, where `InferState` holds the `Subst` and `iNextTypeVarId`. Short-circuit on first error via `Either`. Post-inference, resolution runs in a fresh `StateT Subst (Either [InferError])` so the compressed substitution threads through the resolve walk without the inference state getting in the way.

## Performance characteristics

Union-find with **path compression** (what's implemented here) gives amortised Θ(log n) per `find` operation, where n is the number of type variables. Total inference work is O(|program| · log n).

Tarjan's classic O(α(n)) bound — α being the inverse Ackermann function — requires path compression **plus** union-by-rank. We have the former but not the latter; see the `TODO(perf)` in `Foglang.Subst`. In practice log n is plenty: n above ~1M is unrealistic for fog programs, so the gap between log n (~20) and α(n) (~4) is dwarfed by the constant factor of the IntMap operations.

The map-based substitution this replaced was O(N²) on deeply-nested indexable chains because each use-site re-walked the chain from scratch. Path compression flattens as it traverses, so each chain is walked at most once across the entire pipeline.

A regression test in `test/Foglang/Test/InferencePerfSpec.hs` stresses this with 256- and 1024-level pathological chains under wall-clock budgets.
