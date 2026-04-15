# Inference refactor completion plan

Branch: `union-find-inference`. HEAD: `964e167` (clean Types module on top of Phase A union-find). Working tree: partial rename by codex, doesn't compile.

## Status

- **Master**: pre-refactor design with `Subst = Map Int TypeExpr` and the O(N²) pathology.
- **Phase A** (commits `6fcbc5e`, `e834844`, `e6b0c23`): real union-find via `Foglang.Subst` with `Link/Root` split, `Constraint/RootContent/UFEntry` types, three-function bind API. Inference rewritten against this. Perf regression test added (256/1024-chain). Built green at `e6b0c23`.
- **Phase B** (HEAD `964e167`): new `Foglang.Types` that absorbs constraints into `HeadResolvedType` (HRTyIntLit, HRTyFloatLit, HRTyIndexable) and collapses bind API to a single `substBind`. Only Types.hs committed; rest of repo still expects Phase A symbols.
- **Working tree**: codex did a mechanical rename (`TypeExpr→UnresolvedType`, etc.) and updated imports to pull from `Foglang.Types`, but the new Types module doesn't export `Constraint/FindResult/RootContent/bindConcrete/bindConstraint/bindLink/find/tsInt/tsFloat/TypeSet` — those are Phase A constructs the new design eliminates. Type errors also from `UTyHeadResolved` now requiring SourcePos, `UTyVar` taking TypeVarPtr not Int, `bindingType` deleted.

## Vocabulary

Used consistently throughout this doc and to be reflected in code/comments after the refactor.

| Concept | Canonical |
|---|---|
| Representative of a unifying tree | **root** or "tree root" |
| Forwarding entry pointing to another `TypeVarPtr` | **link** |
| Set of type variables that must unify together | **tree** or "unifying tree" |
| Type variable identifier (the `Int` newtype) | `TypeVarPtr` |
| `UnresolvedType` constructor for a substitution reference | `UTyVar` (Unresolved Type Variable) |
| Type variable as an abstract concept (prose) | "type variable" |
| Fully ground type, codegen input | `ResolvedType` / "resolved" |
| WHNF type — outermost constructor committed, children may be `UTyVar` | `HeadResolvedType` / "head-resolved" |
| The three head-resolved types that can narrow once at root: `HRTyIntLit`, `HRTyFloatLit`, `HRTyIndexable` | "constraint" |

**On "head-resolved":** an adjective applied to either a specific `UnresolvedType` value (the `UTyHeadResolved` variant) or to a tree's root state ("the root is head-resolved"). Don't use as a bare noun.

**Avoid** the following words, which were used inconsistently in the prior codebase:
- "shape" — say "head-resolved type" instead.
- "rep", "chain head" — say "root".
- "head" as a standalone noun — name the specific constructor (`HRTySlice`, `HRTyFunc`, etc.) or say "outermost constructor" when explicitly needed.
- "terminal" as a noun — describe non-constraint head-resolved types as "all other head-resolved types" or by pattern-matching specific constructors.
- "concrete" — say "resolved" or "head-resolved" depending on level.
- Capital-`Constraint` — refers to the dead Phase A type, removed in step 4.

## Domain invariants (state these in Types.hs module header)

**Acyclicity of the link graph.** `substBind` refuses `n == m` self-links; longer cycles can't form because links are only ever created at unbound roots. `substFind`'s path-compression assumes this.

**One-shot narrowing.** A root's head-resolved type can change at most once during inference, and only for the three constraints:

| Starting head-resolved type | Can narrow to |
|---|---|
| `HRTyIntLit` | `HRTyNamed n` where `n ∈ intLitTypes` |
| `HRTyFloatLit` | `HRTyNamed n` where `n ∈ floatLitTypes` |
| `HRTyIndexable k v` | `HRTySlice e`, `HRTyMap mk mv`, or `HRTyNamed "string"` |
| anything else | frozen — once at a root, never changes |

Compound head-resolved types (`HRTySlice`, `HRTyMap`, `HRTyFunc`) are not constraints; once placed at a root they are frozen there. Their children live in independent trees and narrow or not in isolation.

**Occurs check purpose.** Prevents `n ↔ HRTySlice (UTyVar _ n)`-style self-referential types. Runs at bind time in `unify`, not at find time. Both fresh `UTyVar`-to-`UTyHeadResolved` bindings *and* narrowing of a constraint root must go through the checked bind path — narrowing is structurally a write of a new head-resolved type at an existing root, and can introduce a cycle if the new type's children transitively reach back to the root being narrowed.

**Root uniqueness.** After path compression, every member of a tree yields the same root id from `substFind`. Inference relies on this for tree-identity comparisons.

## Open design question to settle first

**Does `SourcePos` belong on `UTyHeadResolved`?** Currently yes, but Inference and prelude construct head-resolved types constantly with no meaningful position to attach (`UTyHeadResolved ? (HRTyNamed "bool")`). Recommendation: **drop it**. Keep `SourcePos` only on `UTyVar` (where it's load-bearing for `CannotInferType` error messages pointing at the mint site). The position you'd want for a head-resolved type in diagnostics already lives on the `ExprAnn` of the AST node carrying it — you don't need it on the type value.

This is the single biggest simplification lever for the rest of the rename/rewrite. Decide and apply before step 2.

## Deferred decisions (revisit after first green build)

- **`substBind` narrowing safety check.** Could guard overwrites against the narrowing invariant; skipped for now. Add if a bug surfaces.
- **Splitting `substBind` into `substBind` + `substNarrow`.** Skipped — narrowing site will be localized to one `unify` case. Revisit if narrowing spreads to 4+ call sites.
- **Strictness annotations on `UnresolvedType` / `HeadResolvedType` fields.** TODO in Types.hs header. Marginal perf; defer, can profile and optimize later with big source code examples.

## Strategy

**Continue on this branch, don't restart from master.** Phase A's perf test is load-bearing — it's the regression guard for the whole refactor's reason-for-being. Restarting loses that and three working commits.

**Discard codex's working tree, except where salvageable.** The Inference changes are a rename of structurally-wrong code and need a real rewrite. The AST/Codegen/Parser/test changes are mostly correct mechanical renames that can be redone cleanly in minutes.

First action: `git stash push -u -m "codex partial rename"` — keeps it recoverable without polluting the working tree.

## Steps

Each step ends at a green build (except step 4, which is a working-in-progress inference rewrite).

### Step 1 — Settle the `SourcePos`-on-`UTyHeadResolved` question
- Apply chosen answer to Types.hs.
- Update `UnresolvedType`'s `Eq` instance and any other dependents.
- Verify Types.hs still builds standalone (`cabal build foglang:lib` will still fail on other modules but Types alone should be clean).

### Step 2 — AST.hs
- Remove the definitions now in Types.hs: `TypeExpr`, `ConcreteShape`, `GroundType`, `TypeSet`, `tsInt`, `tsFloat`, `tvarPos`, `isUnitLikeShape`, `isWildcardShape`, `isUnitLike`, `isWildcard`, pattern synonyms `UnitTypeExpr`/`OpaqueTypeExpr`/`UnitType`/`OpaqueType`.
- Remove `bindingType` and `bindingTypeExpr`. Add the type-specialized equivalents in Types.hs:
  ```haskell
  bindingTypeU :: [Param UnresolvedType] -> UnresolvedType -> UnresolvedType
  bindingTypeR :: [Param ResolvedType] -> ResolvedType -> ResolvedType
  ```
  (Or name them `funcTypeU`/`funcTypeR` — they construct function head-resolved types.)
- Keep `Expr`, `Param`, `Binding`, `MatchArm`, `Pattern`, `ExprAnn`, `Coercion`, `FogFile` and friends — these are payload-parametric and stay.
- Drop unused imports.

### Step 3 — Parsers
- `Parser.hs`: `freshTypeVar p = UTyVar p . TypeVarPtr <$> freshTypeVarId`.
- `Parser/Types.hs`: rename `typeExpr` → `unresolvedType`; update all `UTyHeadResolved` constructions per step 1 decision. Add `opaque`/`any` keywords if keeping them as syntax.
- `Parser/Expr.hs`: same adaptations; `UTyHeadResolved HRTyUnit` / `UTyHeadResolved (HRTyNamed "string")` construction sites.
- `Parser/FogFile.hs`: import `UnresolvedType` from Types, rename return type.

Success: parser tests compile and pass.

### Step 4 — Inference.hs rewrite (the substantive work)
Discard codex's version entirely. Start from Phase A (`HEAD~1:src/Foglang/Inference.hs`) and apply the following structural changes. No mechanical rename — every touched region needs a small redesign.

**Fresh variables.**
- `freshTypeVar p :: Infer UnresolvedType` → wraps `TypeVarPtr`.
- Replace `freshConstrainedVar p c` with `freshConstraintVar p hr :: Infer UnresolvedType` that binds a fresh id to `UTyHeadResolved hr` via `substBind`. Call sites pass `HRTyIntLit`, `HRTyFloatLit`, `HRTyIndexable k v` directly — no intermediate Phase A `Constraint` type.

**TypeView collapses.**
- Drop `data TypeView = VShape | VVarUnbound | VVarConstraint`. Replace with direct pattern match on `substFind`'s result: either `UTyVar _ root` (unbound root) or `UTyHeadResolved _ hr` (head-resolved root). Callers that need to know whether `hr` is one of the constraints (`HRTyIntLit`/`HRTyFloatLit`/`HRTyIndexable`) pattern-match on its constructor.

**Unification.**
- Outer dispatch on the `substFind` results:
  1. `UTyVar root1 ~ UTyVar root2`, `root1 == root2`: same tree, no-op.
  2. `UTyVar root1 ~ UTyVar root2`, `root1 /= root2`: link via `substBind root1 (UTyVar _ root2)`.
  3. `UTyVar root ~ UTyHeadResolved hr` (either side): occurs check, then `substBind root (UTyHeadResolved _ hr)` via the checked bind helper (see below).
  4. `UTyHeadResolved hr1 ~ UTyHeadResolved hr2`: dispatch to `unifyHeadResolved hr1 hr2`.
- `unifyHeadResolved` is a pattern match on the constructor pair. Concrete cases:
  - `HRTyNamed a ~ HRTyNamed b`: `a == b` → no-op, else `TypeMismatch`.
  - `HRTySlice a ~ HRTySlice b`: recurse on `a ~ b`.
  - `HRTyMap k1 v1 ~ HRTyMap k2 v2`: recurse on `k1 ~ k2` and `v1 ~ v2`.
  - `HRTyFunc as va ra ~ HRTyFunc bs vb rb`: recurse pairwise on params, on the optional variadic, on the return.
  - `HRTyUnit ~ HRTyUnit`, `HRTyEmptyStruct ~ HRTyEmptyStruct`, `HRTyUnit ~ HRTyEmptyStruct`, `HRTyEmptyStruct ~ HRTyUnit`: no-op (unit-likes interchange freely).
  - Anything `~ HRTyOpaque` or `HRTyOpaque ~ anything`: no-op (wildcard).
  - Anything `~ HRTyAny` or `HRTyAny ~ anything`: no-op (wildcard).
  - Constraint narrowing cases (each writes the narrowed type at the constraint's root via the checked bind helper):
    - `HRTyIntLit ~ HRTyNamed n` (and reverse): if `n ∈ intLitTypes`, narrow constraint root to `HRTyNamed n`; else `TypeMismatch`.
    - `HRTyFloatLit ~ HRTyNamed n` (and reverse): symmetric for `floatLitTypes`.
    - `HRTyIndexable k v ~ HRTySlice e` (and reverse): unify `k ~ HRTyNamed "int"`, `v ~ e`, narrow constraint root to `HRTySlice e`.
    - `HRTyIndexable k v ~ HRTyMap mk mv` (and reverse): unify `k ~ mk`, `v ~ mv`, narrow constraint root to `HRTyMap mk mv`.
    - `HRTyIndexable k v ~ HRTyNamed "string"` (and reverse): unify `k ~ HRTyNamed "int"`, `v ~ HRTyNamed "byte"`, narrow constraint root to `HRTyNamed "string"`.
  - `HRTyIntLit ~ HRTyIntLit`, `HRTyFloatLit ~ HRTyFloatLit`: no-op.
  - `HRTyIndexable k1 v1 ~ HRTyIndexable k2 v2`: recurse on `k1 ~ k2`, `v1 ~ v2`. No narrowing — both roots stay as `HRTyIndexable`.
  - All other constructor pairs: `TypeMismatch`.
- Former `unifyConstraintShape` merges into `unifyHeadResolved` as the constraint-narrowing cases. No separate function.
- **Narrowing must go through the same checked bind path as fresh `UTyVar`-to-`UTyHeadResolved` bindings.** Narrowing a constraint root is structurally a write of a new head-resolved type at an existing root, and can introduce a cycle if the new type's children transitively reach back to the root being narrowed. Phase A's `unifyConstraintShape` skipped occurs check at the narrowing bind — don't replicate that. Route both fresh binds and narrowings through one internal helper (`bindHeadResolvedChecked` or similar) that runs occurs and emits `InfiniteType` on failure.

**Occurs check.**
- Walks `UnresolvedType` directly. At each `UTyVar`, `substFind` to its root and recurse into the root's head-resolved type if present. At each `UTyHeadResolved`, recurse structurally on children.
- Drop `RootContent`/`UFEntry` plumbing.

**Resolution.**
- `resolveType` walks via `substFind`. On `UTyVar` result → defaulting (opaque). On `UTyHeadResolved` result → `resolveHeadResolved`.
- `resolveHeadResolved` pattern-matches all `HeadResolvedType` constructors. Defaulting clauses live here:
  - `HRTyIntLit` → `RTyNamed "int"`
  - `HRTyFloatLit` → `RTyNamed "float64"`
  - `HRTyIndexable k v` → resolve k; case on k's resolved type to pick `RTySlice`/`RTyMap`/`RTyNamed "string"`; `CannotInferType` if k resolves to a non-defaultable, non-wildcard type.
  - All other `HRTy*` → structural 1:1 mapping.
- `CannotInferType` source position: stored on the `UTyVar` at the top of the call. Preserved explicitly rather than via a `tvarPos` helper.

**preludeEnv.**
- Replace `OpaqueTypeExpr` with `UTyHeadResolved HRTyOpaque` (with or without SourcePos per step 1).

**Pretty printing.**
- `prettyType` pattern-matches all `HeadResolvedType` cases.

Success criterion for this step: compiles clean, existing inference tests pass (including the 1024-chain perf test under 10s).

### Step 5 — Codegen.hs
- Mostly codex's rename pass is fine. Confirm `RTyOpaque`/`RTyAny`/`RTyUnit`/`RTyEmptyStruct` are handled everywhere the old `isUnitLike`/`isWildcard` ran. The wildcard predicate (covering `RTyOpaque` + `RTyAny`) and unit-like predicate (covering `RTyUnit` + `RTyEmptyStruct`) probably want explicit `isWildcardR` / `isUnitLikeR` helpers in Codegen.
- Verify `typeGoText` covers all `ResolvedType` constructors.

### Step 6 — Tests
- Apply codex's renames verbatim.
- `bindingType` import resolves once step 2 adds `bindingTypeR` (or equivalent) to Types.
- `Parser/ExprSpec` stripType/stripPos helpers: rename to new constructors.
- Perf test file `InferencePerfSpec.hs` likely compiles as-is since it only touches `InferError`/`inferAndResolve`.

### Step 7 — Green perf tests
- Run: `stack test --test-arguments "--match 'inference performance'"`.
- 256-chain under 5s; 1024-chain under 10s with exactly one `CannotInferType`.
- If regressed: something is doing eager full-resolution during inference. Profile with `+RTS -p -RTS` to find it.

### Step 8 — Polish
- Apply the Types-module-style review to Inference.hs:
  - Consistent naming: `origPos`/`curPtr`/`linkPos` discipline.
  - Dead `where` clauses stripped.
  - Comments only where an invariant or non-obvious decision isn't visible from the structure. Less dense than Types.hs.
- Module header states the inference pipeline phases (constraint gen → unification → resolution/defaulting → isStmt → coercion insertion).

## Performance considerations

The "tree roots may hold head-resolved types with cross-tree children" design is safe:
- `substFind` returns only the root's head-resolved type; children stay as `UnresolvedType` pointers.
- Path compression per-find, amortized cheap.
- No global `applySubst` pass — resolution is on-demand at the end.
- Cross-tree child pointers refer to the other tree's current state at read time, not frozen copies.

Quadratic risk vectors to avoid:
1. Eager "fully resolve this `UnresolvedType` recursively" during unification.
2. Re-resolving the same subtree multiple times without memoization.
3. Running occurs check without de-duplication (the existing `IntSet`-based visited set handles this).

The 1024-chain perf test is the regression alarm. Keep it green.

## Notes for the Inference rewrite

- `substFind` returns `(UnresolvedType, Subst)`. Inference pattern-matches the returned `UnresolvedType` — either `UTyVar _ root` (unbound root) or `UTyHeadResolved _ hr` (head-resolved root).
- When Inference needs both the root id and the head-resolved type, the pattern match gives both in one step. No separate `FindResult` type needed.
- `substBind` is the single write function. Self-link case (`UTyVar _ m` with `n == m`) is a silent no-op.
- Inference still uses `StateT InferState (Either InferError)` — that part is unchanged.
