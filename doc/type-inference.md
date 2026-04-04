# Type Inference

## Overview

Foglang uses constraint-based type inference. Type annotations are optional everywhere - on local bindings, function parameters, return types, and top-level definitions. When a type is omitted, the compiler assigns a type variable (TVar) and resolves it from context.

```
let add x y = x + y + 1       // inferred: int -> int => int
let greeting = "hello"        // inferred: string
let double (x : int) = x * 2  // param annotated, return type inferred
```

When the user does provide an annotation, the compiler uses it as a constraint. Annotations are never ignored - they anchor inference and produce better error messages.

Inference is monomorphic - there is no let-polymorphism or generalization. Each binding gets a single concrete type. Polymorphism will be introduced later alongside explicit generics.

## Source positions

Every AST node carries a source position (row and column). This is required for meaningful error messages from type inference - "type mismatch: int vs string at line 12, column 5".

## Type variables

A type variable (`TVar n`) represents an unknown type, identified by a unique integer. A constrained type variable (`TConstrained n ts`) represents a type variable that must resolve to a member of a type set. TVars and TConstrained appear in `TypeExpr` alongside concrete types:

```haskell
data TypeSet = TypeSet
  { tsMembers :: Set Ident  - e.g. {"int", "int8", ...} or {"float32", "float64"}
  , tsDefault :: Ident      - default type when unconstrained (e.g. "int" or "float64")
  }

data TypeExpr
  = TNamed Ident
  | TSlice TypeExpr
  | TMap TypeExpr TypeExpr
  | TFunc [TypeExpr] (Maybe TypeExpr) TypeExpr
  | TVar Int
  | TConstrained Int TypeSet
```

Pre-defined type sets: `tsInt` (int, int8, ..., rune; default: int) and `tsFloat` (float32, float64; default: float64).

TVars are introduced by the parser everywhere a type is not explicitly known:
- Unannotated parameters, return types, and value bindings

Constrained type variables are introduced for numeric literals:
- Integer literals - `EIntLit (TConstrained n tsInt) 1`
- Float literals - `EFloatLit (TConstrained n tsFloat) 2.0`

String literals get `TNamed "string"` directly (no TVar needed - there is only one string type).

Every expression in the AST carries a type slot. The parser populates it with either the user's annotation or a fresh TVar. Since the AST and typed AST have identical structure, there is a single unified `Expr` type used throughout the pipeline - no separate TAST. The parser emits `Expr` with TVars, inference resolves TVars in-place (via substitution), and codegen reads the resolved types from the same `Expr`.

## Parameter syntax

Parameters can be written in three forms:

```
let f (x : int) (y : int) = x + y    // fully annotated
let f (x) (y) = x + y                // parenthesised, type omitted
let f x y = x + y                    // bare identifier
```

All three produce the same AST structure. Unannotated params get a `TVar` in the type slot.

### Bare parameter parsing

After parsing `let name`, the parser greedily consumes bare identifiers and parenthesized params. Then it dispatches on the next token:

- `=>` - function with explicit return type: `let f x y => int = x + y`
- `=` with params collected - function with inferred return type: `let f x y = x + y`
- `=` with no params collected - value binding with inferred type: `let x = 5`
- `:` - value binding with explicit type (no params): `let x : int = 5`

The `=` sign is the unambiguous boundary between params and body. The presence or absence of collected params before `=` distinguishes functions from value bindings.

Bare and annotated params can be mixed: `let f x (y : int) = x + y`.

## Inference process

Inference has two phases.

### Phase 1: Constraint generation

Walk the AST and collect a list of constraints. Each constraint is a triple `(TypeExpr, TypeExpr, SrcPos)` - two types that must be equal, plus the source position that generated the constraint. The source position is used for error messages when unification fails.

#### Expressions

| Expression | Constraint |
|---|---|
| `let x : T = e` | type of `e` ~ `T`; `Binding`'s return type ~ `T` |
| `let x = e` | type of `x` (a TVar) ~ type of `e`; `Binding`'s return type (a TVar) ~ type of body |
| `x` (variable ref) | TVar of the `EVar` node ~ type looked up from environment |
| `f x` | type of `x` ~ param type of `f`, result ~ return type of `f` |
| `f a b c` (multi-arg) | each arg unified with corresponding param type; result type accounts for partial application if fewer args than params |
| `if c then a else b` | type of `c` ~ `bool`, type of `a` ~ type of `b` |
| `func (x) = body` | result type is `TFunc` built from param types and body type; `Binding`'s return type ~ type of body |
| `e1; e2; e3` (sequence) | result type ~ type of last expression; intermediate expressions must still be fully resolved (for future diagnostics like "result discarded" warnings) but do not constrain the sequence's result type |
| `()` (unit literal) | type is `TNamed "()"` - no TVar needed |
| `42` | already carries a `TVar` from the parser; constrained by context |
| `3.14` | already carries a `TVar` from the parser; constrained by context |
| `"str"` | already carries a `TVar` from the parser; constrained by context |
| `[a, b, c]` | type of `a` ~ type of `b` ~ type of `c`, result ~ `TSlice(type of a)` |
| `[]` (empty slice) | result ~ `TSlice(TVar n)` - element type resolved by context |
| `{}` (empty map) | result ~ `TMap(TVar k, TVar v)` - key/value types resolved by context |
| `e[idx]` | container `e` stays as its TVar until context resolves whether it is a slice or map; once resolved, index type and result type are constrained (slice: `idx` ~ `int`, result ~ element type; map: `idx` ~ key type, result ~ value type) |
| `xs...` (spread) | `xs` ~ `TSlice(T)` where `T` is the variadic param type |

#### Operators

All infix operators generate constraints on their operands. Foglang does not enforce numeric constraints on arithmetic operators at the inference level - the Go compiler catches operand type mismatches (e.g. `"a" - "b"`) during compilation. This is acceptable because inference guarantees operand types match each other; Go validates that the operation is defined for that type.

| Operator | Constraints |
|---|---|
| `x + y`, `x - y`, `x * y`, `x / y`, `x % y` | type of `x` ~ type of `y`, result ~ type of `x` |
| `x == y`, `x != y`, `x < y`, `x > y`, `x <= y`, `x >= y` | type of `x` ~ type of `y`, result ~ `bool` |
| `x && y`, `x \|\| y` | type of `x` ~ type of `y`, result ~ `bool` |
| `x \|\|\| y`, `x &&& y`, `x ^^^ y`, `x <<< y`, `x >>> y` | type of `x` ~ type of `y`, result ~ type of `x` |
| `x :: xs` | type of `xs` ~ `TSlice(type of x)`, result ~ type of `xs` |

#### Pattern matching

Match expressions generate constraints from the scrutinee and each arm's pattern:

| Pattern | Constraint |
|---|---|
| `_` | none |
| `x` (variable) | binds `x` with type of scrutinee |
| `42` (int literal) | scrutinee ~ `TConstrained n TSInt` (a fresh constrained variable) |
| `true` / `false` | scrutinee ~ `bool` |
| `[]` | scrutinee ~ `TSlice(TVar n)` |
| `hd :: tl` | scrutinee ~ `TSlice(TVar n)`, `hd` bound as `TVar n`, `tl` bound as scrutinee type |
| `(a, b)` | tuple components bound with fresh TVars (constrained by usage in arm body); tuples arise from Go multi-return (e.g. map comma-ok) and have no `TupleType` in `TypeExpr` - components remain opaque unless constrained by usage |

All arm bodies must have the same type (the result type of the match expression).

#### Variadic functions

Variadic parameters (`(args : ...T)`) generate constraints: each argument in the variadic position must unify with `T`. Spread expressions (`xs...`) constrain `xs` ~ `TSlice(T)`.

## Opaque and any types

Qualified names (e.g. `fmt.Println`) and Go builtins (`len`, `append`) have opaque types. The `any` type (Go's empty interface) behaves identically to opaque for unification purposes.

Both `opaque` and `any` unify freely with any type - the wildcard check fires before any TVar binding, so the substitution is unchanged.

Future work: parse Go stdlib source or query the Go compiler to obtain real type signatures, replacing opaque with concrete types.

## Unit and struct{} coercion

`()` and `struct{}` are distinct named types that unify successfully during inference. This is necessary because fog uses `()` as the unit type while Go represents it as `struct{}`. Without this, passing a `struct{}`-returning Go function's result to a fog function expecting `()` would produce a false type error.

The coercion between `()` and `struct{}` in generated Go code remains in codegen.

### Phase 2: Solving (unification)

Process constraints iteratively. For each constraint `(A, B, pos)`:

- `TConstrained n S` ~ `TNamed t` -> if `t in S`, record `n = TNamed t`; otherwise type error
- `TNamed t` ~ `TConstrained n S` -> same
- `TConstrained n S1` ~ `TConstrained m S2` -> if `S1 == S2`, record `m = TConstrained n S1`; otherwise type error (e.g. int literal ~ float literal)
- `TConstrained n S` ~ `TVar m` -> record `m = TConstrained n S` (propagate the constraint)
- `TVar m` ~ `TConstrained n S` -> same
- `TVar n` ~ `T` -> record `TVar n = T` in the substitution map, apply to remaining constraints
- `T` ~ `TVar n` -> same
- `TNamed a` ~ `TNamed b` -> success if `a == b`; also success if one is `()` and the other is `struct{}`; otherwise type error (report `pos`)
- `int` ~ `string` -> type error (report `pos`)
- `[]A` ~ `[]B` -> new constraint: `A` ~ `B`
- `map[K1]V1` ~ `map[K2]V2` -> new constraints: `K1` ~ `K2`, `V1` ~ `V2`
- `TFunc as va ra` ~ `TFunc bs vb rb` -> unify param lists element-wise: each `as[i]` ~ `bs[i]`; lists must have the same length (otherwise type error); unify variadic types: both `Nothing` (ok), both `Just t` (unify `t`s), one `Just` and one `Nothing` (type error); unify return types: `ra` ~ `rb`
- `opaque` ~ `T` -> success (no substitution recorded)
- `T` ~ `opaque` -> success (no substitution recorded)
- `any` ~ `T` -> success (no substitution recorded)
- `T` ~ `any` -> success (no substitution recorded)

The result is a substitution map from TVar IDs to concrete types. Apply it to the Expr tree, replacing every TVar with its solved type.

### Occurs check

Before recording `TVar n = T`, verify that `TVar n` does not appear inside `T`. This prevents cyclic substitutions like `TVar 1 = []TVar 1` that would cause infinite loops during substitution application. This does not affect user-defined recursive types (future ADTs), since those recurse through named types, not through TVars.

## Bidirectional flow

When a type is already known from context, push it downward instead of generating a TVar and unifying later. For example, `let x : int32 = 42` - the `int32` annotation flows down into the literal during parsing, so it's assigned `int32` directly rather than getting a TVar that must be solved. This is an optimisation for directness and better error messages, not a separate mechanism.

## Defaulting

After solving, walk the AST to default any remaining unresolved type variables:

- `TConstrained n ts`: default to `tsDefault ts` (e.g. `int` for `tsInt`, `float64` for `tsFloat`). This applies regardless of which AST node the variable is on.
- Standalone `TVar` on expression nodes: default to `opaque` (workaround for Go builtins like map access that interact with tuple-destructured variables having no type constraints; will be removed when fog models Go builtin signatures with real types).
- `TVar` inside collection types (TSlice, TMap): default to `opaque` (e.g. empty slice/map literals whose element types were not constrained by context).
- `TVar` as direct TFunc params/returns: NOT defaulted; these indicate genuine inference failures and are reported as `CannotInferType` errors.

## Recursive functions

A recursive function references itself in its own body. To handle this:

1. Assign a `TVar` for the function's type
2. Add the function to the environment with that `TVar`
3. Infer the body, which generates constraints involving the `TVar`
4. Unification resolves the `TVar` to the function's actual type

This is the same mechanism as any other binding - the recursion is not special-cased.

## Interaction with codegen

Codegen expects a fully resolved Expr tree with no TVars. After inference + solving + defaulting, every `TVar` must be replaced with a concrete type. If any `TVar` survives to codegen, that's a compiler bug.

## State

The parser maintains a `nextTVar :: Int` counter to generate unique TVar IDs. The parser type is `ParsecT Void Text (ReaderT Env (State Int))`. State is the innermost monad, which means TVar IDs do not backtrack on parse failures (a failed branch that minted TVar 5 causes the next branch to mint TVar 6). This is harmless - IDs only need to be unique, not contiguous - but is incidental rather than intentional.

Inference maintains a `substitution :: Map Int TypeExpr` - the solved TVars. This can be threaded explicitly through function arguments or wrapped in a State monad - they are equivalent.

