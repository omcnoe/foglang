# Type Inference

## Overview

Foglang uses constraint-based type inference. Type annotations are optional everywhere - on local bindings, function parameters, return types, and top-level definitions. When a type is omitted, the compiler assigns a type variable (TypeVar) and resolves it from context.

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

A type variable (`TypeVar n`) represents an unknown type, identified by a unique integer. A constrained type variable (`TypeVarConstrained n ts`) represents a type variable that must resolve to a member of a type set. An indexable type variable (`TypeVarIndexable n tk tv`) represents a type variable that must resolve to an indexable type (able to use indexing `[x]` postfix operator). `TypeVarConstrained` and `TypeVarIndexable` are placeholders until the type system is more powerful and able to represent such concepts natively. TypeVars, TypeVarConstrained and TypeVarIndexable appear in `TypeExpr` alongside concrete types.


TypeVars are introduced by the parser everywhere a type is not explicitly known:
- Unannotated parameters, return types, and value bindings

Constrained type variables are introduced for numeric literals:
- Integer literals - `EIntLit (TypeVarConstrained n tsInt) 1`
- Float literals - `EFloatLit (TypeVarConstrained n tsFloat) 2.0`

String literals get `TNamed "string"` directly (no TypeVar needed - there is only one string type).

Every expression in the AST carries a type slot. The parser populates it with either the user's annotation or a fresh TypeVar. Since the AST and typed AST have identical structure, there is a single unified `Expr` type used throughout the pipeline - no separate TAST. The parser emits `Expr` with TypeVars, inference resolves TypeVars in-place (via substitution), and codegen reads the resolved types from the same `Expr`.

## Parameter syntax

Parameters can be written in three forms:

```
let f (x : int) (y : int) = x + y    // fully annotated
let f (x) (y) = x + y                // parenthesised, type omitted
let f x y = x + y                    // bare identifier
```

All three produce the same AST structure. Unannotated params get a `TypeVar` in the type slot.

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
| `let x = e` | type of `x` (a TypeVar) ~ type of `e`; `Binding`'s return type (a TypeVar) ~ type of body |
| `x` (variable ref) | TypeVar of the `EVar` node ~ type looked up from environment |
| `f x` | type of `x` ~ param type of `f`, result ~ return type of `f` |
| `f a b c` (multi-arg) | each arg unified with corresponding param type; result type accounts for partial application if fewer args than params |
| `if c then a else b` | type of `c` ~ `bool`, type of `a` ~ type of `b` |
| `func (x) = body` | result type is `TFunc` built from param types and body type; `Binding`'s return type ~ type of body |
| `e1; e2; e3` (sequence) | result type ~ type of last expression; intermediate expressions must still be fully resolved (for future diagnostics like "result discarded" warnings) but do not constrain the sequence's result type |
| `()` (unit literal) | type is `TNamed "()"` - no TypeVar needed |
| `42` | already carries a `TypeVar` from the parser; constrained by context |
| `3.14` | already carries a `TypeVar` from the parser; constrained by context |
| `"str"` | already carries a `TypeVar` from the parser; constrained by context |
| `[a, b, c]` | type of `a` ~ type of `b` ~ type of `c`, result ~ `TSlice(type of a)` |
| `[]` (empty slice) | result ~ `TSlice(TypeVar n)` - element type resolved by context |
| `{}` (empty map) | result ~ `TMap(TypeVar k, TypeVar v)` - key/value types resolved by context |
| `e[idx]` | container `e` stays as its TypeVar until context resolves whether it is a slice or map; once resolved, index type and result type are constrained (slice: `idx` ~ `int`, result ~ element type; map: `idx` ~ key type, result ~ value type) |
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
| `42` (int literal) | scrutinee ~ `TypeVarConstrained n TSInt` (a fresh constrained variable) |
| `true` / `false` | scrutinee ~ `bool` |
| `[]` | scrutinee ~ `TSlice(TypeVar n)` |
| `hd :: tl` | scrutinee ~ `TSlice(TypeVar n)`, `hd` bound as `TypeVar n`, `tl` bound as scrutinee type |
| `(a, b)` | tuple components bound with fresh TypeVars (constrained by usage in arm body); tuples arise from Go multi-return (e.g. map comma-ok) and have no `TupleType` in `TypeExpr` - components remain opaque unless constrained by usage |

All arm bodies must have the same type (the result type of the match expression).

#### Variadic functions

Variadic parameters (`(args : ...T)`) generate constraints: each argument in the variadic position must unify with `T`. Spread expressions (`xs...`) constrain `xs` ~ `TSlice(T)`.

## Opaque and any types

Qualified names (e.g. `fmt.Println`) and Go builtins (`len`, `append`) have opaque types. The `any` type (Go's empty interface) behaves identically to opaque for unification purposes.

Both `opaque` and `any` unify freely with any type - the wildcard check fires before any TypeVar binding, so the substitution is unchanged.

Future work: parse Go stdlib source or query the Go compiler to obtain real type signatures, replacing opaque with concrete types.

## Unit and struct{} coercion

`()` and `struct{}` are distinct named types that unify successfully during inference. This is necessary because fog uses `()` as the unit type while Go represents it as `struct{}`. Without this, passing a `struct{}`-returning Go function's result to a fog function expecting `()` would produce a false type error.

The coercion between `()` and `struct{}` in generated Go code remains in codegen.

### Phase 2: Solving (unification)

Process constraints iteratively. For each constraint `(A, B, pos)`:

- `TypeVarConstrained n S` ~ `TNamed t` -> if `t in S`, record `n = TNamed t`; otherwise type error
- `TNamed t` ~ `TypeVarConstrained n S` -> same
- `TypeVarConstrained n S1` ~ `TypeVarConstrained m S2` -> if `S1 == S2`, record `m = TypeVarConstrained n S1`; otherwise type error (e.g. int literal ~ float literal)
- `TypeVarConstrained n S` ~ `TypeVar m` -> record `m = TypeVarConstrained n S` (propagate the constraint)
- `TypeVar m` ~ `TypeVarConstrained n S` -> same
- `TypeVar n` ~ `T` -> record `TypeVar n = T` in the substitution map, apply to remaining constraints
- `T` ~ `TypeVar n` -> same
- `TNamed a` ~ `TNamed b` -> success if `a == b`; also success if one is `()` and the other is `struct{}`; otherwise type error (report `pos`)
- `int` ~ `string` -> type error (report `pos`)
- `[]A` ~ `[]B` -> new constraint: `A` ~ `B`
- `map[K1]V1` ~ `map[K2]V2` -> new constraints: `K1` ~ `K2`, `V1` ~ `V2`
- `TFunc as va ra` ~ `TFunc bs vb rb` -> unify param lists element-wise: each `as[i]` ~ `bs[i]`; lists must have the same length (otherwise type error); unify variadic types: both `Nothing` (ok), both `Just t` (unify `t`s), one `Just` and one `Nothing` (type error); unify return types: `ra` ~ `rb`
- `opaque` ~ `T` -> success (no substitution recorded)
- `T` ~ `opaque` -> success (no substitution recorded)
- `any` ~ `T` -> success (no substitution recorded)
- `T` ~ `any` -> success (no substitution recorded)

The result is a substitution map from TypeVar IDs to concrete types. Apply it to the Expr tree, replacing every TypeVar with its solved type.

### Occurs check

Before recording `TypeVar n = T`, verify that `TypeVar n` does not appear inside `T`. This prevents cyclic substitutions like `TypeVar 1 = []TypeVar 1` that would cause infinite loops during substitution application. This does not affect user-defined recursive types (future ADTs), since those recurse through named types, not through TypeVars.

## Bidirectional flow

When a type is already known from context, push it downward instead of generating a TypeVar and unifying later. For example, `let x : int32 = 42` - the `int32` annotation flows down into the literal during parsing, so it's assigned `int32` directly rather than getting a TypeVar that must be solved. This is an optimisation for directness and better error messages, not a separate mechanism.

## Defaulting

After solving, walk the AST to default any remaining unresolved type variables:

- `TypeVarConstrained n ts`: default to `tsDefault ts` (e.g. `int` for `tsInt`, `float64` for `tsFloat`). This applies regardless of which AST node the variable is on.
- Standalone `TypeVar` on expression nodes: default to `opaque` (workaround for Go builtins like map access that interact with tuple-destructured variables having no type constraints; will be removed when fog models Go builtin signatures with real types).
- `TypeVar` inside collection types (TSlice, TMap): default to `opaque` (e.g. empty slice/map literals whose element types were not constrained by context).
- `TypeVar` as direct TFunc params/returns: NOT defaulted; these indicate genuine inference failures and are reported as `CannotInferType` errors.

## Recursive functions

A recursive function references itself in its own body. To handle this:

1. Assign a `TypeVar` for the function's type
2. Add the function to the environment with that `TypeVar`
3. Infer the body, which generates constraints involving the `TypeVar`
4. Unification resolves the `TypeVar` to the function's actual type

This is the same mechanism as any other binding - the recursion is not special-cased.

## Interaction with codegen

Codegen expects a fully resolved Expr tree with no TypeVars. After inference + solving + defaulting, every `TypeVar` must be replaced with a concrete type. If any `TypeVar` survives to codegen, that's a compiler bug.

## State

The parser maintains a `nextTypeVar :: Int` counter to generate unique TypeVar IDs. The parser type is `ParsecT Void Text (ReaderT Env (State Int))`. State is the innermost monad, which means TypeVar IDs do not backtrack on parse failures (a failed branch that minted TypeVar 5 causes the next branch to mint TypeVar 6). This is harmless - IDs only need to be unique, not contiguous - but is incidental rather than intentional.

Inference maintains a `substitution :: Map Int TypeExpr` - the solved TypeVars. This can be threaded explicitly through function arguments or wrapped in a State monad - they are equivalent.

