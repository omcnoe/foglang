# Foglang Type Annotation Design

## Syntax overview

### Value annotations

```
let x : int = 7
let name : string = "hello"
```

`:` means "has type", uniformly for values and parameters.

### Function definitions

A function definition has zero or more parameters followed by a return type annotation:

```
let f (x : int) (y : string) => bool = ...
```

Each annotated parameter is written `(name : type)`. The return type is written `=> type` after the last parameter.

**Type annotations are currently mandatory — type inference is not yet implemented.**

Parameters may optionally be separated by `->` instead of spaces — purely stylistic:

```
let f (x : int) -> (y : string) => bool = ...   -- identical to above
```

### Token roles

`->` always means "next parameter". `=>` always means "returns". This holds identically in both definition heads and type expressions — the definition head syntax mirrors the type expression syntax exactly, just with names added:

```
let mult (x : float64) -> (y : float64) => float64 = x * y
-- type: (float64 -> float64 => float64)

let applyTwice (f : int -> int => int) -> (x : int) => int = f (f x)
-- type: ((int -> int => int) -> int => int)
```

### Type expressions

Inside `(name : type)` param annotations, a type is either a plain name or a parenthesised function type:

- plain name: `int`, `bool`, `string`, ...
- function type: `(T1 -> T2 -> ... => Tr)` — always in parens

```
int                           -- a plain type
(int -> int => int)           -- takes int, takes int, returns int
((int -> int => int) => bool) -- takes a 2-arg int function, returns bool
```

### Type constraints (generics)

Type constraints follow Go's square bracket syntax, not Haskell's `=>` convention. `=>` is not used for constraints in Foglang:

```
let map[T comparable, U int64 | float64] (f : T => U) -> (xs : []T) => []U = ...
```

This keeps `=>` unambiguously meaning "returns" throughout the language.

**Generics will be implemented in the future.** For now they are not to be implemented in the type system.
