# Partial Application

Fog supports partial application for all functions. Applying fewer arguments than a function requires produces a new function value that captures the supplied arguments and awaits the rest.

## Basic Example

```fog
let add (a : int) (b : int) => int = a + b

let main () => () =
  let add5 : (int => int) = add 5
  fmt.Println (add5 3)  - 8
  fmt.Println (add5 10) - 15
```

`add 5` supplies one of two required arguments. The result is a `(int => int)` function that closes over `5` as `a`.

The generated Go uses a closure:

```go
add5 := func(_p0 int) int { return add(5, _p0) }
```

## How It Works

A fog function call `f a b c` where `f` takes `n` parameters:

- If the number of supplied arguments equals `n`: full call, emits `f(a, b, c)`
- If fewer are supplied: partial application, emits a closure capturing the supplied args

This is determined at compile time from the type of `f`, so no runtime overhead is involved.

## No Auto-Currying

Fog does not auto-curry into return types. In languages where every function takes a single parameter (e.g. Haskell, ML), `f x y` is sugar for `(f x) y`. Fog has multi-param functions (matching Go), so `f x y` means "call f with two args", not "call f with one arg then call the result with the second".

If a function returns another function and you want to call both, use explicit parens:

```fog
let f (x : int) => (int => int) = func (y : int) => int = x + y
(f 1) 2     // ok: call f with 1, then call the result with 2
f 1 2       // type error: f only takes 1 arg
```

Flat `(int -> int => int)` and nested `(int => (int => int))` are distinct types.

## Higher-Order Functions

Partial application composes naturally with higher-order functions:

```fog
let applyN (f : (float64 => float64)) (n : int) (x : float64) => float64 =
  if n <= 0 then x
  else applyN f (n - 1) (f x)

let sqrt (x : float64) => float64 =
  let improve : (float64 => float64) =
    func (g : float64) => float64 = (g + x / g) / 2.0
  applyN improve 50 (x / 2.0)
```

Here `applyN improve 50` partially applies `applyN`, capturing `improve` and `50`, resulting in a `(float64 => float64)`.

## Variadic Functions

Partial application of a variadic function's fixed parameters works normally. What you cannot do is partially apply the variadic slot itself - once you supply any variadic arguments (or `()`), the call is complete. See [variadic.md](variadic.md) for the full design.
