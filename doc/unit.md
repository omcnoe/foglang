# Unit type and `()` semantics

## Type system

`()` is both the unit value and the unit type in fog (like Haskell/OCaml). `unit` is not a type/keyword.

`()` and `struct{}` are distinct fog types, but the compiler performs implicit coercion between them.

## Function definitions and calls

### Zero-param rewrite
`()` in a parameter list means "unit value" — but a sole anonymous `()` is rewritten to a zero-param Go function (no parameter at all). Any other use of `()` becomes `struct{}` in Go.

### Zero-return rewrite
A `... => ()` return type is also rewritten: the Go return type is omitted as is the final `return`.

| fog | Go | note |
|---|---|---|
| `let f () => int` | `func f() int` | sole anonymous `()` → zero-param rewrite |
| `let f () -> () => int` | `func f(_p0 struct{}, _p1 struct{}) int` | multiple `()` → struct{} params |
| `let f (x : struct{}) => int` | `func f(x struct{}) int` | explicit `struct{}` param |
| `let f (x : ()) => int` | compile error | `()` as a named param type has no representable function type due to zero-param rewrite; use `(x : struct{})` |
| `let f () => ()` | `func f() { ... }` | `=> ()` → zero-return rewrite, no return type or return statement |
| `let f () => struct{}` | `func f() struct{} { return struct{}{} }` | `=> struct{}` → return type and statement kept |

At call sites the fog programmer can always write `f ()`. The compiler emits `f()` or `f(struct{}{})` based on the callee's Go type.

## Function type syntax

Function types are written `(T1 -> T2 -> ... => Tr)`, and obey the same zero-param and zero-return rewrite rules.

| fog type | Go type |
|---|---|
| `(() => ())` | `func()` |
| `(() => struct{})` | `func() struct{}` |
| `(struct{} => ())` | `func(struct{})` |
| `(() -> struct{} => ())` | `func(struct{}, struct{})` |

## Coercion

The fog compiler coerces between `()` and `struct{}` when required to produce valid Go output.

**Binding a void unit function result**

Calls `f` for side effects, then synthesises a `struct{}{}` value for the binding.
```
// fog:
let f () => () = ...
let x : () = f ()

// go:
f()
x := struct{}{}
```

**Coercing a void unit function to/from `... => struct{}`**

The compiler generates a wrapper lambda forwarding all arguments.
```
// fog:
let printInt (n : int) => () = ...
let coerced : (int => struct{}) = printInt

// go:
var coerced func(int) struct{} = func(_p0 int) struct{} { printInt(_p0); return struct{}{} }
```

## `main`

`let main () => () = ...` only.
