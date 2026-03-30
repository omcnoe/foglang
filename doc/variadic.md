# Variadic Functions

A variadic parameter is written `(name : ...T)` and must be the final parameter. `...T` is syntax sugar for `[]T` - the distinction only affects calling convention.

```fog
let printf (format : string) (args : ...any) => () =
  fmt.Printf format args...
```

## Calling

Supplying variadic arguments completes the call. Supplying only the fixed parameters produces a partial application.

```fog
let run (cmd : string) (args : ...string) => () = ...

run                 - reference to function value
run "ls"            - partial application: ...string => ()
run "ls" "-la"      - call with one variadic arg:    run("ls", "-la")
run "ls" "-l" "-a"  - call with two variadic args:   run("ls", "-l", "-a")
run "ls" ()         - call with zero variadic args:  run("ls")
```

`()` as the sole variadic argument means "call with zero variadic args" and is stripped by codegen. It is required when you want to call a variadic function and supply no variadic arguments - otherwise the expression is a partial application.

## Passing Variadic Arguments

You can pass individual values of type `T` - they are collected automatically:

```fog
run "ls" "-l" "-a"  - run("ls", "-l", "-a")
```

You can also pass a `[]T` (or `...T`) value using the `...` spread operator:

```fog
let myPrintf (format : string) (args : ...any) => () =
  fmt.Printf format args...  - spread args into the variadic slot
```

Passing a `[]T` as the sole variadic parameter without `...` produces a `MissingSpread` warning.
