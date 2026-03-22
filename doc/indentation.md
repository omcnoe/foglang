# Foglang Indentation Design

## Core concepts

Foglang is indentation-sensitive. There are no braces or semicolons for delimiting blocks or statements. Instead, indentation determines program structure through a single core concept: **line-indent**.

The **line-indent** of a construct is the column of the first non-whitespace character on the physical line where the construct starts. All indentation decisions are made relative to the line-indent:

- **`>` line-indent** → child (part of the construct)
- **`==` line-indent** → sibling (not part of the construct, but part of the enclosing construct)
- **`<` line-indent** → exit (not part of the enclosing construct either)

This rule applies uniformly at every nesting level — sequence items are children of their keyword, continuation lines are children of their expression. The same rule, applied recursively.

A **Sequence** is the fundamental block construct: one or more expressions parsed in order, evaluating to the last. `Sequence` is itself an expression — it can appear anywhere an expression can. Anything that contains a Sequence is a "block header"; the Sequence is its body.

Sequences appear in these positions:

- Module body (top-level)
- `let ... =` right-hand side
- `func ... =` body
- `| pattern =>` match arm body
- `if` condition (terminated by `then`)
- `then` branch (terminated by `else` or dedent)
- `else` branch
- Inside `()`

All parsed the same way. The only variation is what establishes the line-indent and what terminates the Sequence.

## The rules

### Rule 1: line-indent

Children of a construct must be at column **strictly greater than** the construct's line-indent. When something appears at or before the line-indent, it is not a child of that construct.

This governs both sequence membership and expression continuation:

```
let main () => () =               -- "let" line-indent = col 1
  let x : int =                   -- "let x" line-indent = col 3
    1                              -- col 5 > col 3 → child of "let x" (sequence item)
      + 2                          -- col 7 > col 5 → child of "1" (continuation)
    fmt.Println "hello"            -- col 5 == col 5 → sibling of "1" (new sequence item)
  fmt.Println x                    -- col 3 == col 3 → sibling of "let x" (in-expression)
```

When an expression starts mid-line (e.g., after `=` on the same line as `let`), its line-indent is the line's leading indentation, not the expression's token column:

```
let y : int = (                    -- ( starts mid-line; line-indent = col 1 (the "let" line)
  long
  expression
) + 1                              -- col 3 > col 1 → continuation of the paren expression
```

### Rule 2: Line-indent exception

A line at the same indentation normally starts a sibling, and a line at a greater indentation normally starts a child. However, if the line starts with a token that is unambiguously a child or continuation, it is treated as such — indentation within the construct does not affect its role. The token must still be at column >= the parent construct's line-indent; below that, it exits the construct entirely.

- **Unambiguously infix operators** (`||`, `&&`, `*`, `/`, `==`, `!=`, `>=`, `<=`, `>`, `<`, `::`, `|||`, `&&&`, `^^^`, `<<<`, `>>>`) continue an expression.
- `|` continues a `match` expression as a new arm.
- `then`, `else`, `else if` continue an `if` expression.

Ambiguous prefix/infix operators (`+`, `-`) are excluded — at the same column they begin a new sibling.

```
a
|| b                 -- || at same column as "a", unambiguous → continuation
|| c

a
- 3                  -- - is ambiguous, at same column → new sibling

a
  - 3                -- indented past "a" → child by Rule 1 → a - 3

match x with
  | 1 =>
    match y with
      | a => "inner"
      | b => "inner"
  | 2 => "outer"     -- | at same column as outer match → outer arm

match x with
  | 0 => "zero"
      | 1 => "one"   -- deeper than previous |, but | is unambiguous → sibling arm
    | _ => "other"    -- same: unambiguous arm regardless of indentation
```

## Summary of grammar constructs

### Module

A module is a top-level Sequence with line-indent = 0. It contains `package`, `import`, and top-level `let` bindings and expressions.

```
package main

import "fmt"

let x : int = 42

let main () => () =
  fmt.Println x
```

### Let binding

`let name [params] : type = body` or `let name [params] => returnType = body`

The right-hand side of `=` is a Sequence (line-indent = `let`'s line-indent). Everything after the `let` at the same indentation level in the enclosing Sequence becomes the implicit "in" expression — the scope where the binding is visible.

```
let main () => () =
  let x : int = 10       -- binding
  let y : int = x + 1    -- in-expression of x; also a binding
  fmt.Println y           -- in-expression of y
```

### Function expression

`func (params) => returnType = body`

The body after `=` is a Sequence. `func` is an expression — it evaluates to a function value and can appear anywhere an expression can.

```
let double : (int => int) =
  func (x : int) => int =
    fmt.Println "doubling"
    x * 2
```

### If / then / else

`if condition then consequent else alternative`

The condition, `then` branch, and `else` branch are each Sequences. All branches share the outermost `if`'s line-indent.

`else if` is treated as a single compound keyword — it continues the chain at the same level rather than nesting a new `if` inside the `else` branch. This avoids escalating indentation requirements in `else if` chains.

```
if x > 0 then x else 0 - x

if a then 1
else if b then 2
else if c then 3
else 4

if
  let positive : bool = x > 0
  positive
then
  fmt.Println "yes"
  x
else
  fmt.Println "no"
  0 - x
```

### Match

`match scrutinee with | pattern => body | pattern => body ...`

Each arm body is a Sequence. Arms are consumed at column >= `match`'s line-indent.

```
match xs with
  | [] => 0
  | x :: rest =>
    let doubled : int = x * 2
    doubled + sumRest rest
```

### Parenthesised expressions

`( expression )`

Parentheses reset the layout context. Inside, a fresh Sequence is parsed with the line-indent reset. The outer indentation constraints do not apply.

```
let result : int = someFunction (
  let temp : int = compute 42
  temp * 2
)
```
