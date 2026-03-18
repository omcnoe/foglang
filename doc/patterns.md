# Pattern Matching

## match expression

```fog
match <scrutinee> with
  | <pattern> => <body>
  | <pattern> => <body>
```

`match` is an expression — it returns the value of the matched arm's body.

Arms are prefixed with `|` and use `=>` to separate the pattern from the body. Bodies can be multi-line (indented block) or single expressions.

## Examples

Simple value matching:
```fog
match x with
  | 0 => "zero"
  | 1 => "one"
  | _ => "other"
```

Cons destructuring (`::` operator, right-associative, lowest precedence):
```fog
match candidates with
  | [] => []
  | x :: rest => x + 1 :: process rest
```

Tuple patterns (useful for Go's multi-return idioms like map lookup):
```fog
match table[key] with
  | (value, true) => value
  | (_, false) => defaultValue
```

## Open questions

- Exhaustiveness checking (requires type information from elaboration)
- Guard clauses (`| pattern if cond => body`)
- Nested patterns (`| Some (x :: rest) => ...`)
- Or-patterns (`| 0 | 1 => ...`)
