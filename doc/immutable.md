# Immutable Data Structures

## Maps

Fog maps (`map[K]V`) have immutable semantics. Operations that modify a map return a new map, leaving the original unchanged.

### Operations

```fog
// Lookup — returns (V, bool), no copy
table[key]

// Insert — returns new map with key set
let table = insert table key value

// Delete — returns new map with key removed
let table = delete table key

// Insert with merge function — if key exists, merge old and new values
let table = insertWith append table key [p]
```

### Codegen

Mutation operations compile to a shallow copy of the underlying Go map followed by the mutation on the copy. No wrapper struct or runtime library needed.

`insert table key value` emits:
```go
table_ := make(map[K]V, len(table))
for k, v := range table {
    table_[k] = v
}
table_[key] = value
```

`delete table key` emits:
```go
table_ := make(map[K]V, len(table))
for k, v := range table {
    table_[k] = v
}
delete(table_, key)
```

Lookup (`table[key]`) is a read and emits directly as Go map access:
```fog
let (value, ok) = table[key]
```
emits:
```go
value, ok := table[key]
```

### Future optimization

The copy-on-write approach is O(n) per mutation. Once the semantics are proven out, the codegen can be swapped to use a persistent data structure (HAMT or balanced BST) without changing any fog source code.

## Slices

Fog slices (`[]T`) have immutable semantics.

### Operations

```fog
// Index access — read, no copy
xs[i]

// Length — read, no copy
len xs

// Cons — prepend element, returns new slice
x :: xs

// Append — returns new slice
append xs ys

// Pattern matching — destructure head and tail
match xs with
  | [] => ...
  | x :: rest => ...
```

### Codegen

`x :: xs` emits:
```go
xs_ := make([]T, len(xs)+1)
xs_[0] = x
copy(xs_[1:], xs)
```

`append xs ys` emits:
```go
xs_ := make([]T, len(xs)+len(ys))
copy(xs_, xs)
copy(xs_[len(xs):], ys)
```

Pattern matching on `x :: rest` emits:
```go
x := xs[0]
rest := xs[1:]  // sub-slice, shares underlying array (safe since immutable)
```

### Future optimization

Cons and append are O(n) with copy-on-write. Could be replaced with a persistent vector (RRB tree or similar) if performance becomes a bottleneck.
