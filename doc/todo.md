* Missing Go syntax - filling out more of Go's expression/statement surface
* Functional data types - sum types, product types, etc.
* Generics - noted in types.md as future work, following Go's square-bracket syntax
* Improved/extended pattern matching - likely tied to functional data types
* Extend type system - generics, polymorphism
* Package/module design - how foglang's module system maps to Go's
* Conforming string literal syntax
* Language server (LSP) - go-to-def, type info, diagnostics; requires holding elab results in memory and implementing the LSP protocol
* Codegen: hoist if/else out of expression context into declare-then-assign to avoid IIFEs wherever possible
* Mutable bindings: `let mut` or similar - enables mutual recursion via forward declaration, loop accumulators, mutable local state, Go interop with mutable APIs
* Immutability: immutability for vars, efficient immutable data structures (eg. Map), "slice" sequences
* Error handling: monads!
* Record/struct syntax, go interop
* Error quality
* Fix: same line sequence siblings require semicolon