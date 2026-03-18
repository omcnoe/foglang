* Missing Go syntax — filling out more of Go's expression/statement surface
* Functional data types — sum types, product types, etc.
* Generics — noted in types.md as future work, following Go's square-bracket syntax
* Pattern matching — likely tied to functional data types
* Extend type system — type checking / inference pass
* Package/module design — how foglang's module system maps to Go's
* Conforming string literal syntax
* Language server (LSP) — go-to-def, type info, diagnostics; requires holding elab results in memory and implementing the LSP protocol
* Codegen: hoist if/else out of expression context into declare-then-assign to avoid IIFEs wherever possible
