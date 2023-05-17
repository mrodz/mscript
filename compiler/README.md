# This is the crate for the MScript Compiler.

## Compilation Steps
There are three primary steps in the MScript Compilation phase.

1. Source -> Abstract Syntax Tree (AST)
2. AST (unoptimized) -> AST (optimized)
3. AST (optimized) -> Bytecode

---

More details will come soon. Individual implementations for different parts of the AST are found in the [/ast](./src/ast/) folder, including source -> ast mappings, component-specific optimizations, and `ast -> bytecode` algorithms.

Stay tuned! 🎺