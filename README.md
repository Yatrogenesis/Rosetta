# Rosetta Stone

**Legacy Language to Rust Transpiler**

Rosetta enables scientists using legacy languages (FORTRAN, COBOL, LISP, QuickBASIC, ML) to migrate their code to modern, safe, high-performance Rust while maintaining numerical equivalence.

## Supported Languages

| Language | Status | Extensions |
|----------|--------|------------|
| FORTRAN 77 | In Development | `.f`, `.for`, `.f77` |
| FORTRAN 90+ | In Development | `.f90`, `.f95`, `.f03`, `.f08` |
| COBOL | Planned | `.cob`, `.cbl` |
| Common Lisp | Planned | `.lisp`, `.cl` |
| Scheme | Planned | `.scm`, `.ss` |
| QuickBASIC | Planned | `.bas`, `.bi` |
| Standard ML | Planned | `.sml`, `.sig` |
| OCaml | Planned | `.ml`, `.mli` |

## Architecture

```
Source Code (FORTRAN, COBOL, LISP, QB, ML)
      |
      v
+-------------+
|   Lexer     |  Token stream
+-------------+
      |
      v
+-------------+
|   Parser    |  Source AST
+-------------+
      |
      v
+-------------+
|  Analyzer   |  Type inference, semantic analysis
+-------------+
      |
      v
+-------------+
|   IR Gen    |  Rosetta Intermediate Representation
+-------------+
      |
      v
+-------------+
|  Optimizer  |  IR transformations
+-------------+
      |
      v
+-------------+
|  Codegen    |  Rust source code
+-------------+
```

## Usage

```bash
# Transpile FORTRAN to Rust
rosetta transpile program.f90 -o program.rs

# Specify source language
rosetta transpile --lang fortran77 legacy.f -o modern.rs

# Validate transpilation (compare outputs)
rosetta validate program.f90 program.rs --input test_data.txt

# List supported languages
rosetta languages
```

## Crates

- `rosetta-core`: Common types, IR, traits
- `rosetta-ir`: Intermediate Representation
- `rosetta-codegen`: Rust code generation
- `rosetta-fortran`: FORTRAN frontend
- `rosetta-cobol`: COBOL frontend
- `rosetta-lisp`: Common Lisp / Scheme frontend
- `rosetta-quickbasic`: QuickBASIC frontend
- `rosetta-ml`: Standard ML / OCaml frontend
- `rosetta-cli`: Command-line interface
- `rosetta-validator`: Numerical equivalence testing

## Why Rosetta?

1. **Preserve Scientific Code**: Millions of lines of validated FORTRAN code in academia
2. **Memory Safety**: Rust's guarantees without rewriting from scratch
3. **Performance**: Match or exceed FORTRAN performance
4. **Maintainability**: Modern tooling, testing, documentation
5. **Interoperability**: Call Rust from anywhere you'd call C

## Integration with HumanBrain

Rosetta is designed to integrate with [HumanBrain](https://github.com/Yatrogenesis/HumanBrain):

- Import ModelDB legacy models
- Convert NMODL mechanisms to Rust
- Port ion channels from FORTRAN (Channelpedia originals)
- Enable scientists to use HumanBrain without learning Rust

## License

MIT OR Apache-2.0
