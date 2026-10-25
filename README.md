# 🪨 Rosetta Stone

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18071014.svg)](https://doi.org/10.5281/zenodo.18071014)
[![Rust](https://img.shields.io/badge/Rust-1.75%2B-orange?logo=rust)](https://www.rust-lang.org/)
[![License](https://img.shields.io/badge/License-MIT%2FApache--2.0-blue)](LICENSE)
[![GitHub Stars](https://img.shields.io/github/stars/Yatrogenesis/Rosetta?style=social)](https://github.com/Yatrogenesis/Rosetta)

**Legacy Language to Rust Transpiler**

> *Preserve decades of scientific code. Transform it into modern, safe, blazing-fast Rust.*

Rosetta enables scientists using legacy languages to migrate their code to modern, safe, high-performance Rust while maintaining **numerical equivalence**.

> **Project status: paused, pending rework (2026-10).**
> Rosetta is an early scaffold, not a finished transpiler. No frontend has been
> validated for numerical equivalence against the original language, and the
> equivalence claims below are goals, not results. The status column states what
> the code does today: frontends marked "Not implemented" return an error, and
> languages marked "Planned" have no frontend yet. Do not use it for research results.

## 🚀 Quick Start

```bash
# Install
cargo install --git https://github.com/Yatrogenesis/Rosetta rosetta-cli

# Interactive mode (recommended)
rosetta

# Or direct transpilation
rosetta transpile program.f90 -o program.rs
```

## ✨ Features

- **Interactive Wizard** - Guided transpilation with fuzzy search
- **Goal: many legacy languages** - see the per-language status below (most are not implemented yet)
- **Batch Processing** - Transpile entire directories with progress bars
- **Numerical Validation** - experimental checker; no reference benchmark suite yet
- **Shell Completions** - For Bash, Zsh, Fish, PowerShell

## 📚 Supported Languages

| Era | Language | Extensions | Status |
|-----|----------|------------|--------|
| 1957 | FORTRAN II | `.f`, `.for` | 📝 Planned (no frontend yet) |
| 1966 | FORTRAN 66 | `.f66` | 📝 Planned (no frontend yet) |
| 1977 | FORTRAN 77 | `.f77` | 🧪 Experimental (parser exists; not validated) |
| 1990 | Fortran 90/95 | `.f90`, `.f95` | 🧪 Experimental (parser exists; not validated) |
| 2003 | Fortran 2003/08/18 | `.f03`, `.f08`, `.f18` | 🧪 Experimental (parser exists; not validated) |
| 1959 | COBOL | `.cob`, `.cbl` | ⛔ Not implemented (returns an error) |
| 1958 | LISP | `.lisp`, `.cl` | 📝 Planned (no frontend yet) |
| 1975 | Scheme | `.scm`, `.ss` | ⛔ Not implemented (returns an error) |
| 1984 | Common Lisp | `.lisp`, `.cl` | ⛔ Not implemented (returns an error) |
| 1991 | Clojure | `.clj`, `.cljs` | 📝 Planned (no frontend yet) |
| 1964 | BASIC | `.bas` | 📝 Planned (no frontend yet) |
| 1985 | QuickBASIC | `.bas`, `.bi` | ⛔ Not implemented (returns an error) |
| 1991 | Visual Basic | `.vb`, `.frm` | 📝 Planned (no frontend yet) |
| 1963 | ALGOL 60/68 | `.alg`, `.a60` | 🧪 Experimental (parser exists; not validated) |
| 1970 | Pascal | `.pas`, `.pp` | 🧪 Experimental (parser exists; not validated) |
| 1972 | Modula-2 | `.mod`, `.def` | 🧪 Experimental (parser exists; not validated) |
| 1988 | Oberon | `.ob`, `.obn` | 📝 Planned (no frontend yet) |
| 1979 | Ada 83/95/2012 | `.ada`, `.adb`, `.ads` | 🧪 Experimental (parser exists; not validated) |
| 1962 | APL | `.apl` | 🧪 Experimental (parser exists; not validated) |
| 1973 | ML | `.sml`, `.ml` | ⛔ Not implemented (returns an error) |
| 1983 | Standard ML | `.sml`, `.sig` | ⛔ Not implemented (returns an error) |
| 1996 | OCaml | `.ml`, `.mli` | ⛔ Not implemented (returns an error) |
| 1990 | Haskell 98/2010 | `.hs`, `.lhs` | 📝 Planned (no frontend yet) |
| 1964 | PL/I | `.pli`, `.pl1` | 🧪 Experimental (parser exists; not validated) |
| 1978 | MATLAB | `.m` | 📝 Planned (no frontend yet) |
| 1988 | Mathematica | `.m`, `.nb`, `.wl` | 📝 Planned (no frontend yet) |
| 1987 | Perl | `.pl`, `.pm` | 📝 Planned (no frontend yet) |
| 1979 | Icon | `.icn` | 📝 Planned (no frontend yet) |
| 1990 | J | `.ijs` | 📝 Planned (no frontend yet) |

## 🎯 Interactive Mode

```
$ rosetta

🪨 ROSETTA STONE - Legacy to Rust Transpiler
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

? What would you like to do?
› 📄 Transpile a single file
  📁 Batch transpile directory
  🔍 Analyze without converting
  📋 List supported languages
  🐚 Generate shell completions
  ❓ Help

? Select source language (type to search):
› FORTRAN 77 (1977)
  FORTRAN 90+ (1990)
  Common Lisp (1984)
  ...

⠹ Parsing source file...
⠹ Analyzing types...
⠹ Generating Rust code...

✅ Transpilation complete!
   Output: program.rs
   Lines: 1,247 → 892 (28% reduction)
```

## 🏗️ Architecture

```
Source Code (FORTRAN, COBOL, LISP, QB, ML, ...)
                    │
                    ▼
            ┌───────────────┐
            │    Lexer      │  Token stream
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │    Parser     │  Source AST
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │   Analyzer    │  Type inference
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │    IR Gen     │  Rosetta IR
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │  Optimizer    │  Transformations
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │   Codegen     │  Safe Rust ✅
            └───────────────┘
```

## 📦 Crates

| Crate | Description |
|-------|-------------|
| `rosetta-core` | Common types, IR, traits |
| `rosetta-ir` | Intermediate Representation |
| `rosetta-codegen` | Rust code generation |
| `rosetta-fortran` | FORTRAN frontend (all versions) |
| `rosetta-cobol` | COBOL frontend |
| `rosetta-lisp` | Lisp family frontend |
| `rosetta-quickbasic` | BASIC family frontend |
| `rosetta-ml` | ML family frontend |
| `rosetta-cli` | Interactive CLI |
| `rosetta-validator` | Numerical equivalence testing |

## 💡 Why Rosetta?

| Problem | Rosetta Solution |
|---------|------------------|
| Millions of lines of legacy FORTRAN in academia | Automatic transpilation |
| Buffer overflows in C/FORTRAN | Rust's memory safety |
| Single-threaded legacy code | Automatic parallelization |
| Unmaintainable 40-year-old code | Modern, documented Rust |
| Can't call from modern languages | Rust FFI works everywhere |

## 🔗 Ecosystem Integration

Rosetta is part of the **Yatrogenesis** scientific computing suite:

- **[HumanBrain](https://github.com/Yatrogenesis/HumanBrain)** - GPU neural simulation
- **[OldiesRules](https://github.com/Yatrogenesis/OldiesRules)** - Legacy simulator revival
- **[Stochastic-Framework](https://github.com/Yatrogenesis/Stochastic-Framework)** - Pattern detection

### Use Cases

- Import ModelDB legacy models into HumanBrain
- Convert NMODL mechanisms to Rust
- Port ion channels from FORTRAN
- Enable scientists to use Rust without learning it

## 📥 Installation

```bash
# From crates.io (when published)
cargo install rosetta-cli

# From source
git clone https://github.com/Yatrogenesis/Rosetta
cd Rosetta
cargo install --path crates/rosetta-cli

# Generate shell completions
rosetta completions bash > ~/.local/share/bash-completion/completions/rosetta
rosetta completions zsh > ~/.zfunc/_rosetta
rosetta completions fish > ~/.config/fish/completions/rosetta.fish
```

## 🤝 Contributing

We welcome contributions! See our [Contributing Guide](CONTRIBUTING.md).

## 📜 License

MIT OR Apache-2.0

---

<p align="center">
  <i>"The Rosetta Stone unlocked ancient Egypt. Rosetta unlocks ancient code."</i>
</p>
