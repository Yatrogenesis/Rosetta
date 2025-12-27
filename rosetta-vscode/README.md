# Rosetta VSCode Extension

Language support and transpilation tools for legacy programming languages.

## Features

- **Syntax Highlighting** for FORTRAN, COBOL, MUMPS, RPG, REXX, Lisp, Pascal, Ada, APL, PL/I, QuickBASIC, Prolog, and more
- **One-Click Transpilation** to Rust, C, or WebAssembly
- **IR Visualization** - View the intermediate representation
- **Numerical Equivalence Validation** - Ensure transpiled code matches original behavior

## Supported Languages

| Language | Extensions | Status |
|----------|------------|--------|
| FORTRAN | .f, .for, .f77, .f90, .f95 | Full |
| COBOL | .cob, .cbl, .cobol | Full |
| MUMPS/M | .m, .mps | Full |
| RPG | .rpg, .rpgle | Full |
| REXX | .rexx, .rex | Full |
| Lisp | .lisp, .lsp, .cl, .scm | Full |
| Pascal | .pas, .pp | Full |
| Ada | .ada, .adb, .ads | Full |
| APL | .apl | Full |
| PL/I | .pli, .pl1 | Full |
| QuickBASIC | .bas | Full |
| Prolog | .pl, .pro | Full |

## Usage

1. Open a supported legacy language file
2. Use CodeLens buttons at the top of the file, or:
3. Right-click and select a Rosetta command from the context menu

### Commands

- `Rosetta: Transpile to Rust` - Convert to idiomatic Rust
- `Rosetta: Transpile to C` - Convert to portable C
- `Rosetta: Transpile to WebAssembly` - Convert to WAT format
- `Rosetta: Show IR` - Display intermediate representation
- `Rosetta: Validate Equivalence` - Test numerical accuracy

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rosetta.cliPath` | Path to Rosetta CLI | `rosetta` |
| `rosetta.defaultTarget` | Default transpilation target | `rust` |
| `rosetta.showIROnTranspile` | Show IR with output | `false` |
| `rosetta.autoValidate` | Auto-validate after transpile | `true` |

## Requirements

- Rosetta CLI installed and in PATH (or configure `rosetta.cliPath`)

## Installation

### From Marketplace

Search for "Rosetta Transpiler" in VSCode extensions.

### From Source

```bash
cd rosetta-vscode
npm install
npm run compile
vsce package
code --install-extension rosetta-transpiler-0.1.0.vsix
```

## License

MIT OR Apache-2.0
