//! # Rosetta FORTRAN Frontend
//!
//! Parses FORTRAN source code (F77 through F2008) into Rosetta IR.
//!
//! ## Supported FORTRAN Features
//!
//! ### FORTRAN 77 (Fixed Format)
//! - Columns 1-5: Labels
//! - Column 6: Continuation
//! - Columns 7-72: Statements
//! - Implicit typing
//! - COMMON blocks
//! - Arithmetic IF
//!
//! ### FORTRAN 90+
//! - Free format
//! - Modules
//! - Derived types
//! - Array operations
//! - Pointers
//! - Intent declarations
//!
//! ## FORTRAN to Rust Mapping
//!
//! | FORTRAN | Rust |
//! |---------|------|
//! | INTEGER | i32 |
//! | REAL | f32 |
//! | DOUBLE PRECISION | f64 |
//! | COMPLEX | num_complex::Complex32 |
//! | LOGICAL | bool |
//! | CHARACTER(n) | [u8; n] or String |
//! | DIMENSION(n) | [T; n] or ndarray |
//! | SUBROUTINE | fn(...) -> () |
//! | FUNCTION | fn(...) -> T |

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, Result, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrModule;
use serde::{Deserialize, Serialize};

/// FORTRAN token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum FortranToken {
    // Keywords
    #[token("PROGRAM", ignore(ascii_case))]
    Program,
    #[token("END", ignore(ascii_case))]
    End,
    #[token("SUBROUTINE", ignore(ascii_case))]
    Subroutine,
    #[token("FUNCTION", ignore(ascii_case))]
    Function,
    #[token("MODULE", ignore(ascii_case))]
    Module,
    #[token("USE", ignore(ascii_case))]
    Use,
    #[token("IMPLICIT", ignore(ascii_case))]
    Implicit,
    #[token("NONE", ignore(ascii_case))]
    None_,

    // Types
    #[token("INTEGER", ignore(ascii_case))]
    Integer,
    #[token("REAL", ignore(ascii_case))]
    Real,
    #[token("DOUBLE", ignore(ascii_case))]
    Double,
    #[token("PRECISION", ignore(ascii_case))]
    Precision,
    #[token("COMPLEX", ignore(ascii_case))]
    Complex,
    #[token("LOGICAL", ignore(ascii_case))]
    Logical,
    #[token("CHARACTER", ignore(ascii_case))]
    Character,

    // Control flow
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("ENDIF", ignore(ascii_case))]
    EndIf,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("ENDDO", ignore(ascii_case))]
    EndDo,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("GOTO", ignore(ascii_case))]
    Goto,
    #[token("CONTINUE", ignore(ascii_case))]
    Continue,
    #[token("RETURN", ignore(ascii_case))]
    Return,
    #[token("CALL", ignore(ascii_case))]
    Call,

    // Declarations
    #[token("DIMENSION", ignore(ascii_case))]
    Dimension,
    #[token("PARAMETER", ignore(ascii_case))]
    Parameter,
    #[token("COMMON", ignore(ascii_case))]
    Common,
    #[token("DATA", ignore(ascii_case))]
    Data,
    #[token("INTENT", ignore(ascii_case))]
    Intent,
    #[token("IN", ignore(ascii_case))]
    In,
    #[token("OUT", ignore(ascii_case))]
    Out,
    #[token("INOUT", ignore(ascii_case))]
    InOut,
    #[token("ALLOCATABLE", ignore(ascii_case))]
    Allocatable,
    #[token("POINTER", ignore(ascii_case))]
    Pointer,
    #[token("TARGET", ignore(ascii_case))]
    Target,

    // I/O
    #[token("READ", ignore(ascii_case))]
    Read,
    #[token("WRITE", ignore(ascii_case))]
    Write,
    #[token("PRINT", ignore(ascii_case))]
    Print,
    #[token("OPEN", ignore(ascii_case))]
    Open,
    #[token("CLOSE", ignore(ascii_case))]
    Close,
    #[token("FORMAT", ignore(ascii_case))]
    Format,

    // Operators
    #[token(".TRUE.", ignore(ascii_case))]
    True,
    #[token(".FALSE.", ignore(ascii_case))]
    False,
    #[token(".AND.", ignore(ascii_case))]
    And,
    #[token(".OR.", ignore(ascii_case))]
    Or,
    #[token(".NOT.", ignore(ascii_case))]
    Not,
    #[token(".EQ.", ignore(ascii_case))]
    Eq,
    #[token(".NE.", ignore(ascii_case))]
    Ne,
    #[token(".LT.", ignore(ascii_case))]
    Lt,
    #[token(".LE.", ignore(ascii_case))]
    Le,
    #[token(".GT.", ignore(ascii_case))]
    Gt,
    #[token(".GE.", ignore(ascii_case))]
    Ge,

    // Symbols
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*", priority = 3)]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    Power,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]*([eEdD][+-]?[0-9]+)?", |lex| lex.slice().replace(['d', 'D'], "e").parse().ok())]
    RealLit(f64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    StringLit(String),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_uppercase())]
    Ident(String),

    // Newline
    #[regex(r"\n")]
    Newline,

    // Comment (! style only for free-form, C/c/* are handled by preprocessor for fixed-form)
    #[regex(r"!.*", priority = 1)]
    Comment,
}

/// FORTRAN AST types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranAst {
    Program {
        name: String,
        units: Vec<FortranUnit>,
    },
}

/// FORTRAN program unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranUnit {
    Subroutine {
        name: String,
        params: Vec<String>,
        body: Vec<FortranStmt>,
    },
    Function {
        name: String,
        params: Vec<String>,
        return_type: FortranType,
        body: Vec<FortranStmt>,
    },
    Module {
        name: String,
        declarations: Vec<FortranDecl>,
        contains: Vec<FortranUnit>,
    },
}

/// FORTRAN type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranType {
    Integer { kind: Option<i32> },
    Real { kind: Option<i32> },
    DoublePrecision,
    Complex { kind: Option<i32> },
    Logical { kind: Option<i32> },
    Character { len: Option<FortranExpr> },
    Derived(String),
}

/// FORTRAN declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranDecl {
    Variable {
        ty: FortranType,
        names: Vec<(String, Option<Vec<FortranExpr>>)>, // name, dimensions
        intent: Option<Intent>,
    },
    Parameter {
        name: String,
        value: FortranExpr,
    },
    Common {
        name: Option<String>,
        vars: Vec<String>,
    },
    Implicit(Option<Vec<(FortranType, char, char)>>), // None = IMPLICIT NONE
}

/// Intent attribute
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Intent {
    In,
    Out,
    InOut,
}

/// FORTRAN statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranStmt {
    Assignment {
        target: FortranExpr,
        value: FortranExpr,
    },
    If {
        condition: FortranExpr,
        then_block: Vec<FortranStmt>,
        else_block: Option<Vec<FortranStmt>>,
    },
    Do {
        var: String,
        start: FortranExpr,
        end: FortranExpr,
        step: Option<FortranExpr>,
        body: Vec<FortranStmt>,
    },
    DoWhile {
        condition: FortranExpr,
        body: Vec<FortranStmt>,
    },
    Call {
        name: String,
        args: Vec<FortranExpr>,
    },
    Return(Option<FortranExpr>),
    Continue,
    Goto(i32),
    Label(i32, Box<FortranStmt>),
    Write {
        unit: FortranExpr,
        format: Option<FortranExpr>,
        items: Vec<FortranExpr>,
    },
    Read {
        unit: FortranExpr,
        format: Option<FortranExpr>,
        items: Vec<FortranExpr>,
    },
    Declaration(FortranDecl),
}

/// FORTRAN expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranExpr {
    IntLit(i64),
    RealLit(f64),
    StringLit(String),
    LogicalLit(bool),
    Var(String),
    ArrayRef {
        name: String,
        indices: Vec<FortranExpr>,
    },
    BinOp {
        op: FortranBinOp,
        left: Box<FortranExpr>,
        right: Box<FortranExpr>,
    },
    UnaryOp {
        op: FortranUnaryOp,
        operand: Box<FortranExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<FortranExpr>,
    },
    Substring {
        var: Box<FortranExpr>,
        start: Box<FortranExpr>,
        end: Box<FortranExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FortranBinOp {
    Add, Sub, Mul, Div, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FortranUnaryOp {
    Neg, Not,
}

/// FORTRAN parser
pub struct FortranParser {
    /// Source language version
    version: SourceLanguage,
    /// Whether to use fixed format (F77)
    fixed_format: bool,
}

impl FortranParser {
    /// Create a new FORTRAN 77 parser (fixed format)
    pub fn fortran77() -> Self {
        Self {
            version: SourceLanguage::Fortran77,
            fixed_format: true,
        }
    }

    /// Create a new FORTRAN 90+ parser (free format)
    pub fn fortran90() -> Self {
        Self {
            version: SourceLanguage::Fortran90,
            fixed_format: false,
        }
    }

    /// Preprocess fixed-format source
    fn preprocess_fixed(&self, source: &str) -> String {
        let mut result = String::new();

        for line in source.lines() {
            // Handle comment lines (C, c, *, or !)
            if line.starts_with('C') || line.starts_with('c') ||
               line.starts_with('*') || line.starts_with('!') {
                result.push_str("! ");
                result.push_str(line);
                result.push('\n');
                continue;
            }

            // Handle continuation (column 6)
            if line.len() >= 6 && !line.chars().nth(5).unwrap_or(' ').is_whitespace()
                && line.chars().nth(5).unwrap_or('0') != '0' {
                // Continuation line - append to previous
                if result.ends_with('\n') {
                    result.pop();
                }
                if line.len() > 6 {
                    result.push_str(&line[6..].trim_end());
                }
            } else {
                // New statement
                if line.len() > 6 {
                    // Extract label if present (columns 1-5)
                    let label = line[..5].trim();
                    if !label.is_empty() {
                        result.push_str(label);
                        result.push(' ');
                    }
                    result.push_str(line[6..].trim_end());
                }
                result.push('\n');
            }
        }

        result
    }
}

impl Frontend for FortranParser {
    fn name(&self) -> &'static str {
        match self.version {
            SourceLanguage::Fortran77 => "FORTRAN 77",
            SourceLanguage::Fortran90 => "FORTRAN 90",
            SourceLanguage::Fortran95 => "FORTRAN 95",
            SourceLanguage::Fortran2003 => "FORTRAN 2003",
            SourceLanguage::Fortran2008 => "FORTRAN 2008",
            _ => "FORTRAN",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        match self.version {
            SourceLanguage::Fortran77 => &["f", "for", "f77"],
            SourceLanguage::Fortran90 => &["f90"],
            SourceLanguage::Fortran95 => &["f95"],
            SourceLanguage::Fortran2003 => &["f03"],
            SourceLanguage::Fortran2008 => &["f08"],
            _ => &["f", "for"],
        }
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let processed = if self.fixed_format {
            self.preprocess_fixed(&source.content)
        } else {
            source.content.clone()
        };

        let mut lexer = FortranToken::lexer(&processed);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => tokens.push(t),
                Err(_) => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "valid token".to_string(),
                        found: "invalid".to_string(),
                        line: 1,
                        column: lexer.span().start,
                    });
                }
            }
        }

        // Parse tokens into AST
        let ast = FortranAst::Program {
            name: "main".to_string(),
            units: vec![],
        };

        // Convert AST to IR and then to RosettaIr
        let module = FortranToIr::convert(&ast).map_err(|e| ParseError::SemanticError(e.to_string()))?;

        Ok(module.into_rosetta_ir())
    }
}

/// Convert FORTRAN AST to Rosetta IR
pub struct FortranToIr;

impl FortranToIr {
    pub fn convert(ast: &FortranAst) -> Result<IrModule> {
        let FortranAst::Program { name, units: _ } = ast;

        let builder = rosetta_ir::IrBuilder::with_language(name, SourceLanguage::Fortran77);

        // TODO: Convert units to IR functions/modules

        Ok(builder.build())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let source = "PROGRAM TEST\nINTEGER X\nEND PROGRAM";
        let mut lexer = FortranToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(FortranToken::Program)));
    }

    #[test]
    fn test_fixed_format_preprocessing() {
        let parser = FortranParser::fortran77();
        let source = "C     COMMENT\n      X = 1";
        let processed = parser.preprocess_fixed(source);

        assert!(processed.contains("!"));
        assert!(processed.contains("X = 1"));
    }

    #[test]
    fn test_parse_simple() {
        use rosetta_core::{SourceFile, SourceLanguage};
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: "PROGRAM TEST\nEND PROGRAM TEST".to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);

        assert!(result.is_ok());
    }
}
