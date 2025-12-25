//! # Rosetta Core
//!
//! Core types, traits, and abstractions for the Rosetta transpiler.
//!
//! ## Architecture
//!
//! ```text
//! Source Code (FORTRAN, COBOL, LISP, QB, ML)
//!       |
//!       v
//! +-------------+
//! |   Lexer     |  (Token stream)
//! +-------------+
//!       |
//!       v
//! +-------------+
//! |   Parser    |  (Source AST)
//! +-------------+
//!       |
//!       v
//! +-------------+
//! |  Analyzer   |  (Type inference, semantic analysis)
//! +-------------+
//!       |
//!       v
//! +-------------+
//! |   IR Gen    |  (Rosetta Intermediate Representation)
//! +-------------+
//!       |
//!       v
//! +-------------+
//! |  Optimizer  |  (IR transformations)
//! +-------------+
//!       |
//!       v
//! +-------------+
//! |  Codegen    |  (Rust source code)
//! +-------------+
//! ```

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Source language being transpiled
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SourceLanguage {
    /// FORTRAN 77
    Fortran77,
    /// FORTRAN 90
    Fortran90,
    /// FORTRAN 95
    Fortran95,
    /// FORTRAN 2003
    Fortran2003,
    /// FORTRAN 2008
    Fortran2008,
    /// COBOL (any version)
    Cobol,
    /// Common Lisp
    CommonLisp,
    /// Scheme
    Scheme,
    /// QuickBASIC / QBASIC
    QuickBasic,
    /// Standard ML
    StandardML,
    /// OCaml
    OCaml,
    /// Pascal
    Pascal,
    /// Legacy C (K&R style)
    LegacyC,
}

impl SourceLanguage {
    /// Get file extensions for this language
    pub fn extensions(&self) -> &'static [&'static str] {
        match self {
            Self::Fortran77 | Self::Fortran90 | Self::Fortran95 |
            Self::Fortran2003 | Self::Fortran2008 => {
                &["f", "for", "f77", "f90", "f95", "f03", "f08"]
            }
            Self::Cobol => &["cob", "cbl", "cpy"],
            Self::CommonLisp => &["lisp", "cl", "lsp"],
            Self::Scheme => &["scm", "ss"],
            Self::QuickBasic => &["bas", "bi"],
            Self::StandardML => &["sml", "ml", "sig"],
            Self::OCaml => &["ml", "mli"],
            Self::Pascal => &["pas", "pp"],
            Self::LegacyC => &["c", "h"],
        }
    }

    /// Check if language uses significant whitespace/columns
    pub fn is_fixed_format(&self) -> bool {
        matches!(self, Self::Fortran77 | Self::Cobol)
    }
}

/// Position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
    /// Byte offset from start
    pub offset: usize,
}

impl SourceLocation {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self { line, column, offset }
    }
}

/// Span of source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    /// Start location
    pub start: SourceLocation,
    /// End location
    pub end: SourceLocation,
    /// Source file ID
    pub file_id: usize,
}

/// Core transpiler trait that all frontends must implement
pub trait Frontend {
    /// The AST type produced by this frontend
    type Ast;

    /// Parse source code into an AST
    fn parse(&self, source: &str) -> Result<Self::Ast, TranspileError>;

    /// Get the source language
    fn language(&self) -> SourceLanguage;
}

/// Trait for converting AST to IR
pub trait IrGenerator {
    /// The AST type being converted
    type Ast;

    /// Convert AST to Rosetta IR
    fn to_ir(&self, ast: &Self::Ast) -> Result<crate::RosettaIr, TranspileError>;
}

/// Placeholder for the IR type (defined in rosetta-ir)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RosettaIr {
    /// Module name
    pub name: String,
    /// IR nodes
    pub nodes: Vec<IrNode>,
}

/// IR node placeholder
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrNode {
    /// Function definition
    Function {
        name: String,
        params: Vec<(String, IrType)>,
        return_type: Option<IrType>,
        body: Vec<IrNode>,
    },
    /// Variable declaration
    Variable {
        name: String,
        ty: IrType,
        init: Option<Box<IrNode>>,
    },
    /// Expression
    Expr(IrExpr),
    /// Statement
    Stmt(IrStmt),
}

/// IR type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrType {
    /// Integer (with bit width)
    Int(u8),
    /// Floating point (with bit width)
    Float(u8),
    /// Boolean
    Bool,
    /// String
    String,
    /// Array of type
    Array(Box<IrType>, Option<usize>),
    /// Reference to type
    Ref(Box<IrType>),
    /// Mutable reference
    MutRef(Box<IrType>),
    /// Struct
    Struct(String),
    /// Unknown (needs inference)
    Unknown,
}

/// IR expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrExpr {
    /// Literal value
    Literal(IrLiteral),
    /// Variable reference
    Var(String),
    /// Binary operation
    BinOp {
        op: BinOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// Unary operation
    UnaryOp {
        op: UnaryOp,
        operand: Box<IrExpr>,
    },
    /// Function call
    Call {
        func: String,
        args: Vec<IrExpr>,
    },
    /// Array index
    Index {
        array: Box<IrExpr>,
        index: Box<IrExpr>,
    },
    /// Field access
    Field {
        object: Box<IrExpr>,
        field: String,
    },
}

/// IR literal values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor,
    BitAnd, BitOr, BitXor,
    Shl, Shr,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg, Not, BitNot, Deref, Ref,
}

/// IR statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrStmt {
    /// Assignment
    Assign { target: IrExpr, value: IrExpr },
    /// If statement
    If {
        condition: IrExpr,
        then_block: Vec<IrNode>,
        else_block: Option<Vec<IrNode>>,
    },
    /// Loop (general)
    Loop {
        condition: Option<IrExpr>,
        body: Vec<IrNode>,
        is_do_while: bool,
    },
    /// For loop
    For {
        var: String,
        start: IrExpr,
        end: IrExpr,
        step: Option<IrExpr>,
        body: Vec<IrNode>,
    },
    /// Return statement
    Return(Option<IrExpr>),
    /// Break
    Break,
    /// Continue
    Continue,
    /// Expression statement
    Expr(IrExpr),
}

/// Errors during transpilation
#[derive(Debug, Error)]
pub enum TranspileError {
    #[error("Lexer error at {location:?}: {message}")]
    LexerError {
        message: String,
        location: SourceLocation,
    },

    #[error("Parse error at {location:?}: {message}")]
    ParseError {
        message: String,
        location: SourceLocation,
    },

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Semantic error: {0}")]
    SemanticError(String),

    #[error("Unsupported construct: {0}")]
    UnsupportedConstruct(String),

    #[error("Code generation error: {0}")]
    CodegenError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

/// Result type for transpilation operations
pub type Result<T> = std::result::Result<T, TranspileError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_language_extensions() {
        assert!(SourceLanguage::Fortran77.extensions().contains(&"f77"));
        assert!(SourceLanguage::Cobol.extensions().contains(&"cob"));
        assert!(SourceLanguage::CommonLisp.extensions().contains(&"lisp"));
    }

    #[test]
    fn test_fixed_format() {
        assert!(SourceLanguage::Fortran77.is_fixed_format());
        assert!(SourceLanguage::Cobol.is_fixed_format());
        assert!(!SourceLanguage::Fortran90.is_fixed_format());
    }
}
