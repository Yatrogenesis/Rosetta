//! # Rosetta Core
//!
//! Core types, traits, and abstractions for the Rosetta transpiler.
//!
//! ## Architecture
//!
//! ```text
//! Source Code (FORTRAN, COBOL, LISP, QB, ML, PLANNER, OPS5, KRL, Prolog)
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
    /// PLANNER (1969, MIT)
    Planner,
    /// OPS5 (1981, CMU)
    Ops5,
    /// KRL (1977, Xerox PARC)
    Krl,
    /// Prolog
    Prolog,
    /// Unknown/Generic
    Unknown,
}

impl Default for SourceLanguage {
    fn default() -> Self {
        Self::Unknown
    }
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
            Self::Planner => &["pln", "planner"],
            Self::Ops5 => &["ops", "ops5"],
            Self::Krl => &["krl"],
            Self::Prolog => &["pl", "pro", "prolog"],
            Self::Unknown => &[],
        }
    }

    /// Check if language uses significant whitespace/columns
    pub fn is_fixed_format(&self) -> bool {
        matches!(self, Self::Fortran77 | Self::Cobol)
    }

    /// Check if language is a symbolic AI language
    pub fn is_symbolic_ai(&self) -> bool {
        matches!(self, Self::Planner | Self::Ops5 | Self::Krl | Self::Prolog)
    }

    /// Check if language uses Lisp-style s-expressions
    pub fn is_lisp_family(&self) -> bool {
        matches!(self, Self::CommonLisp | Self::Scheme | Self::Planner)
    }
}

/// Position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct SourceSpan {
    /// Start location
    pub start: SourceLocation,
    /// End location
    pub end: SourceLocation,
    /// Source file ID
    pub file_id: usize,
}

/// Source file representation
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// File name
    pub name: String,
    /// File content
    pub content: String,
    /// Detected language
    pub language: SourceLanguage,
}

impl SourceFile {
    pub fn new(name: &str, content: String) -> Self {
        let language = Self::detect_language(name);
        Self {
            name: name.to_string(),
            content,
            language,
        }
    }

    fn detect_language(filename: &str) -> SourceLanguage {
        let ext = filename.rsplit('.').next().unwrap_or("");
        match ext.to_lowercase().as_str() {
            "f" | "for" | "f77" => SourceLanguage::Fortran77,
            "f90" => SourceLanguage::Fortran90,
            "f95" => SourceLanguage::Fortran95,
            "f03" => SourceLanguage::Fortran2003,
            "f08" => SourceLanguage::Fortran2008,
            "cob" | "cbl" | "cpy" => SourceLanguage::Cobol,
            "lisp" | "cl" | "lsp" => SourceLanguage::CommonLisp,
            "scm" | "ss" => SourceLanguage::Scheme,
            "bas" | "bi" => SourceLanguage::QuickBasic,
            "sml" | "sig" => SourceLanguage::StandardML,
            "ml" | "mli" => SourceLanguage::OCaml,
            "pas" | "pp" => SourceLanguage::Pascal,
            "pln" | "planner" => SourceLanguage::Planner,
            "ops" | "ops5" => SourceLanguage::Ops5,
            "krl" => SourceLanguage::Krl,
            "pl" | "pro" | "prolog" => SourceLanguage::Prolog,
            _ => SourceLanguage::Unknown,
        }
    }
}

/// Core transpiler trait that all frontends must implement
pub trait Frontend {
    /// Frontend name
    fn name(&self) -> &'static str;

    /// File extensions this frontend handles
    fn file_extensions(&self) -> &[&'static str];

    /// Parse source code into IR module
    fn parse(&self, source: &SourceFile) -> std::result::Result<crate::RosettaIr, ParseError>;
}

/// Parse errors
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found} at line {line}, column {column}")]
    UnexpectedToken {
        expected: String,
        found: String,
        line: usize,
        column: usize,
    },

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Lexer error: {0}")]
    LexerError(String),

    #[error("Semantic error: {0}")]
    SemanticError(String),

    #[error("Unsupported construct: {0}")]
    Unsupported(String),
}

/// Placeholder for the IR type (defined in rosetta-ir)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RosettaIr {
    /// Module name
    pub name: String,
    /// IR nodes
    pub nodes: Vec<IrNode>,
}

/// IR node
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
    /// Struct definition
    Struct {
        name: String,
        fields: Vec<(String, IrType)>,
    },
    /// Expression
    Expr(IrExpr),
    /// Statement
    Stmt(IrStmt),
    /// Comment
    Comment(String),
}

/// IR type - comprehensive type system
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum IrType {
    /// Integer (with bit width)
    Int(u8),
    /// Floating point (with bit width)
    Float(u8),
    /// Boolean
    Bool,
    /// String
    String,
    /// Character
    Char,
    /// Unit type ()
    Unit,
    /// Array of type with optional size
    Array(Box<IrType>, Option<usize>),
    /// Vector (dynamic array)
    Vec(Box<IrType>),
    /// Reference to type
    Ref(Box<IrType>),
    /// Mutable reference
    MutRef(Box<IrType>),
    /// Box (owned pointer)
    Box(Box<IrType>),
    /// Option type
    Option(Box<IrType>),
    /// Result type
    Result(Box<IrType>, Box<IrType>),
    /// Tuple
    Tuple(Vec<IrType>),
    /// Struct by name
    Struct(String),
    /// Function type
    Fn(Vec<IrType>, Box<IrType>),
    /// Iterator type
    Iterator(Box<IrType>),
    /// Any (for dynamic typing from AI languages)
    Any,
    /// Unknown (needs inference)
    Unknown,
}

/// IR expression - comprehensive expression types for all source languages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrExpr {
    // === Basic Literals ===
    /// Integer literal
    Int(i64),
    /// Float literal
    Float(f64),
    /// Boolean literal
    Bool(bool),
    /// String literal
    String(String),
    /// Character literal
    Char(char),
    /// Nil/null/None
    Nil,
    /// Symbol (for Lisp-family)
    Symbol(String),

    // === Variables and Identifiers ===
    /// Variable/identifier reference
    Identifier(String),
    /// Pattern variable (for PLANNER, Prolog)
    PatternVar(String),

    // === Operators ===
    /// Binary operation
    BinaryOp {
        op: String,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// Unary operation
    UnaryOp {
        op: String,
        operand: Box<IrExpr>,
    },

    // === Function Calls ===
    /// Function call with expression as function
    Call {
        func: Box<IrExpr>,
        args: Vec<IrExpr>,
    },

    // === Data Structures ===
    /// List literal
    List(Vec<IrExpr>),
    /// List cons (head :: tail)
    ListCons {
        head: Box<IrExpr>,
        tail: Box<IrExpr>,
    },
    /// Struct initialization
    StructInit {
        name: String,
        fields: Vec<(String, IrExpr)>,
    },
    /// Tuple
    Tuple(Vec<IrExpr>),

    // === Access ===
    /// Field access (object.field)
    FieldAccess {
        object: Box<IrExpr>,
        field: String,
    },
    /// Field assignment (object.field = value)
    FieldAssign {
        object: String,
        field: String,
        value: Box<IrExpr>,
    },
    /// Array/list index
    Index {
        array: Box<IrExpr>,
        index: Box<IrExpr>,
    },

    // === Control Flow ===
    /// If expression
    If {
        condition: Box<IrExpr>,
        then_branch: Box<IrExpr>,
        else_branch: Option<Box<IrExpr>>,
    },
    /// Cond (multiple conditions, Lisp-style)
    Cond(Vec<(IrExpr, IrExpr)>),
    /// Match/case expression
    Match {
        scrutinee: Box<IrExpr>,
        arms: Vec<(IrExpr, IrExpr)>,
    },
    /// Block of expressions
    Block(Vec<IrExpr>),
    /// Return
    Return(Box<IrExpr>),

    // === Assignment ===
    /// Variable assignment
    Assign {
        target: String,
        value: Box<IrExpr>,
    },
    /// Let binding
    Let {
        name: String,
        value: Box<IrExpr>,
        body: Box<IrExpr>,
    },

    // === Functions ===
    /// Lambda/closure
    Lambda {
        params: Vec<String>,
        body: Vec<IrExpr>,
    },

    // === Lisp-specific ===
    /// Quoted expression
    Quote(Box<IrExpr>),
    /// Quasiquote
    Quasiquote(Box<IrExpr>),
    /// Unquote (within quasiquote)
    Unquote(Box<IrExpr>),

    // === Logic Programming (Prolog, PLANNER) ===
    /// Goal (for pattern-directed invocation)
    Goal {
        pattern: Box<IrExpr>,
        body: Vec<IrExpr>,
    },
    /// Unification
    Unify {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// Pattern matching
    PatternMatch {
        value: Box<IrExpr>,
        pattern: Box<IrExpr>,
    },
    /// Choice point (for backtracking)
    Choice(Vec<IrExpr>),

    // === Production Systems (OPS5) ===
    /// Working memory element creation
    WmeCreate {
        class: String,
        attributes: Vec<(String, IrExpr)>,
    },

    // === Comments and Metadata ===
    /// Comment (for documentation)
    Comment(String),
}

/// IR literal values (for backward compatibility)
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
    /// For-each loop
    ForEach {
        var: String,
        iterable: IrExpr,
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
        assert!(SourceLanguage::Prolog.extensions().contains(&"pl"));
        assert!(SourceLanguage::Planner.extensions().contains(&"pln"));
    }

    #[test]
    fn test_fixed_format() {
        assert!(SourceLanguage::Fortran77.is_fixed_format());
        assert!(SourceLanguage::Cobol.is_fixed_format());
        assert!(!SourceLanguage::Fortran90.is_fixed_format());
    }

    #[test]
    fn test_symbolic_ai() {
        assert!(SourceLanguage::Planner.is_symbolic_ai());
        assert!(SourceLanguage::Ops5.is_symbolic_ai());
        assert!(SourceLanguage::Krl.is_symbolic_ai());
        assert!(SourceLanguage::Prolog.is_symbolic_ai());
        assert!(!SourceLanguage::Fortran77.is_symbolic_ai());
    }

    #[test]
    fn test_lisp_family() {
        assert!(SourceLanguage::CommonLisp.is_lisp_family());
        assert!(SourceLanguage::Scheme.is_lisp_family());
        assert!(SourceLanguage::Planner.is_lisp_family());
        assert!(!SourceLanguage::Prolog.is_lisp_family());
    }
}
