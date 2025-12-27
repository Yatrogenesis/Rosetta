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
use rosetta_core::{Frontend, SourceLanguage, Result, SourceFile, ParseError, RosettaIr, IrExpr, IrType};
use rosetta_ir::{IrModule, IrFunction, IrParam, IrBuilder, Visibility};
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
    #[token("CONTAINS", ignore(ascii_case))]
    Contains,

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
    #[token("ELSEIF", ignore(ascii_case))]
    ElseIf,
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
    #[token("STOP", ignore(ascii_case))]
    Stop,
    #[token("EXIT", ignore(ascii_case))]
    Exit,
    #[token("CYCLE", ignore(ascii_case))]
    Cycle,

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
    #[token("SAVE", ignore(ascii_case))]
    Save,
    #[token("EXTERNAL", ignore(ascii_case))]
    External,
    #[token("INTRINSIC", ignore(ascii_case))]
    Intrinsic,

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
    #[token(".EQV.", ignore(ascii_case))]
    Eqv,
    #[token(".NEQV.", ignore(ascii_case))]
    Neqv,

    // Modern comparison operators (F90+)
    #[token("==")]
    EqEq,
    #[token("/=")]
    SlashEq,
    #[token("<")]
    LtSym,
    #[token("<=")]
    LeSym,
    #[token(">")]
    GtSym,
    #[token(">=")]
    GeSym,

    // Symbols
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
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
    #[token("/", priority = 2)]
    Slash,
    #[token("**")]
    Power,
    #[token("//")]
    Concat,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]*([eEdD][+-]?[0-9]+)?", |lex| lex.slice().replace(['d', 'D'], "e").parse().ok())]
    RealLit(f64),
    #[regex(r"\.[0-9]+([eEdD][+-]?[0-9]+)?", |lex| lex.slice().replace(['d', 'D'], "e").parse().ok())]
    RealLitDot(f64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    StringLit(String),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLitDouble(String),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_uppercase(), priority = 1)]
    Ident(String),

    // Newline
    #[regex(r"\n")]
    Newline,

    // Comment (! style for free-form)
    #[regex(r"!.*", priority = 2)]
    Comment,
}

/// Token with position information
#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: FortranToken,
    pub line: usize,
    pub column: usize,
}

/// FORTRAN AST types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FortranAst {
    Program {
        name: String,
        body: Vec<FortranStmt>,
        subprograms: Vec<FortranUnit>,
    },
    Module {
        name: String,
        declarations: Vec<FortranDecl>,
        contains: Vec<FortranUnit>,
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
        return_type: Option<FortranType>,
        body: Vec<FortranStmt>,
    },
    Program {
        name: String,
        body: Vec<FortranStmt>,
    },
}

/// FORTRAN type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FortranType {
    Integer { kind: Option<i32> },
    Real { kind: Option<i32> },
    DoublePrecision,
    Complex { kind: Option<i32> },
    Logical { kind: Option<i32> },
    Character { len: Option<Box<FortranExpr>> },
    Derived(String),
}

impl Default for FortranType {
    fn default() -> Self {
        FortranType::Real { kind: None }
    }
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
        assignments: Vec<(String, FortranExpr)>,
    },
    Common {
        name: Option<String>,
        vars: Vec<String>,
    },
    Implicit(Option<Vec<(FortranType, char, char)>>), // None = IMPLICIT NONE
    External(Vec<String>),
    Dimension {
        name: String,
        dims: Vec<FortranExpr>,
    },
    Data {
        vars: Vec<String>,
        values: Vec<FortranExpr>,
    },
}

/// Intent attribute
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
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
        else_if_blocks: Vec<(FortranExpr, Vec<FortranStmt>)>,
        else_block: Option<Vec<FortranStmt>>,
    },
    ArithmeticIf {
        expr: FortranExpr,
        neg_label: i64,
        zero_label: i64,
        pos_label: i64,
    },
    Do {
        label: Option<i64>,
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
    Goto(i64),
    ComputedGoto {
        labels: Vec<i64>,
        index: FortranExpr,
    },
    Label(i64, Box<FortranStmt>),
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
    Print {
        format: Option<FortranExpr>,
        items: Vec<FortranExpr>,
    },
    Declaration(FortranDecl),
    Stop(Option<String>),
    Exit,
    Cycle,
    Comment(String),
}

/// FORTRAN expression
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
        start: Option<Box<FortranExpr>>,
        end: Option<Box<FortranExpr>>,
    },
    ArraySection {
        name: String,
        sections: Vec<ArraySection>,
    },
    ImpliedDo {
        expr: Box<FortranExpr>,
        var: String,
        start: Box<FortranExpr>,
        end: Box<FortranExpr>,
        step: Option<Box<FortranExpr>>,
    },
}

/// Array section specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ArraySection {
    Index(FortranExpr),
    Range {
        start: Option<FortranExpr>,
        end: Option<FortranExpr>,
        stride: Option<FortranExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum FortranBinOp {
    Add, Sub, Mul, Div, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Eqv, Neqv,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum FortranUnaryOp {
    Neg, Not, Plus,
}

// ============================================================================
// TOKEN PARSER
// ============================================================================

/// Token-based parser for FORTRAN
pub struct TokenParser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl TokenParser {
    /// Create a new token parser
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Get current token
    fn current(&self) -> Option<&FortranToken> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    /// Peek at token at offset
    fn peek(&self, offset: usize) -> Option<&FortranToken> {
        self.tokens.get(self.pos + offset).map(|t| &t.token)
    }

    /// Advance to next token
    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    /// Skip newlines and comments
    fn skip_newlines(&mut self) {
        while let Some(tok) = self.current() {
            match tok {
                FortranToken::Newline | FortranToken::Comment => self.advance(),
                _ => break,
            }
        }
    }

    /// Check if current token matches
    fn check(&self, expected: &FortranToken) -> bool {
        self.current() == Some(expected)
    }

    /// Expect and consume a specific token
    fn expect(&mut self, expected: &FortranToken) -> Result<()> {
        if self.current() == Some(expected) {
            self.advance();
            Ok(())
        } else {
            let (line, column) = self.tokens.get(self.pos)
                .map(|t| (t.line, t.column))
                .unwrap_or((1, 1));
            Err(rosetta_core::TranspileError::ParseError {
                message: format!("Expected {:?}, found {:?}", expected, self.current()),
                location: rosetta_core::SourceLocation::new(line, column, 0),
            })
        }
    }

    /// Try to consume a token, return true if successful
    fn try_consume(&mut self, expected: &FortranToken) -> bool {
        if self.check(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Get current identifier
    fn get_ident(&mut self) -> Result<String> {
        match self.current() {
            Some(FortranToken::Ident(name)) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => {
                let (line, column) = self.tokens.get(self.pos)
                    .map(|t| (t.line, t.column))
                    .unwrap_or((1, 1));
                Err(rosetta_core::TranspileError::ParseError {
                    message: "Expected identifier".to_string(),
                    location: rosetta_core::SourceLocation::new(line, column, 0),
                })
            }
        }
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<FortranAst> {
        self.skip_newlines();

        // Check for PROGRAM statement
        if self.check(&FortranToken::Program) {
            self.advance();
            let name = self.get_ident()?;
            self.skip_newlines();

            let mut body = Vec::new();
            let mut subprograms = Vec::new();

            // Parse body until END or CONTAINS
            while let Some(tok) = self.current() {
                match tok {
                    FortranToken::End => {
                        self.advance();
                        // Skip PROGRAM keyword if present
                        if self.check(&FortranToken::Program) {
                            self.advance();
                            // Skip program name if present
                            if matches!(self.current(), Some(FortranToken::Ident(_))) {
                                self.advance();
                            }
                        }
                        break;
                    }
                    FortranToken::Contains => {
                        self.advance();
                        self.skip_newlines();
                        // Parse contained subprograms
                        while let Some(tok) = self.current() {
                            match tok {
                                FortranToken::Subroutine => {
                                    subprograms.push(self.parse_subroutine()?);
                                }
                                FortranToken::Function => {
                                    subprograms.push(self.parse_function()?);
                                }
                                FortranToken::End => break,
                                _ => {
                                    self.skip_newlines();
                                    if !matches!(
                                        self.current(),
                                        Some(FortranToken::Subroutine)
                                            | Some(FortranToken::Function)
                                            | Some(FortranToken::End)
                                    ) {
                                        self.advance();
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        if let Some(stmt) = self.parse_statement()? {
                            body.push(stmt);
                        }
                    }
                }
            }

            Ok(FortranAst::Program {
                name,
                body,
                subprograms,
            })
        } else {
            // No PROGRAM statement - parse as implicit main
            let mut body = Vec::new();
            while self.current().is_some() {
                if let Some(stmt) = self.parse_statement()? {
                    body.push(stmt);
                }
            }
            Ok(FortranAst::Program {
                name: "main".to_string(),
                body,
                subprograms: Vec::new(),
            })
        }
    }

    /// Parse a subroutine
    fn parse_subroutine(&mut self) -> Result<FortranUnit> {
        self.expect(&FortranToken::Subroutine)?;
        let name = self.get_ident()?;

        // Parse parameters
        let params = if self.try_consume(&FortranToken::LParen) {
            let params = self.parse_param_list()?;
            self.expect(&FortranToken::RParen)?;
            params
        } else {
            Vec::new()
        };

        self.skip_newlines();

        // Parse body
        let body = self.parse_subprogram_body()?;

        Ok(FortranUnit::Subroutine { name, params, body })
    }

    /// Parse a function
    fn parse_function(&mut self) -> Result<FortranUnit> {
        // Check for return type prefix
        let return_type = self.parse_optional_type();

        self.expect(&FortranToken::Function)?;
        let name = self.get_ident()?;

        // Parse parameters
        let params = if self.try_consume(&FortranToken::LParen) {
            let params = self.parse_param_list()?;
            self.expect(&FortranToken::RParen)?;
            params
        } else {
            Vec::new()
        };

        self.skip_newlines();

        // Parse body
        let body = self.parse_subprogram_body()?;

        Ok(FortranUnit::Function {
            name,
            params,
            return_type,
            body,
        })
    }

    /// Parse optional type prefix for functions
    fn parse_optional_type(&mut self) -> Option<FortranType> {
        match self.current() {
            Some(FortranToken::Integer) => {
                self.advance();
                Some(FortranType::Integer { kind: self.parse_kind() })
            }
            Some(FortranToken::Real) => {
                self.advance();
                Some(FortranType::Real { kind: self.parse_kind() })
            }
            Some(FortranToken::Double) => {
                self.advance();
                let _ = self.try_consume(&FortranToken::Precision);
                Some(FortranType::DoublePrecision)
            }
            Some(FortranToken::Complex) => {
                self.advance();
                Some(FortranType::Complex { kind: self.parse_kind() })
            }
            Some(FortranToken::Logical) => {
                self.advance();
                Some(FortranType::Logical { kind: self.parse_kind() })
            }
            Some(FortranToken::Character) => {
                self.advance();
                let len = self.parse_char_len();
                Some(FortranType::Character { len })
            }
            _ => None,
        }
    }

    /// Parse kind specification
    fn parse_kind(&mut self) -> Option<i32> {
        if self.try_consume(&FortranToken::LParen) {
            if let Some(FortranToken::IntLit(k)) = self.current() {
                let kind = *k as i32;
                self.advance();
                let _ = self.expect(&FortranToken::RParen);
                return Some(kind);
            }
            // Handle KIND= syntax
            if matches!(self.current(), Some(FortranToken::Ident(s)) if s == "KIND") {
                self.advance();
                let _ = self.try_consume(&FortranToken::Assign);
                if let Some(FortranToken::IntLit(k)) = self.current() {
                    let kind = *k as i32;
                    self.advance();
                    let _ = self.expect(&FortranToken::RParen);
                    return Some(kind);
                }
            }
            let _ = self.expect(&FortranToken::RParen);
        }
        // Handle *N syntax (e.g., INTEGER*4)
        if self.try_consume(&FortranToken::Star) {
            if let Some(FortranToken::IntLit(k)) = self.current() {
                let kind = *k as i32;
                self.advance();
                return Some(kind);
            }
        }
        None
    }

    /// Parse character length specification
    fn parse_char_len(&mut self) -> Option<Box<FortranExpr>> {
        if self.try_consume(&FortranToken::LParen) {
            // Handle LEN= or just the length
            if matches!(self.current(), Some(FortranToken::Ident(s)) if s == "LEN") {
                self.advance();
                let _ = self.try_consume(&FortranToken::Assign);
            }
            if self.try_consume(&FortranToken::Star) {
                // CHARACTER(*) - assumed length
                let _ = self.expect(&FortranToken::RParen);
                return None;
            }
            let len = self.parse_expression().ok();
            let _ = self.expect(&FortranToken::RParen);
            return len.map(Box::new);
        }
        // Handle *N syntax
        if self.try_consume(&FortranToken::Star) {
            if let Some(FortranToken::IntLit(n)) = self.current() {
                let len = *n;
                self.advance();
                return Some(Box::new(FortranExpr::IntLit(len)));
            }
        }
        None
    }

    /// Parse parameter list
    fn parse_param_list(&mut self) -> Result<Vec<String>> {
        let mut params = Vec::new();
        if !self.check(&FortranToken::RParen) {
            params.push(self.get_ident()?);
            while self.try_consume(&FortranToken::Comma) {
                params.push(self.get_ident()?);
            }
        }
        Ok(params)
    }

    /// Parse subprogram body until END
    fn parse_subprogram_body(&mut self) -> Result<Vec<FortranStmt>> {
        let mut body = Vec::new();

        loop {
            self.skip_newlines();
            match self.current() {
                Some(FortranToken::End) => {
                    self.advance();
                    // Skip SUBROUTINE/FUNCTION keyword and name
                    if matches!(
                        self.current(),
                        Some(FortranToken::Subroutine) | Some(FortranToken::Function)
                    ) {
                        self.advance();
                        if matches!(self.current(), Some(FortranToken::Ident(_))) {
                            self.advance();
                        }
                    }
                    break;
                }
                None => break,
                _ => {
                    if let Some(stmt) = self.parse_statement()? {
                        body.push(stmt);
                    }
                }
            }
        }

        Ok(body)
    }

    /// Parse a single statement
    fn parse_statement(&mut self) -> Result<Option<FortranStmt>> {
        self.skip_newlines();

        // Check for label
        let label = if let Some(FortranToken::IntLit(n)) = self.current() {
            let n = *n;
            self.advance();
            Some(n)
        } else {
            None
        };

        let stmt = match self.current() {
            None => return Ok(None),
            Some(FortranToken::Newline) | Some(FortranToken::Comment) => {
                self.advance();
                return Ok(None);
            }

            // Declarations
            Some(FortranToken::Integer)
            | Some(FortranToken::Real)
            | Some(FortranToken::Double)
            | Some(FortranToken::Complex)
            | Some(FortranToken::Logical)
            | Some(FortranToken::Character) => {
                let decl = self.parse_type_declaration()?;
                FortranStmt::Declaration(decl)
            }

            Some(FortranToken::Implicit) => {
                self.advance();
                if self.try_consume(&FortranToken::None_) {
                    FortranStmt::Declaration(FortranDecl::Implicit(None))
                } else {
                    // TODO: Parse IMPLICIT type specifications
                    self.skip_to_newline();
                    FortranStmt::Declaration(FortranDecl::Implicit(None))
                }
            }

            Some(FortranToken::Parameter) => {
                self.advance();
                self.expect(&FortranToken::LParen)?;
                let mut assignments = Vec::new();
                loop {
                    let name = self.get_ident()?;
                    self.expect(&FortranToken::Assign)?;
                    let value = self.parse_expression()?;
                    assignments.push((name, value));
                    if !self.try_consume(&FortranToken::Comma) {
                        break;
                    }
                }
                self.expect(&FortranToken::RParen)?;
                FortranStmt::Declaration(FortranDecl::Parameter { assignments })
            }

            Some(FortranToken::Dimension) => {
                self.advance();
                let name = self.get_ident()?;
                self.expect(&FortranToken::LParen)?;
                let dims = self.parse_expression_list()?;
                self.expect(&FortranToken::RParen)?;
                FortranStmt::Declaration(FortranDecl::Dimension { name, dims })
            }

            Some(FortranToken::Common) => {
                self.advance();
                let block_name = if self.try_consume(&FortranToken::Slash) {
                    let name = if matches!(self.current(), Some(FortranToken::Ident(_))) {
                        Some(self.get_ident()?)
                    } else {
                        None
                    };
                    self.expect(&FortranToken::Slash)?;
                    name
                } else {
                    None
                };
                let mut vars = Vec::new();
                vars.push(self.get_ident()?);
                while self.try_consume(&FortranToken::Comma) {
                    vars.push(self.get_ident()?);
                }
                FortranStmt::Declaration(FortranDecl::Common {
                    name: block_name,
                    vars,
                })
            }

            Some(FortranToken::External) => {
                self.advance();
                let mut names = Vec::new();
                names.push(self.get_ident()?);
                while self.try_consume(&FortranToken::Comma) {
                    names.push(self.get_ident()?);
                }
                FortranStmt::Declaration(FortranDecl::External(names))
            }

            Some(FortranToken::Data) => {
                self.advance();
                let mut vars = Vec::new();
                let mut values = Vec::new();
                // Parse variable list
                vars.push(self.get_ident()?);
                while self.try_consume(&FortranToken::Comma) {
                    if self.check(&FortranToken::Slash) {
                        break;
                    }
                    vars.push(self.get_ident()?);
                }
                // Parse value list
                self.expect(&FortranToken::Slash)?;
                loop {
                    values.push(self.parse_expression()?);
                    if !self.try_consume(&FortranToken::Comma) {
                        break;
                    }
                }
                self.expect(&FortranToken::Slash)?;
                FortranStmt::Declaration(FortranDecl::Data { vars, values })
            }

            // Control flow
            Some(FortranToken::If) => self.parse_if_statement()?,

            Some(FortranToken::Do) => self.parse_do_statement()?,

            Some(FortranToken::Call) => {
                self.advance();
                let name = self.get_ident()?;
                let args = if self.try_consume(&FortranToken::LParen) {
                    let args = self.parse_expression_list()?;
                    self.expect(&FortranToken::RParen)?;
                    args
                } else {
                    Vec::new()
                };
                FortranStmt::Call { name, args }
            }

            Some(FortranToken::Return) => {
                self.advance();
                let expr = if !matches!(
                    self.current(),
                    Some(FortranToken::Newline) | Some(FortranToken::Comment) | None
                ) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                FortranStmt::Return(expr)
            }

            Some(FortranToken::Continue) => {
                self.advance();
                FortranStmt::Continue
            }

            Some(FortranToken::Goto) => {
                self.advance();
                if let Some(FortranToken::IntLit(n)) = self.current() {
                    let n = *n;
                    self.advance();
                    FortranStmt::Goto(n)
                } else if self.try_consume(&FortranToken::LParen) {
                    // Computed GOTO
                    let mut labels = Vec::new();
                    loop {
                        if let Some(FortranToken::IntLit(n)) = self.current() {
                            labels.push(*n);
                            self.advance();
                        }
                        if !self.try_consume(&FortranToken::Comma) {
                            break;
                        }
                    }
                    self.expect(&FortranToken::RParen)?;
                    let index = self.parse_expression()?;
                    FortranStmt::ComputedGoto { labels, index }
                } else {
                    let (line, column) = self.tokens.get(self.pos)
                        .map(|t| (t.line, t.column))
                        .unwrap_or((1, 1));
                    return Err(rosetta_core::TranspileError::ParseError {
                        message: "Expected label after GOTO".to_string(),
                        location: rosetta_core::SourceLocation::new(line, column, 0),
                    });
                }
            }

            Some(FortranToken::Stop) => {
                self.advance();
                let msg = match self.current() {
                    Some(FortranToken::StringLit(s)) | Some(FortranToken::StringLitDouble(s)) => {
                        let s = s.clone();
                        self.advance();
                        Some(s)
                    }
                    Some(FortranToken::IntLit(n)) => {
                        let s = n.to_string();
                        self.advance();
                        Some(s)
                    }
                    _ => None,
                };
                FortranStmt::Stop(msg)
            }

            Some(FortranToken::Exit) => {
                self.advance();
                FortranStmt::Exit
            }

            Some(FortranToken::Cycle) => {
                self.advance();
                FortranStmt::Cycle
            }

            // I/O
            Some(FortranToken::Write) => {
                self.advance();
                self.expect(&FortranToken::LParen)?;
                let unit = self.parse_expression()?;
                self.expect(&FortranToken::Comma)?;
                let format = if self.try_consume(&FortranToken::Star) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                self.expect(&FortranToken::RParen)?;
                let items = if !matches!(
                    self.current(),
                    Some(FortranToken::Newline) | Some(FortranToken::Comment) | None
                ) {
                    self.parse_expression_list()?
                } else {
                    Vec::new()
                };
                FortranStmt::Write {
                    unit,
                    format,
                    items,
                }
            }

            Some(FortranToken::Read) => {
                self.advance();
                // Check for simplified READ *, X syntax vs READ(unit, format)
                if self.try_consume(&FortranToken::LParen) {
                    let unit = self.parse_expression()?;
                    self.expect(&FortranToken::Comma)?;
                    let format = if self.try_consume(&FortranToken::Star) {
                        None
                    } else {
                        Some(self.parse_expression()?)
                    };
                    self.expect(&FortranToken::RParen)?;
                    let items = if !matches!(
                        self.current(),
                        Some(FortranToken::Newline) | Some(FortranToken::Comment) | None
                    ) {
                        self.parse_expression_list()?
                    } else {
                        Vec::new()
                    };
                    FortranStmt::Read {
                        unit,
                        format,
                        items,
                    }
                } else {
                    // Simplified READ *, X or READ format, X
                    let format = if self.try_consume(&FortranToken::Star) {
                        None
                    } else {
                        Some(self.parse_expression()?)
                    };
                    let _ = self.try_consume(&FortranToken::Comma);
                    let items = if !matches!(
                        self.current(),
                        Some(FortranToken::Newline) | Some(FortranToken::Comment) | None
                    ) {
                        self.parse_expression_list()?
                    } else {
                        Vec::new()
                    };
                    FortranStmt::Read {
                        unit: FortranExpr::Var("*".to_string()),
                        format,
                        items,
                    }
                }
            }

            Some(FortranToken::Print) => {
                self.advance();
                let format = if self.try_consume(&FortranToken::Star) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                let _ = self.try_consume(&FortranToken::Comma);
                let items = if !matches!(
                    self.current(),
                    Some(FortranToken::Newline) | Some(FortranToken::Comment) | None
                ) {
                    self.parse_expression_list()?
                } else {
                    Vec::new()
                };
                FortranStmt::Print { format, items }
            }

            Some(FortranToken::Format) => {
                // Skip FORMAT statements (handled by WRITE/READ)
                self.skip_to_newline();
                FortranStmt::Comment("FORMAT statement".to_string())
            }

            // Assignment or procedure call
            Some(FortranToken::Ident(_)) => {
                let target = self.parse_expression()?;
                if self.try_consume(&FortranToken::Assign) {
                    let value = self.parse_expression()?;
                    FortranStmt::Assignment { target, value }
                } else {
                    // Procedure call without CALL keyword (F90+)
                    match target {
                        FortranExpr::FunctionCall { name, args } => FortranStmt::Call { name, args },
                        FortranExpr::Var(name) => FortranStmt::Call {
                            name,
                            args: Vec::new(),
                        },
                        _ => {
                            let (line, column) = self.tokens.get(self.pos)
                                .map(|t| (t.line, t.column))
                                .unwrap_or((1, 1));
                            return Err(rosetta_core::TranspileError::ParseError {
                                message: "Expected assignment or call".to_string(),
                                location: rosetta_core::SourceLocation::new(line, column, 0),
                            })
                        }
                    }
                }
            }

            _ => {
                // Skip unknown tokens
                self.advance();
                return Ok(None);
            }
        };

        // Handle labeled statements
        let stmt = if let Some(lbl) = label {
            FortranStmt::Label(lbl, Box::new(stmt))
        } else {
            stmt
        };

        self.skip_newlines();
        Ok(Some(stmt))
    }

    /// Parse type declaration
    fn parse_type_declaration(&mut self) -> Result<FortranDecl> {
        let ty = self.parse_type()?;

        // Check for :: (F90+ style)
        let has_double_colon = self.try_consume(&FortranToken::DoubleColon);

        // Parse intent if present
        let intent = if has_double_colon {
            None // Intent would be before ::
        } else {
            None
        };

        // Parse variable list
        let mut names = Vec::new();
        loop {
            let name = self.get_ident()?;
            let dims = if self.try_consume(&FortranToken::LParen) {
                let dims = self.parse_expression_list()?;
                self.expect(&FortranToken::RParen)?;
                Some(dims)
            } else {
                None
            };

            // Handle initialization
            if self.try_consume(&FortranToken::Assign) {
                let _init = self.parse_expression()?;
                // TODO: Store initialization
            }

            names.push((name, dims));
            if !self.try_consume(&FortranToken::Comma) {
                break;
            }
        }

        Ok(FortranDecl::Variable { ty, names, intent })
    }

    /// Parse a type specification
    fn parse_type(&mut self) -> Result<FortranType> {
        match self.current() {
            Some(FortranToken::Integer) => {
                self.advance();
                Ok(FortranType::Integer {
                    kind: self.parse_kind(),
                })
            }
            Some(FortranToken::Real) => {
                self.advance();
                Ok(FortranType::Real {
                    kind: self.parse_kind(),
                })
            }
            Some(FortranToken::Double) => {
                self.advance();
                let _ = self.try_consume(&FortranToken::Precision);
                Ok(FortranType::DoublePrecision)
            }
            Some(FortranToken::Complex) => {
                self.advance();
                Ok(FortranType::Complex {
                    kind: self.parse_kind(),
                })
            }
            Some(FortranToken::Logical) => {
                self.advance();
                Ok(FortranType::Logical {
                    kind: self.parse_kind(),
                })
            }
            Some(FortranToken::Character) => {
                self.advance();
                Ok(FortranType::Character {
                    len: self.parse_char_len(),
                })
            }
            _ => {
                let (line, column) = self.tokens.get(self.pos)
                    .map(|t| (t.line, t.column))
                    .unwrap_or((1, 1));
                Err(rosetta_core::TranspileError::ParseError {
                    message: "Expected type".to_string(),
                    location: rosetta_core::SourceLocation::new(line, column, 0),
                })
            }
        }
    }

    /// Parse IF statement
    fn parse_if_statement(&mut self) -> Result<FortranStmt> {
        self.expect(&FortranToken::If)?;
        self.expect(&FortranToken::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(&FortranToken::RParen)?;

        // Check for arithmetic IF
        if let Some(FortranToken::IntLit(_)) = self.current() {
            let neg_label = if let Some(FortranToken::IntLit(n)) = self.current() {
                let n = *n;
                self.advance();
                n
            } else {
                0
            };
            self.expect(&FortranToken::Comma)?;
            let zero_label = if let Some(FortranToken::IntLit(n)) = self.current() {
                let n = *n;
                self.advance();
                n
            } else {
                0
            };
            self.expect(&FortranToken::Comma)?;
            let pos_label = if let Some(FortranToken::IntLit(n)) = self.current() {
                let n = *n;
                self.advance();
                n
            } else {
                0
            };
            return Ok(FortranStmt::ArithmeticIf {
                expr: condition,
                neg_label,
                zero_label,
                pos_label,
            });
        }

        // Check for single-line IF
        if !self.try_consume(&FortranToken::Then) {
            // Single-line IF: IF (cond) stmt
            if let Some(stmt) = self.parse_statement()? {
                return Ok(FortranStmt::If {
                    condition,
                    then_block: vec![stmt],
                    else_if_blocks: Vec::new(),
                    else_block: None,
                });
            }
        }

        self.skip_newlines();

        // Block IF
        let mut then_block = Vec::new();
        let mut else_if_blocks = Vec::new();
        let mut else_block = None;

        loop {
            match self.current() {
                Some(FortranToken::End) | Some(FortranToken::EndIf) => {
                    self.advance();
                    if self.check(&FortranToken::If) {
                        self.advance();
                    }
                    break;
                }
                Some(FortranToken::Else) => {
                    self.advance();
                    if self.try_consume(&FortranToken::If) || self.check(&FortranToken::ElseIf) {
                        if self.check(&FortranToken::ElseIf) {
                            self.advance();
                        }
                        // ELSE IF
                        self.expect(&FortranToken::LParen)?;
                        let cond = self.parse_expression()?;
                        self.expect(&FortranToken::RParen)?;
                        self.expect(&FortranToken::Then)?;
                        self.skip_newlines();

                        let mut block = Vec::new();
                        loop {
                            match self.current() {
                                Some(FortranToken::End)
                                | Some(FortranToken::EndIf)
                                | Some(FortranToken::Else)
                                | Some(FortranToken::ElseIf) => break,
                                _ => {
                                    if let Some(stmt) = self.parse_statement()? {
                                        block.push(stmt);
                                    }
                                }
                            }
                        }
                        else_if_blocks.push((cond, block));
                    } else {
                        // Plain ELSE
                        self.skip_newlines();
                        let mut block = Vec::new();
                        loop {
                            match self.current() {
                                Some(FortranToken::End) | Some(FortranToken::EndIf) => break,
                                _ => {
                                    if let Some(stmt) = self.parse_statement()? {
                                        block.push(stmt);
                                    }
                                }
                            }
                        }
                        else_block = Some(block);
                    }
                }
                Some(FortranToken::ElseIf) => {
                    self.advance();
                    self.expect(&FortranToken::LParen)?;
                    let cond = self.parse_expression()?;
                    self.expect(&FortranToken::RParen)?;
                    self.expect(&FortranToken::Then)?;
                    self.skip_newlines();

                    let mut block = Vec::new();
                    loop {
                        match self.current() {
                            Some(FortranToken::End)
                            | Some(FortranToken::EndIf)
                            | Some(FortranToken::Else)
                            | Some(FortranToken::ElseIf) => break,
                            _ => {
                                if let Some(stmt) = self.parse_statement()? {
                                    block.push(stmt);
                                }
                            }
                        }
                    }
                    else_if_blocks.push((cond, block));
                }
                None => break,
                _ => {
                    if let Some(stmt) = self.parse_statement()? {
                        then_block.push(stmt);
                    }
                }
            }
        }

        Ok(FortranStmt::If {
            condition,
            then_block,
            else_if_blocks,
            else_block,
        })
    }

    /// Parse DO statement
    fn parse_do_statement(&mut self) -> Result<FortranStmt> {
        self.expect(&FortranToken::Do)?;

        // Check for label
        let label = if let Some(FortranToken::IntLit(n)) = self.current() {
            let n = *n;
            self.advance();
            Some(n)
        } else {
            None
        };

        // Check for DO WHILE
        if self.try_consume(&FortranToken::While) {
            self.expect(&FortranToken::LParen)?;
            let condition = self.parse_expression()?;
            self.expect(&FortranToken::RParen)?;
            self.skip_newlines();

            let body = self.parse_do_body(label)?;
            return Ok(FortranStmt::DoWhile { condition, body });
        }

        // Regular DO loop
        let var = self.get_ident()?;
        self.expect(&FortranToken::Assign)?;
        let start = self.parse_expression()?;
        self.expect(&FortranToken::Comma)?;
        let end = self.parse_expression()?;
        let step = if self.try_consume(&FortranToken::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.skip_newlines();
        let body = self.parse_do_body(label)?;

        Ok(FortranStmt::Do {
            label,
            var,
            start,
            end,
            step,
            body,
        })
    }

    /// Parse DO loop body
    fn parse_do_body(&mut self, label: Option<i64>) -> Result<Vec<FortranStmt>> {
        let mut body = Vec::new();

        loop {
            match self.current() {
                Some(FortranToken::End) | Some(FortranToken::EndDo) => {
                    self.advance();
                    if self.check(&FortranToken::Do) {
                        self.advance();
                    }
                    break;
                }
                None => break,
                _ => {
                    // Check for labeled CONTINUE (end of F77 DO loop)
                    if let Some(FortranToken::IntLit(n)) = self.current() {
                        if label == Some(*n) {
                            self.advance();
                            if self.try_consume(&FortranToken::Continue) {
                                break;
                            }
                        }
                    }
                    if let Some(stmt) = self.parse_statement()? {
                        body.push(stmt);
                    }
                }
            }
        }

        Ok(body)
    }

    /// Parse expression list
    fn parse_expression_list(&mut self) -> Result<Vec<FortranExpr>> {
        let mut exprs = Vec::new();
        if !self.check(&FortranToken::RParen) {
            exprs.push(self.parse_expression()?);
            while self.try_consume(&FortranToken::Comma) {
                exprs.push(self.parse_expression()?);
            }
        }
        Ok(exprs)
    }

    /// Parse expression (with precedence climbing)
    fn parse_expression(&mut self) -> Result<FortranExpr> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<FortranExpr> {
        let mut left = self.parse_and_expr()?;
        while self.try_consume(&FortranToken::Or) {
            let right = self.parse_and_expr()?;
            left = FortranExpr::BinOp {
                op: FortranBinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<FortranExpr> {
        let mut left = self.parse_not_expr()?;
        while self.try_consume(&FortranToken::And) {
            let right = self.parse_not_expr()?;
            left = FortranExpr::BinOp {
                op: FortranBinOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_not_expr(&mut self) -> Result<FortranExpr> {
        if self.try_consume(&FortranToken::Not) {
            let operand = self.parse_not_expr()?;
            Ok(FortranExpr::UnaryOp {
                op: FortranUnaryOp::Not,
                operand: Box::new(operand),
            })
        } else {
            self.parse_comparison()
        }
    }

    fn parse_comparison(&mut self) -> Result<FortranExpr> {
        let left = self.parse_concat()?;

        let op = match self.current() {
            Some(FortranToken::Eq) | Some(FortranToken::EqEq) => Some(FortranBinOp::Eq),
            Some(FortranToken::Ne) | Some(FortranToken::SlashEq) => Some(FortranBinOp::Ne),
            Some(FortranToken::Lt) | Some(FortranToken::LtSym) => Some(FortranBinOp::Lt),
            Some(FortranToken::Le) | Some(FortranToken::LeSym) => Some(FortranBinOp::Le),
            Some(FortranToken::Gt) | Some(FortranToken::GtSym) => Some(FortranBinOp::Gt),
            Some(FortranToken::Ge) | Some(FortranToken::GeSym) => Some(FortranBinOp::Ge),
            Some(FortranToken::Eqv) => Some(FortranBinOp::Eqv),
            Some(FortranToken::Neqv) => Some(FortranBinOp::Neqv),
            _ => None,
        };

        if let Some(op) = op {
            self.advance();
            let right = self.parse_concat()?;
            Ok(FortranExpr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    fn parse_concat(&mut self) -> Result<FortranExpr> {
        let mut left = self.parse_additive()?;
        while self.try_consume(&FortranToken::Concat) {
            let right = self.parse_additive()?;
            left = FortranExpr::BinOp {
                op: FortranBinOp::Concat,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<FortranExpr> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let op = match self.current() {
                Some(FortranToken::Plus) => Some(FortranBinOp::Add),
                Some(FortranToken::Minus) => Some(FortranBinOp::Sub),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let right = self.parse_multiplicative()?;
                left = FortranExpr::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<FortranExpr> {
        let mut left = self.parse_power()?;
        loop {
            let op = match self.current() {
                Some(FortranToken::Star) => Some(FortranBinOp::Mul),
                Some(FortranToken::Slash) => Some(FortranBinOp::Div),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let right = self.parse_power()?;
                left = FortranExpr::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_power(&mut self) -> Result<FortranExpr> {
        let left = self.parse_unary()?;
        if self.try_consume(&FortranToken::Power) {
            let right = self.parse_power()?; // Right associative
            Ok(FortranExpr::BinOp {
                op: FortranBinOp::Power,
                left: Box::new(left),
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    fn parse_unary(&mut self) -> Result<FortranExpr> {
        match self.current() {
            Some(FortranToken::Plus) => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(FortranExpr::UnaryOp {
                    op: FortranUnaryOp::Plus,
                    operand: Box::new(operand),
                })
            }
            Some(FortranToken::Minus) => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(FortranExpr::UnaryOp {
                    op: FortranUnaryOp::Neg,
                    operand: Box::new(operand),
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<FortranExpr> {
        match self.current() {
            Some(FortranToken::IntLit(n)) => {
                let n = *n;
                self.advance();
                Ok(FortranExpr::IntLit(n))
            }
            Some(FortranToken::RealLit(r)) | Some(FortranToken::RealLitDot(r)) => {
                let r = *r;
                self.advance();
                Ok(FortranExpr::RealLit(r))
            }
            Some(FortranToken::StringLit(s)) | Some(FortranToken::StringLitDouble(s)) => {
                let s = s.clone();
                self.advance();
                Ok(FortranExpr::StringLit(s))
            }
            Some(FortranToken::True) => {
                self.advance();
                Ok(FortranExpr::LogicalLit(true))
            }
            Some(FortranToken::False) => {
                self.advance();
                Ok(FortranExpr::LogicalLit(false))
            }
            Some(FortranToken::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&FortranToken::RParen)?;
                Ok(expr)
            }
            Some(FortranToken::Ident(name)) => {
                let name = name.clone();
                self.advance();

                // Check for array reference or function call
                if self.try_consume(&FortranToken::LParen) {
                    let args = self.parse_expression_list()?;
                    self.expect(&FortranToken::RParen)?;

                    // Check for substring
                    if self.try_consume(&FortranToken::LParen) {
                        let start = if self.check(&FortranToken::Colon) {
                            None
                        } else {
                            Some(Box::new(self.parse_expression()?))
                        };
                        self.expect(&FortranToken::Colon)?;
                        let end = if self.check(&FortranToken::RParen) {
                            None
                        } else {
                            Some(Box::new(self.parse_expression()?))
                        };
                        self.expect(&FortranToken::RParen)?;
                        return Ok(FortranExpr::Substring {
                            var: Box::new(FortranExpr::ArrayRef {
                                name,
                                indices: args,
                            }),
                            start,
                            end,
                        });
                    }

                    // Determine if array or function based on context
                    // (In real parser, would use symbol table)
                    Ok(FortranExpr::FunctionCall { name, args })
                } else {
                    Ok(FortranExpr::Var(name))
                }
            }
            Some(FortranToken::Star) => {
                // Could be format specifier
                self.advance();
                Ok(FortranExpr::Var("*".to_string()))
            }
            _ => {
                let (line, column) = self.tokens.get(self.pos)
                    .map(|t| (t.line, t.column))
                    .unwrap_or((1, 1));
                Err(rosetta_core::TranspileError::ParseError {
                    message: format!("Unexpected token in expression: {:?}", self.current()),
                    location: rosetta_core::SourceLocation::new(line, column, 0),
                })
            }
        }
    }

    /// Skip to end of line
    fn skip_to_newline(&mut self) {
        while let Some(tok) = self.current() {
            if matches!(tok, FortranToken::Newline) {
                break;
            }
            self.advance();
        }
    }
}

// ============================================================================
// FORTRAN PARSER (Main Interface)
// ============================================================================

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

    /// Create parser for specific version
    pub fn new(version: SourceLanguage) -> Self {
        let fixed_format = matches!(version, SourceLanguage::Fortran77);
        Self { version, fixed_format }
    }

    /// Preprocess fixed-format source
    fn preprocess_fixed(&self, source: &str) -> String {
        let mut result = String::new();
        let mut prev_continued = false;

        for line in source.lines() {
            // Handle empty lines
            if line.trim().is_empty() {
                if !prev_continued {
                    result.push('\n');
                }
                continue;
            }

            // Handle comment lines (C, c, *, or !)
            let first_char = line.chars().next().unwrap_or(' ');
            if first_char == 'C' || first_char == 'c' || first_char == '*' || first_char == '!' {
                result.push_str("! ");
                result.push_str(line.get(1..).unwrap_or(""));
                result.push('\n');
                prev_continued = false;
                continue;
            }

            // Get the line content
            let line_len = line.len();

            // Handle continuation (column 6)
            if line_len >= 6 {
                let col6 = line.chars().nth(5).unwrap_or(' ');
                if col6 != ' ' && col6 != '0' {
                    // Continuation line - append to previous
                    if result.ends_with('\n') {
                        result.pop();
                    }
                    result.push(' ');
                    if line_len > 6 {
                        result.push_str(line.get(6..).unwrap_or("").trim_end());
                    }
                    prev_continued = true;
                    continue;
                }
            }

            // New statement
            prev_continued = false;

            // Extract label if present (columns 1-5)
            if line_len >= 5 {
                let label = line.get(..5).unwrap_or("").trim();
                if !label.is_empty() && label.chars().all(|c| c.is_ascii_digit()) {
                    result.push_str(label);
                    result.push(' ');
                }
            }

            // Statement content (columns 7-72)
            if line_len > 6 {
                let end = std::cmp::min(line_len, 72);
                let stmt = line.get(6..end).unwrap_or("").trim_end();
                result.push_str(stmt);
            }
            result.push('\n');
        }

        result
    }

    /// Tokenize source code
    fn tokenize(&self, source: &str) -> Result<Vec<SpannedToken>> {
        let mut tokens = Vec::new();
        let mut line = 1;
        let mut col = 1;
        let mut lexer = FortranToken::lexer(source);

        while let Some(result) = lexer.next() {
            match result {
                Ok(token) => {
                    // Update position for newlines
                    if matches!(token, FortranToken::Newline) {
                        tokens.push(SpannedToken {
                            token,
                            line,
                            column: col,
                        });
                        line += 1;
                        col = 1;
                    } else if !matches!(token, FortranToken::Comment) {
                        tokens.push(SpannedToken {
                            token,
                            line,
                            column: col,
                        });
                        col += lexer.span().len();
                    }
                }
                Err(_) => {
                    // Skip invalid tokens
                    col += lexer.span().len();
                }
            }
        }

        Ok(tokens)
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
        // Preprocess if fixed format
        let processed = if self.fixed_format {
            self.preprocess_fixed(&source.content)
        } else {
            source.content.clone()
        };

        // Tokenize
        let tokens = self.tokenize(&processed).map_err(|e| {
            ParseError::SemanticError(format!("Tokenization failed: {}", e))
        })?;

        // Parse tokens into AST
        let mut parser = TokenParser::new(tokens);
        let ast = parser.parse_program().map_err(|e| {
            ParseError::SemanticError(format!("Parse failed: {}", e))
        })?;

        // Convert AST to IR
        let module = FortranToIr::convert(&ast, self.version).map_err(|e| {
            ParseError::SemanticError(e.to_string())
        })?;

        Ok(module.into_rosetta_ir())
    }
}

// ============================================================================
// AST TO IR CONVERSION
// ============================================================================

/// Convert FORTRAN AST to Rosetta IR
pub struct FortranToIr;

impl FortranToIr {
    /// Convert FORTRAN AST to IR module
    pub fn convert(ast: &FortranAst, lang: SourceLanguage) -> Result<IrModule> {
        match ast {
            FortranAst::Program { name, body, subprograms } => {
                let mut builder = IrBuilder::with_language(name, lang);

                // Convert main program body
                if !body.is_empty() {
                    let main_body = Self::convert_statements(body)?;
                    let main_func = IrFunction {
                        name: "main".to_string(),
                        generics: vec![],
                        params: vec![],
                        return_type: IrType::Unit,
                        body: main_body,
                        is_unsafe: false,
                        visibility: Visibility::Public,
                        attributes: vec![],
                        span: None,
                    };
                    builder.add_function(main_func);
                }

                // Convert subprograms
                for unit in subprograms {
                    let func = Self::convert_unit(unit)?;
                    builder.add_function(func);
                }

                Ok(builder.build())
            }
            FortranAst::Module { name, declarations: _, contains } => {
                let mut builder = IrBuilder::with_language(name, lang);

                for unit in contains {
                    let func = Self::convert_unit(unit)?;
                    builder.add_function(func);
                }

                Ok(builder.build())
            }
        }
    }

    /// Convert a program unit to IR function
    fn convert_unit(unit: &FortranUnit) -> Result<IrFunction> {
        match unit {
            FortranUnit::Subroutine { name, params, body } => {
                let ir_params: Vec<IrParam> = params
                    .iter()
                    .map(|p| IrParam {
                        name: p.to_lowercase(),
                        ty: IrType::Any, // Type inference needed
                        is_mutable: true,
                        by_ref: true, // FORTRAN passes by reference
                    })
                    .collect();

                let ir_body = Self::convert_statements(body)?;

                Ok(IrFunction {
                    name: name.to_lowercase(),
                    generics: vec![],
                    params: ir_params,
                    return_type: IrType::Unit,
                    body: ir_body,
                    is_unsafe: false,
                    visibility: Visibility::Public,
                    attributes: vec![],
                    span: None,
                })
            }
            FortranUnit::Function { name, params, return_type, body } => {
                let ir_params: Vec<IrParam> = params
                    .iter()
                    .map(|p| IrParam {
                        name: p.to_lowercase(),
                        ty: IrType::Any,
                        is_mutable: true,
                        by_ref: true,
                    })
                    .collect();

                let ir_return = return_type
                    .as_ref()
                    .map(Self::convert_type)
                    .unwrap_or(IrType::Any);

                let ir_body = Self::convert_statements(body)?;

                Ok(IrFunction {
                    name: name.to_lowercase(),
                    generics: vec![],
                    params: ir_params,
                    return_type: ir_return,
                    body: ir_body,
                    is_unsafe: false,
                    visibility: Visibility::Public,
                    attributes: vec![],
                    span: None,
                })
            }
            FortranUnit::Program { name, body } => {
                let ir_body = Self::convert_statements(body)?;

                Ok(IrFunction {
                    name: name.to_lowercase(),
                    generics: vec![],
                    params: vec![],
                    return_type: IrType::Unit,
                    body: ir_body,
                    is_unsafe: false,
                    visibility: Visibility::Public,
                    attributes: vec![],
                    span: None,
                })
            }
        }
    }

    /// Convert FORTRAN type to IR type
    fn convert_type(ty: &FortranType) -> IrType {
        match ty {
            FortranType::Integer { kind } => {
                let bits = match kind {
                    Some(1) => 8,
                    Some(2) => 16,
                    Some(4) | None => 32,
                    Some(8) => 64,
                    _ => 32,
                };
                IrType::Int(bits)
            }
            FortranType::Real { kind } => {
                let bits = match kind {
                    Some(4) | None => 32,
                    Some(8) => 64,
                    _ => 32,
                };
                IrType::Float(bits)
            }
            FortranType::DoublePrecision => IrType::Float(64),
            FortranType::Complex { .. } => IrType::Struct("Complex".to_string()),
            FortranType::Logical { .. } => IrType::Bool,
            FortranType::Character { .. } => IrType::String,
            FortranType::Derived(name) => IrType::Struct(name.clone()),
        }
    }

    /// Convert statements to IR expressions
    fn convert_statements(stmts: &[FortranStmt]) -> Result<Vec<IrExpr>> {
        let mut result = Vec::new();

        for stmt in stmts {
            if let Some(ir) = Self::convert_statement(stmt)? {
                result.push(ir);
            }
        }

        Ok(result)
    }

    /// Convert a single statement to IR expression
    fn convert_statement(stmt: &FortranStmt) -> Result<Option<IrExpr>> {
        match stmt {
            FortranStmt::Assignment { target, value } => {
                let target_name = Self::expr_to_string(target);
                let ir_value = Self::convert_expr(value)?;
                Ok(Some(IrExpr::Assign {
                    target: target_name,
                    value: Box::new(ir_value),
                }))
            }

            FortranStmt::If { condition, then_block, else_if_blocks, else_block } => {
                let ir_cond = Self::convert_expr(condition)?;
                let ir_then = Self::convert_statements(then_block)?;

                // Build nested if-else chain for else-if blocks
                let mut current_else: Option<Box<IrExpr>> = else_block
                    .as_ref()
                    .map(|b| Self::convert_statements(b))
                    .transpose()?
                    .map(|stmts| Box::new(IrExpr::Block(stmts)));

                // Process else-if blocks in reverse order
                for (cond, block) in else_if_blocks.iter().rev() {
                    let elif_cond = Self::convert_expr(cond)?;
                    let elif_body = Self::convert_statements(block)?;
                    current_else = Some(Box::new(IrExpr::If {
                        condition: Box::new(elif_cond),
                        then_branch: Box::new(IrExpr::Block(elif_body)),
                        else_branch: current_else,
                    }));
                }

                Ok(Some(IrExpr::If {
                    condition: Box::new(ir_cond),
                    then_branch: Box::new(IrExpr::Block(ir_then)),
                    else_branch: current_else,
                }))
            }

            FortranStmt::Do { var, start, end, step, body, .. } => {
                // Convert DO loop to a block with loop variable initialization
                // and a simulated loop using recursion pattern
                let var_name = var.to_lowercase();
                let ir_start = Self::convert_expr(start)?;
                let ir_end = Self::convert_expr(end)?;
                let ir_step = step.as_ref()
                    .map(Self::convert_expr)
                    .transpose()?
                    .unwrap_or(IrExpr::Int(1));
                let ir_body = Self::convert_statements(body)?;

                // Create: for var in start..=end { body }
                // Using Block with comments for now
                let mut loop_block = vec![
                    IrExpr::Comment(format!(
                        "DO {} = {} TO {} STEP {}",
                        var_name,
                        Self::ir_expr_to_string(&ir_start),
                        Self::ir_expr_to_string(&ir_end),
                        Self::ir_expr_to_string(&ir_step)
                    )),
                ];
                loop_block.push(IrExpr::Assign {
                    target: var_name.clone(),
                    value: Box::new(ir_start.clone()),
                });
                loop_block.extend(ir_body);

                Ok(Some(IrExpr::Block(loop_block)))
            }

            FortranStmt::DoWhile { condition, body } => {
                let ir_cond = Self::convert_expr(condition)?;
                let ir_body = Self::convert_statements(body)?;

                let mut loop_block = vec![
                    IrExpr::Comment(format!(
                        "DO WHILE ({})",
                        Self::ir_expr_to_string(&ir_cond)
                    )),
                ];
                loop_block.extend(ir_body);

                Ok(Some(IrExpr::Block(loop_block)))
            }

            FortranStmt::Call { name, args } => {
                let ir_args: Vec<IrExpr> = args
                    .iter()
                    .map(Self::convert_expr)
                    .collect::<Result<_>>()?;

                Ok(Some(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier(name.to_lowercase())),
                    args: ir_args,
                }))
            }

            FortranStmt::Return(expr) => {
                let ir_expr = expr
                    .as_ref()
                    .map(Self::convert_expr)
                    .transpose()?
                    .unwrap_or(IrExpr::Nil);
                Ok(Some(IrExpr::Return(Box::new(ir_expr))))
            }

            FortranStmt::Write { items, .. } => {
                // Convert WRITE to print! macro call
                let ir_args: Vec<IrExpr> = items
                    .iter()
                    .map(Self::convert_expr)
                    .collect::<Result<_>>()?;

                Ok(Some(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier("println".to_string())),
                    args: ir_args,
                }))
            }

            FortranStmt::Print { items, .. } => {
                let ir_args: Vec<IrExpr> = items
                    .iter()
                    .map(Self::convert_expr)
                    .collect::<Result<_>>()?;

                Ok(Some(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier("println".to_string())),
                    args: ir_args,
                }))
            }

            FortranStmt::Continue => {
                Ok(Some(IrExpr::Comment("CONTINUE".to_string())))
            }

            FortranStmt::Goto(label) => {
                Ok(Some(IrExpr::Comment(format!("GOTO {}", label))))
            }

            FortranStmt::Stop(msg) => {
                let msg = msg.as_ref().map(|s| s.as_str()).unwrap_or("");
                Ok(Some(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier("std::process::exit".to_string())),
                    args: vec![IrExpr::Int(0)],
                }))
            }

            FortranStmt::Label(label, inner) => {
                // Convert labeled statement
                let mut result = vec![IrExpr::Comment(format!("Label {}", label))];
                if let Some(ir) = Self::convert_statement(inner)? {
                    result.push(ir);
                }
                Ok(Some(IrExpr::Block(result)))
            }

            FortranStmt::Declaration(decl) => {
                match decl {
                    FortranDecl::Variable { ty, names, .. } => {
                        let ir_ty = Self::convert_type(ty);
                        let mut lets = Vec::new();
                        for (name, dims) in names {
                            let init = if dims.is_some() {
                                IrExpr::List(vec![])
                            } else {
                                match &ir_ty {
                                    IrType::Int(_) => IrExpr::Int(0),
                                    IrType::Float(_) => IrExpr::Float(0.0),
                                    IrType::Bool => IrExpr::Bool(false),
                                    IrType::String => IrExpr::String(String::new()),
                                    _ => IrExpr::Nil,
                                }
                            };
                            lets.push(IrExpr::Assign {
                                target: name.to_lowercase(),
                                value: Box::new(init),
                            });
                        }
                        Ok(Some(IrExpr::Block(lets)))
                    }
                    FortranDecl::Parameter { assignments } => {
                        let mut consts = Vec::new();
                        for (name, value) in assignments {
                            let ir_value = Self::convert_expr(value)?;
                            consts.push(IrExpr::Assign {
                                target: name.to_lowercase(),
                                value: Box::new(ir_value),
                            });
                        }
                        Ok(Some(IrExpr::Block(consts)))
                    }
                    _ => Ok(None),
                }
            }

            FortranStmt::Comment(text) => {
                Ok(Some(IrExpr::Comment(text.clone())))
            }

            _ => Ok(None),
        }
    }

    /// Convert FORTRAN expression to IR expression
    fn convert_expr(expr: &FortranExpr) -> Result<IrExpr> {
        match expr {
            FortranExpr::IntLit(n) => Ok(IrExpr::Int(*n)),
            FortranExpr::RealLit(r) => Ok(IrExpr::Float(*r)),
            FortranExpr::StringLit(s) => Ok(IrExpr::String(s.clone())),
            FortranExpr::LogicalLit(b) => Ok(IrExpr::Bool(*b)),
            FortranExpr::Var(name) => Ok(IrExpr::Identifier(name.to_lowercase())),

            FortranExpr::ArrayRef { name, indices } => {
                let ir_indices: Vec<IrExpr> = indices
                    .iter()
                    .map(Self::convert_expr)
                    .collect::<Result<_>>()?;

                // Convert to index operation
                Ok(IrExpr::Index {
                    array: Box::new(IrExpr::Identifier(name.to_lowercase())),
                    index: Box::new(if ir_indices.len() == 1 {
                        ir_indices.into_iter().next().unwrap()
                    } else {
                        IrExpr::List(ir_indices)
                    }),
                })
            }

            FortranExpr::FunctionCall { name, args } => {
                let ir_args: Vec<IrExpr> = args
                    .iter()
                    .map(Self::convert_expr)
                    .collect::<Result<_>>()?;

                // Handle intrinsic functions
                let func_name = Self::convert_intrinsic(name);

                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier(func_name)),
                    args: ir_args,
                })
            }

            FortranExpr::BinOp { op, left, right } => {
                let ir_left = Self::convert_expr(left)?;
                let ir_right = Self::convert_expr(right)?;
                let op_str = match op {
                    FortranBinOp::Add => "+",
                    FortranBinOp::Sub => "-",
                    FortranBinOp::Mul => "*",
                    FortranBinOp::Div => "/",
                    FortranBinOp::Power => "**",
                    FortranBinOp::Eq => "==",
                    FortranBinOp::Ne => "!=",
                    FortranBinOp::Lt => "<",
                    FortranBinOp::Le => "<=",
                    FortranBinOp::Gt => ">",
                    FortranBinOp::Ge => ">=",
                    FortranBinOp::And => "&&",
                    FortranBinOp::Or => "||",
                    FortranBinOp::Eqv => "==",
                    FortranBinOp::Neqv => "!=",
                    FortranBinOp::Concat => "+",
                };

                Ok(IrExpr::BinaryOp {
                    op: op_str.to_string(),
                    left: Box::new(ir_left),
                    right: Box::new(ir_right),
                })
            }

            FortranExpr::UnaryOp { op, operand } => {
                let ir_operand = Self::convert_expr(operand)?;
                let op_str = match op {
                    FortranUnaryOp::Neg => "-",
                    FortranUnaryOp::Not => "!",
                    FortranUnaryOp::Plus => "+",
                };

                Ok(IrExpr::UnaryOp {
                    op: op_str.to_string(),
                    operand: Box::new(ir_operand),
                })
            }

            FortranExpr::Substring { var, start, end } => {
                let ir_var = Self::convert_expr(var)?;
                let ir_start = start.as_ref()
                    .map(|e| Self::convert_expr(e))
                    .transpose()?
                    .unwrap_or(IrExpr::Int(0));
                let ir_end = end.as_ref()
                    .map(|e| Self::convert_expr(e))
                    .transpose()?;

                // Convert to slice indexing
                Ok(IrExpr::Index {
                    array: Box::new(ir_var),
                    index: Box::new(IrExpr::List(vec![ir_start, ir_end.unwrap_or(IrExpr::Nil)])),
                })
            }

            _ => Ok(IrExpr::Nil),
        }
    }

    /// Convert FORTRAN intrinsic function name to Rust equivalent
    fn convert_intrinsic(name: &str) -> String {
        match name.to_uppercase().as_str() {
            // Math functions
            "ABS" | "IABS" | "DABS" => "abs".to_string(),
            "SQRT" | "DSQRT" => "sqrt".to_string(),
            "EXP" | "DEXP" => "exp".to_string(),
            "LOG" | "DLOG" | "ALOG" => "ln".to_string(),
            "LOG10" | "DLOG10" | "ALOG10" => "log10".to_string(),
            "SIN" | "DSIN" => "sin".to_string(),
            "COS" | "DCOS" => "cos".to_string(),
            "TAN" | "DTAN" => "tan".to_string(),
            "ASIN" | "DASIN" => "asin".to_string(),
            "ACOS" | "DACOS" => "acos".to_string(),
            "ATAN" | "DATAN" => "atan".to_string(),
            "ATAN2" | "DATAN2" => "atan2".to_string(),
            "SINH" | "DSINH" => "sinh".to_string(),
            "COSH" | "DCOSH" => "cosh".to_string(),
            "TANH" | "DTANH" => "tanh".to_string(),

            // Type conversion
            "INT" | "IFIX" => "as_i32".to_string(),
            "REAL" | "FLOAT" => "as_f32".to_string(),
            "DBLE" => "as_f64".to_string(),
            "CMPLX" => "Complex::new".to_string(),

            // Array functions
            "MIN" | "MIN0" | "AMIN0" | "DMIN1" => "min".to_string(),
            "MAX" | "MAX0" | "AMAX0" | "DMAX1" => "max".to_string(),
            "SUM" => "sum".to_string(),
            "PRODUCT" => "product".to_string(),

            // Other
            "MOD" => "rem".to_string(),
            "SIGN" | "DSIGN" | "ISIGN" => "copysign".to_string(),
            "LEN" => "len".to_string(),
            "TRIM" => "trim".to_string(),

            // Default: lowercase
            _ => name.to_lowercase(),
        }
    }

    /// Convert FORTRAN expression to string (for target names)
    fn expr_to_string(expr: &FortranExpr) -> String {
        match expr {
            FortranExpr::Var(name) => name.to_lowercase(),
            FortranExpr::ArrayRef { name, .. } => name.to_lowercase(),
            _ => "temp".to_string(),
        }
    }

    /// Convert IR expression to string (for comments)
    fn ir_expr_to_string(expr: &IrExpr) -> String {
        match expr {
            IrExpr::Int(n) => n.to_string(),
            IrExpr::Float(f) => f.to_string(),
            IrExpr::Identifier(s) => s.clone(),
            _ => "...".to_string(),
        }
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let source = "PROGRAM TEST\nINTEGER X\nEND PROGRAM";
        let mut lexer = FortranToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(FortranToken::Program)));
        assert!(matches!(lexer.next(), Some(Ok(FortranToken::Ident(_)))));
    }

    #[test]
    fn test_lexer_operators() {
        let source = ".AND. .OR. .NOT. .EQ. .NE. .LT. .GT.";
        let mut lexer = FortranToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(FortranToken::And)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Or)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Not)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Eq)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Ne)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Lt)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::Gt)));
    }

    #[test]
    fn test_lexer_literals() {
        let source = "42 3.14 'hello' .TRUE. .FALSE.";
        let mut lexer = FortranToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(FortranToken::IntLit(42))));
        assert!(matches!(lexer.next(), Some(Ok(FortranToken::RealLit(_)))));
        assert_eq!(
            lexer.next(),
            Some(Ok(FortranToken::StringLit("hello".to_string())))
        );
        assert_eq!(lexer.next(), Some(Ok(FortranToken::True)));
        assert_eq!(lexer.next(), Some(Ok(FortranToken::False)));
    }

    #[test]
    fn test_fixed_format_preprocessing() {
        let parser = FortranParser::fortran77();
        let source = "C     This is a comment\n      X = 1\n     +    + 2";
        let processed = parser.preprocess_fixed(source);

        assert!(processed.contains("!"));
        assert!(processed.contains("X = 1"));
        assert!(processed.contains("+ 2"));
    }

    #[test]
    fn test_parse_simple_program() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: "PROGRAM TEST\nINTEGER :: X\nX = 42\nEND PROGRAM TEST".to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_subroutine() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM MAIN
    CALL MYSUB(10, 20)
END PROGRAM

SUBROUTINE MYSUB(A, B)
    INTEGER, INTENT(IN) :: A, B
    PRINT *, A + B
END SUBROUTINE
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM MAIN
    REAL :: Y
    Y = SQUARE(5.0)
    PRINT *, Y
END PROGRAM

REAL FUNCTION SQUARE(X)
    REAL, INTENT(IN) :: X
    SQUARE = X * X
END FUNCTION
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_if_statement() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    INTEGER :: X
    X = 10
    IF (X > 5) THEN
        PRINT *, 'Greater'
    ELSE IF (X == 5) THEN
        PRINT *, 'Equal'
    ELSE
        PRINT *, 'Less'
    END IF
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_do_loop() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    INTEGER :: I, SUM
    SUM = 0
    DO I = 1, 10
        SUM = SUM + I
    END DO
    PRINT *, SUM
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_expressions() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    REAL :: A, B, C
    A = 2.0 ** 3.0
    B = SQRT(A) + SIN(3.14159 / 2.0)
    C = (A + B) * (A - B)
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_array_operations() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    INTEGER :: ARR(10)
    INTEGER :: I
    DO I = 1, 10
        ARR(I) = I * I
    END DO
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_io() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    INTEGER :: X
    PRINT *, 'Enter a number:'
    READ *, X
    WRITE(*, *) 'You entered:', X
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_f77_fixed_format() {
        let parser = FortranParser::fortran77();
        let source = SourceFile {
            name: "test.f".to_string(),
            content: r#"C     FORTRAN 77 PROGRAM
      PROGRAM HELLO
      INTEGER I
      DO 10 I = 1, 5
         PRINT *, I
   10 CONTINUE
      END
"#
            .to_string(),
            language: SourceLanguage::Fortran77,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_ir_conversion() {
        let parser = FortranParser::fortran90();
        let source = SourceFile {
            name: "test.f90".to_string(),
            content: r#"
PROGRAM TEST
    INTEGER :: X, Y
    X = 10
    Y = X * 2 + 5
END PROGRAM
"#
            .to_string(),
            language: SourceLanguage::Fortran90,
        };

        let result = parser.parse(&source);
        assert!(result.is_ok());

        let ir = result.unwrap();
        assert!(!ir.nodes.is_empty());
    }

    #[test]
    fn test_intrinsic_conversion() {
        assert_eq!(FortranToIr::convert_intrinsic("SQRT"), "sqrt");
        assert_eq!(FortranToIr::convert_intrinsic("DSQRT"), "sqrt");
        assert_eq!(FortranToIr::convert_intrinsic("ABS"), "abs");
        assert_eq!(FortranToIr::convert_intrinsic("SIN"), "sin");
        assert_eq!(FortranToIr::convert_intrinsic("MOD"), "rem");
    }
}
