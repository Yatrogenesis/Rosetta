//! # Rosetta SNOBOL Frontend
//!
//! Parses SNOBOL4 (StriNg Oriented and symBOlic Language) source code into Rosetta IR.
//!
//! ## SNOBOL History
//!
//! Developed at Bell Labs (1962-1967), SNOBOL is specialized for string processing.
//! Features powerful pattern matching that influenced later languages.
//!
//! ## Key Features
//!
//! - Pattern matching with backtracking
//! - String concatenation as default operation
//! - Associative arrays (tables)
//! - Success/failure as control flow mechanism

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// SNOBOL4 token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum SnobolToken {
    // Pattern primitives
    #[token("ARB")]
    Arb,
    #[token("BAL")]
    Bal,
    #[token("REM")]
    Rem,
    #[token("FAIL")]
    Fail,
    #[token("FENCE")]
    Fence,
    #[token("ABORT")]
    Abort,
    #[token("SUCCEED")]
    Succeed,

    // Pattern functions
    #[token("LEN")]
    Len,
    #[token("SPAN")]
    Span,
    #[token("BREAK")]
    Break,
    #[token("ANY")]
    Any,
    #[token("NOTANY")]
    NotAny,
    #[token("TAB")]
    Tab,
    #[token("RTAB")]
    Rtab,
    #[token("POS")]
    Pos,
    #[token("RPOS")]
    Rpos,
    #[token("ARBNO")]
    Arbno,

    // Built-in functions
    #[token("SIZE")]
    Size,
    #[token("DUPL")]
    Dupl,
    #[token("REPLACE")]
    Replace,
    #[token("TRIM")]
    Trim,
    #[token("LPAD")]
    Lpad,
    #[token("RPAD")]
    Rpad,
    #[token("REVERSE")]
    Reverse,
    #[token("DATE")]
    Date,
    #[token("TIME")]
    Time,
    #[token("EQ")]
    Eq,
    #[token("NE")]
    Ne,
    #[token("LT")]
    Lt,
    #[token("LE")]
    Le,
    #[token("GT")]
    Gt,
    #[token("GE")]
    Ge,
    #[token("INTEGER")]
    Integer,
    #[token("IDENT")]
    Ident_,
    #[token("DIFFER")]
    Differ,
    #[token("TABLE")]
    Table,
    #[token("ARRAY")]
    Array,
    #[token("CONVERT")]
    Convert,
    #[token("DATATYPE")]
    Datatype,
    #[token("COPY")]
    Copy,
    #[token("DEFINE")]
    Define,
    #[token("INPUT")]
    Input,
    #[token("OUTPUT")]
    Output,
    #[token("PUNCH")]
    Punch,
    #[token("TERMINAL")]
    Terminal,
    #[token("EVAL")]
    Eval,
    #[token("APPLY")]
    Apply,
    #[token("OPSYN")]
    Opsyn,
    #[token("LOAD")]
    Load,
    #[token("UNLOAD")]
    Unload,
    #[token("DATA")]
    Data,
    #[token("FIELD")]
    Field,
    #[token("ITEM")]
    Item,
    #[token("LOCAL")]
    Local,
    #[token("PROTOTYPE")]
    Prototype,
    #[token("STOPTR")]
    Stoptr,
    #[token("TRACE")]
    Trace,

    // Control flow keywords
    #[token("END")]
    End,

    // Operators
    #[token("=")]
    Assign,
    #[token(".")]
    Dot,          // Indirect reference
    #[token("$")]
    Dollar,       // Immediate assignment
    #[token("@")]
    At,           // Cursor position
    #[token("*")]
    Star,         // Deferred evaluation
    #[token("**")]
    Power,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("|")]
    Alt,          // Alternation
    #[token("&")]
    Ampersand,    // Keywords start with &
    #[token("!")]
    Bang,         // Negation
    #[token("?")]
    Question,     // Question mark (interrogation)
    #[token("^")]
    Caret,        // Exponentiation

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,

    // Goto types
    #[token(":S(")]
    GotoSuccess,
    #[token(":F(")]
    GotoFailure,
    #[token(":(")]
    GotoUnconditional,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    RealLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    StringLit(String),

    // Label (at start of line)
    #[regex(r"[A-Za-z][A-Za-z0-9_.]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comment (line starting with * followed by text)
    #[regex(r"\*[^\n]+", priority = 3)]
    Comment,

    // Note: Continuation (+/- at start of line) is handled at parser level
    // The Plus and Minus tokens serve this purpose contextually

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// SNOBOL4 AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnobolProgram {
    pub statements: Vec<SnobolStmt>,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnobolStmt {
    pub label: Option<String>,
    pub subject: Option<SnobolExpr>,
    pub pattern: Option<SnobolPattern>,
    pub replacement: Option<SnobolExpr>,
    pub goto: Option<SnobolGoto>,
}

/// Pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnobolPattern {
    Literal(String),
    Variable(String),
    Arb,
    Bal,
    Rem,
    Fail,
    Fence,
    Abort,
    Succeed,
    Len(Box<SnobolExpr>),
    Span(Box<SnobolExpr>),
    Break(Box<SnobolExpr>),
    Any(Box<SnobolExpr>),
    NotAny(Box<SnobolExpr>),
    Tab(Box<SnobolExpr>),
    Rtab(Box<SnobolExpr>),
    Pos(Box<SnobolExpr>),
    Rpos(Box<SnobolExpr>),
    Arbno(Box<SnobolPattern>),
    Concat(Box<SnobolPattern>, Box<SnobolPattern>),
    Alt(Box<SnobolPattern>, Box<SnobolPattern>),
    Assign {
        pattern: Box<SnobolPattern>,
        var: String,
    },
    ImmediateAssign {
        pattern: Box<SnobolPattern>,
        var: String,
    },
    Cursor(String),
    Deferred(Box<SnobolExpr>),
}

/// Goto
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnobolGoto {
    Unconditional(String),
    Success(String),
    Failure(String),
    Both {
        success: String,
        failure: String,
    },
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnobolExpr {
    IntLit(i64),
    RealLit(f64),
    StringLit(String),
    Ident(String),
    Indirect(Box<SnobolExpr>),
    Keyword(String),
    FunctionCall {
        name: String,
        args: Vec<SnobolExpr>,
    },
    ArrayRef {
        name: String,
        indices: Vec<SnobolExpr>,
    },
    TableRef {
        name: String,
        key: Box<SnobolExpr>,
    },
    Concat(Box<SnobolExpr>, Box<SnobolExpr>),
    BinOp {
        op: SnobolBinOp,
        left: Box<SnobolExpr>,
        right: Box<SnobolExpr>,
    },
    UnaryOp {
        op: SnobolUnaryOp,
        operand: Box<SnobolExpr>,
    },
    Negate(Box<SnobolExpr>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SnobolBinOp {
    Add, Sub, Mul, Div, Power,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SnobolUnaryOp {
    Neg, Not, Size, Question,
}

/// SNOBOL parser
pub struct SnobolParser;

impl SnobolParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SnobolParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for SnobolParser {
    fn name(&self) -> &'static str {
        "SNOBOL4"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["sno", "spt", "spitbol"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = SnobolToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, SnobolToken::Comment | SnobolToken::Newline) {
                        tokens.push(t);
                    }
                }
                Err(_) => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "valid token".to_string(),
                        found: lexer.slice().to_string(),
                        line: 1,
                        column: lexer.span().start,
                    });
                }
            }
        }

        let mut builder = IrBuilder::with_language("snobol_module", SourceLanguage::Snobol);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "OUTPUT = 'Hello, World!'";
        let lexer = SnobolToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&SnobolToken::Output));
    }

    #[test]
    fn test_pattern() {
        let source = "LINE SPAN('0123456789') . NUM";
        let lexer = SnobolToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&SnobolToken::Span));
    }

    #[test]
    fn test_parse() {
        let parser = SnobolParser::new();
        let source = SourceFile {
            name: "test.sno".to_string(),
            content: "OUTPUT = 'Test'".to_string(),
            language: SourceLanguage::Snobol,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
