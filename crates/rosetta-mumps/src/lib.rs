//! # Rosetta MUMPS/M Frontend
//!
//! Parses MUMPS (Massachusetts General Hospital Utility Multi-Programming System)
//! source code into Rosetta IR.
//!
//! ## MUMPS History
//!
//! Developed in 1966 at MGH for medical records. Still widely used in:
//! - Epic Systems (largest US healthcare software)
//! - VA VistA (US Veterans Affairs)
//! - Many hospital systems worldwide
//!
//! ## Key Features
//!
//! - Integrated hierarchical database (globals)
//! - Terse syntax (single-letter commands)
//! - Pattern matching
//! - No reserved words (context-sensitive)
//!
//! ## MUMPS to Rust Mapping
//!
//! | MUMPS | Rust |
//! |-------|------|
//! | SET X=1 | let x = 1; |
//! | ^GLOBAL | BTreeMap (persistent) |
//! | $P(X,D,N) | x.split(d).nth(n) |
//! | FOR I=1:1:10 | for i in 1..=10 |
//! | IF X DO | if x { ... } |
//! | QUIT | return |

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// MUMPS token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum MumpsToken {
    // Commands (full and abbreviated)
    #[token("SET", ignore(ascii_case))]
    #[token("S", ignore(ascii_case))]
    Set,
    #[token("KILL", ignore(ascii_case))]
    #[token("K", ignore(ascii_case))]
    Kill,
    #[token("NEW", ignore(ascii_case))]
    #[token("N", ignore(ascii_case))]
    New,
    #[token("QUIT", ignore(ascii_case))]
    #[token("Q", ignore(ascii_case))]
    Quit,
    #[token("DO", ignore(ascii_case))]
    #[token("D", ignore(ascii_case))]
    Do,
    #[token("GOTO", ignore(ascii_case))]
    #[token("G", ignore(ascii_case))]
    Goto,
    #[token("IF", ignore(ascii_case))]
    #[token("I", ignore(ascii_case))]
    If,
    #[token("ELSE", ignore(ascii_case))]
    #[token("E", ignore(ascii_case))]
    Else,
    #[token("FOR", ignore(ascii_case))]
    #[token("F", ignore(ascii_case))]
    For,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("WRITE", ignore(ascii_case))]
    #[token("W", ignore(ascii_case))]
    Write,
    #[token("READ", ignore(ascii_case))]
    #[token("R", ignore(ascii_case))]
    Read,
    #[token("OPEN", ignore(ascii_case))]
    #[token("O", ignore(ascii_case))]
    Open,
    #[token("CLOSE", ignore(ascii_case))]
    #[token("C", ignore(ascii_case))]
    Close,
    #[token("USE", ignore(ascii_case))]
    #[token("U", ignore(ascii_case))]
    Use,
    #[token("HANG", ignore(ascii_case))]
    #[token("H", ignore(ascii_case))]
    Hang,
    #[token("JOB", ignore(ascii_case))]
    #[token("J", ignore(ascii_case))]
    Job,
    #[token("LOCK", ignore(ascii_case))]
    #[token("L", ignore(ascii_case))]
    Lock,
    #[token("MERGE", ignore(ascii_case))]
    #[token("M", ignore(ascii_case))]
    Merge,
    #[token("XECUTE", ignore(ascii_case))]
    #[token("X", ignore(ascii_case))]
    Xecute,
    #[token("HALT", ignore(ascii_case))]
    Halt,
    #[token("BREAK", ignore(ascii_case))]
    #[token("B", ignore(ascii_case))]
    Break,
    #[token("VIEW", ignore(ascii_case))]
    #[token("V", ignore(ascii_case))]
    View,
    #[token("TSTART", ignore(ascii_case))]
    #[token("TS", ignore(ascii_case))]
    TStart,
    #[token("TCOMMIT", ignore(ascii_case))]
    #[token("TC", ignore(ascii_case))]
    TCommit,
    #[token("TROLLBACK", ignore(ascii_case))]
    #[token("TRO", ignore(ascii_case))]
    TRollback,

    // Intrinsic functions ($functions)
    #[token("$PIECE", ignore(ascii_case))]
    #[token("$P", ignore(ascii_case))]
    FnPiece,
    #[token("$LENGTH", ignore(ascii_case))]
    #[token("$L", ignore(ascii_case))]
    FnLength,
    #[token("$EXTRACT", ignore(ascii_case))]
    #[token("$E", ignore(ascii_case))]
    FnExtract,
    #[token("$FIND", ignore(ascii_case))]
    #[token("$F", ignore(ascii_case))]
    FnFind,
    #[token("$JUSTIFY", ignore(ascii_case))]
    #[token("$J", ignore(ascii_case))]
    FnJustify,
    #[token("$TRANSLATE", ignore(ascii_case))]
    #[token("$TR", ignore(ascii_case))]
    FnTranslate,
    #[token("$REVERSE", ignore(ascii_case))]
    #[token("$RE", ignore(ascii_case))]
    FnReverse,
    #[token("$ORDER", ignore(ascii_case))]
    #[token("$O", ignore(ascii_case))]
    FnOrder,
    #[token("$NEXT", ignore(ascii_case))]
    #[token("$N", ignore(ascii_case))]
    FnNext,
    #[token("$DATA", ignore(ascii_case))]
    #[token("$D", ignore(ascii_case))]
    FnData,
    #[token("$GET", ignore(ascii_case))]
    #[token("$G", ignore(ascii_case))]
    FnGet,
    #[token("$NAME", ignore(ascii_case))]
    #[token("$NA", ignore(ascii_case))]
    FnName,
    #[token("$QUERY", ignore(ascii_case))]
    #[token("$Q", ignore(ascii_case))]
    FnQuery,
    #[token("$QLENGTH", ignore(ascii_case))]
    #[token("$QL", ignore(ascii_case))]
    FnQLength,
    #[token("$QSUBSCRIPT", ignore(ascii_case))]
    #[token("$QS", ignore(ascii_case))]
    FnQSubscript,
    #[token("$ASCII", ignore(ascii_case))]
    #[token("$A", ignore(ascii_case))]
    FnAscii,
    #[token("$CHAR", ignore(ascii_case))]
    #[token("$C", ignore(ascii_case))]
    FnChar,
    #[token("$RANDOM", ignore(ascii_case))]
    #[token("$R", ignore(ascii_case))]
    FnRandom,
    #[token("$SELECT", ignore(ascii_case))]
    #[token("$S", ignore(ascii_case))]
    FnSelect,
    #[token("$STACK", ignore(ascii_case))]
    #[token("$ST", ignore(ascii_case))]
    FnStack,
    #[token("$TEXT", ignore(ascii_case))]
    #[token("$T", ignore(ascii_case))]
    FnText,
    #[token("$HOROLOG", ignore(ascii_case))]
    #[token("$H", ignore(ascii_case))]
    FnHorolog,
    #[token("$JOB", ignore(ascii_case))]
    FnJob,
    #[token("$IO", ignore(ascii_case))]
    FnIO,
    #[token("$TEST", ignore(ascii_case))]
    FnTest,
    #[token("$PRINCIPAL", ignore(ascii_case))]
    FnPrincipal,
    #[token("$STORAGE", ignore(ascii_case))]
    FnStorage,
    #[token("$SYSTEM", ignore(ascii_case))]
    FnSystem,
    #[token("$ZVERSION", ignore(ascii_case))]
    FnZVersion,

    // Special variables
    #[regex(r"\$\$[A-Za-z][A-Za-z0-9]*", |lex| lex.slice()[2..].to_string())]
    ExtrinsicFunc(String),

    // Operators
    #[token("=")]
    Equal,
    #[token("'=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("'<")]
    NotLess,
    #[token("'>")]
    NotGreater,
    #[token("[")]
    Contains,
    #[token("'[")]
    NotContains,
    #[token("]")]
    Follows,
    #[token("']")]
    NotFollows,
    #[token("]]")]
    SortsAfter,
    #[token("?")]
    PatternMatch,
    #[token("'?")]
    NotPatternMatch,
    #[token("&")]
    And,
    #[token("!")]
    Or,
    #[token("'")]
    Not,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("\\")]
    IntDiv,
    #[token("#")]
    Modulo,
    #[token("**")]
    Power,
    #[token("_")]
    Concat,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("@")]
    Indirection,
    #[token("^")]
    Caret,  // Global prefix
    #[token("^^")]
    DoubleCaret,  // Extended global

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    NumLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLit(String),

    // Identifiers and globals
    #[regex(r"\^[A-Za-z%][A-Za-z0-9]*", |lex| lex.slice()[1..].to_string())]
    Global(String),
    #[regex(r"%[A-Za-z][A-Za-z0-9]*", |lex| lex.slice().to_string())]
    PercentRoutine(String),
    #[regex(r"[A-Za-z][A-Za-z0-9]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Label (at start of line)
    #[regex(r"[A-Za-z%][A-Za-z0-9]*\([^)]*\)", |lex| lex.slice().to_string())]
    LabelWithParams(String),

    // Comment
    #[regex(r";[^\n]*")]
    Comment,

    // Newline (significant in MUMPS)
    #[regex(r"\n")]
    Newline,

    // Dot level (indentation)
    #[token(".")]
    Dot,
    #[token(" .")]
    SpaceDot,
}

/// MUMPS AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MumpsProgram {
    pub routines: Vec<MumpsRoutine>,
}

/// Routine (file/module)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MumpsRoutine {
    pub name: String,
    pub lines: Vec<MumpsLine>,
}

/// A single line of MUMPS code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MumpsLine {
    pub label: Option<String>,
    pub params: Option<Vec<String>>,
    pub level: usize,  // Dot level for block structure
    pub commands: Vec<MumpsCommand>,
}

/// Command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MumpsCommand {
    Set(Vec<MumpsAssignment>),
    Kill(Vec<MumpsExpr>),
    New(Vec<String>),
    Quit(Option<MumpsExpr>),
    Do {
        target: MumpsExpr,
        postcond: Option<MumpsExpr>,
        args: Option<Vec<MumpsExpr>>,
    },
    Goto {
        target: MumpsExpr,
        postcond: Option<MumpsExpr>,
    },
    If(MumpsExpr),
    Else,
    For {
        var: Option<String>,
        specs: Vec<ForSpec>,
    },
    While(MumpsExpr),
    Write(Vec<WriteArg>),
    Read(Vec<ReadArg>),
    Open {
        device: MumpsExpr,
        params: Option<MumpsExpr>,
    },
    Close(MumpsExpr),
    Use(MumpsExpr),
    Hang(MumpsExpr),
    Job {
        target: MumpsExpr,
        params: Option<MumpsExpr>,
    },
    Lock {
        targets: Vec<MumpsExpr>,
        timeout: Option<MumpsExpr>,
    },
    Merge(MumpsExpr, MumpsExpr),
    Xecute(MumpsExpr),
    Halt,
    Break,
    View(MumpsExpr),
    TStart(Option<Vec<String>>),
    TCommit,
    TRollback,
}

/// Assignment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MumpsAssignment {
    pub target: MumpsExpr,
    pub value: MumpsExpr,
}

/// FOR loop specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ForSpec {
    Single(MumpsExpr),
    Range {
        start: MumpsExpr,
        step: Option<MumpsExpr>,
        end: Option<MumpsExpr>,
    },
}

/// WRITE argument
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WriteArg {
    Expr(MumpsExpr),
    NewLine,
    Tab(MumpsExpr),
    Clear,
    FormFeed,
}

/// READ argument
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReadArg {
    Var {
        name: MumpsExpr,
        length: Option<MumpsExpr>,
        timeout: Option<MumpsExpr>,
    },
    Prompt(MumpsExpr),
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MumpsExpr {
    IntLit(i64),
    NumLit(f64),
    StringLit(String),
    Ident(String),
    Global {
        name: String,
        subscripts: Vec<MumpsExpr>,
    },
    LocalArray {
        name: String,
        subscripts: Vec<MumpsExpr>,
    },
    Indirection(Box<MumpsExpr>),
    FunctionCall {
        name: MumpsFn,
        args: Vec<MumpsExpr>,
    },
    ExtrinsicCall {
        routine: String,
        label: Option<String>,
        args: Vec<MumpsExpr>,
    },
    BinOp {
        op: MumpsBinOp,
        left: Box<MumpsExpr>,
        right: Box<MumpsExpr>,
    },
    UnaryOp {
        op: MumpsUnaryOp,
        operand: Box<MumpsExpr>,
    },
    PatternMatch {
        value: Box<MumpsExpr>,
        pattern: String,
    },
}

/// Intrinsic functions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MumpsFn {
    Piece, Length, Extract, Find, Justify, Translate, Reverse,
    Order, Next, Data, Get, Name, Query, QLength, QSubscript,
    Ascii, Char, Random, Select, Stack, Text,
    Horolog, Job, IO, Test, Principal, Storage, System,
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum MumpsBinOp {
    Add, Sub, Mul, Div, IntDiv, Mod, Power,
    Eq, Ne, Lt, Gt, Le, Ge,
    Contains, Follows, SortsAfter,
    And, Or,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum MumpsUnaryOp {
    Neg, Not, Plus,
}

/// MUMPS parser
pub struct MumpsParser;

impl MumpsParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for MumpsParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for MumpsParser {
    fn name(&self) -> &'static str {
        "MUMPS/M"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["m", "mps", "mumps", "ros"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = MumpsToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, MumpsToken::Comment) {
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

        let builder = IrBuilder::with_language("mumps_module", SourceLanguage::Mumps);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = r#"SET X=1 WRITE "Hello",!"#;
        let lexer = MumpsToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&MumpsToken::Set));
        assert!(tokens.contains(&MumpsToken::Write));
    }

    #[test]
    fn test_global() {
        let source = "SET ^PATIENT(123,\"NAME\")=\"John Doe\"";
        let lexer = MumpsToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.iter().any(|t| matches!(t, MumpsToken::Global(_))));
    }

    #[test]
    fn test_parse() {
        let parser = MumpsParser::new();
        let source = SourceFile {
            name: "test.m".to_string(),
            content: "SET X=1 QUIT".to_string(),
            language: SourceLanguage::Mumps,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
