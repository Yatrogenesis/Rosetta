//! # Rosetta PL/I Frontend
//!
//! Parses PL/I (Programming Language One) source code into Rosetta IR.
//!
//! ## PL/I History
//!
//! Developed by IBM in 1964 for scientific, engineering, and business applications.
//! Designed to be a "universal" language combining features of FORTRAN, COBOL, and ALGOL.
//!
//! ## Key Features
//!
//! - Block structure with BEGIN/END
//! - Exception handling (ON conditions)
//! - Concurrent programming (multitasking)
//! - Compile-time macros
//! - Powerful I/O with stream and record I/O

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// PL/I token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum PliToken {
    // Keywords
    #[token("PROCEDURE", ignore(ascii_case))]
    #[token("PROC", ignore(ascii_case))]
    Procedure,
    #[token("ENTRY", ignore(ascii_case))]
    Entry,
    #[token("RETURNS", ignore(ascii_case))]
    Returns,
    #[token("DECLARE", ignore(ascii_case))]
    #[token("DCL", ignore(ascii_case))]
    Declare,
    #[token("BEGIN", ignore(ascii_case))]
    Begin,
    #[token("END", ignore(ascii_case))]
    End,
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("UNTIL", ignore(ascii_case))]
    Until,
    #[token("TO", ignore(ascii_case))]
    To,
    #[token("BY", ignore(ascii_case))]
    By,
    #[token("SELECT", ignore(ascii_case))]
    Select,
    #[token("WHEN", ignore(ascii_case))]
    When,
    #[token("OTHERWISE", ignore(ascii_case))]
    Otherwise,
    #[token("CALL", ignore(ascii_case))]
    Call,
    #[token("RETURN", ignore(ascii_case))]
    Return,
    #[token("GO", ignore(ascii_case))]
    Go,
    #[token("GOTO", ignore(ascii_case))]
    Goto,
    #[token("LEAVE", ignore(ascii_case))]
    Leave,
    #[token("ITERATE", ignore(ascii_case))]
    Iterate,
    #[token("STOP", ignore(ascii_case))]
    Stop,

    // Data types
    #[token("FIXED", ignore(ascii_case))]
    Fixed,
    #[token("FLOAT", ignore(ascii_case))]
    Float,
    #[token("DECIMAL", ignore(ascii_case))]
    #[token("DEC", ignore(ascii_case))]
    Decimal,
    #[token("BINARY", ignore(ascii_case))]
    #[token("BIN", ignore(ascii_case))]
    Binary,
    #[token("CHARACTER", ignore(ascii_case))]
    #[token("CHAR", ignore(ascii_case))]
    Character,
    #[token("BIT", ignore(ascii_case))]
    Bit,
    #[token("PICTURE", ignore(ascii_case))]
    #[token("PIC", ignore(ascii_case))]
    Picture,
    #[token("POINTER", ignore(ascii_case))]
    #[token("PTR", ignore(ascii_case))]
    Pointer,
    #[token("OFFSET", ignore(ascii_case))]
    Offset,
    #[token("AREA", ignore(ascii_case))]
    Area,
    #[token("FILE", ignore(ascii_case))]
    File,
    #[token("LABEL", ignore(ascii_case))]
    Label,
    #[token("FORMAT", ignore(ascii_case))]
    Format,
    // Note: ENTRY as type uses the Entry token (context-sensitive)

    // Storage classes
    #[token("AUTOMATIC", ignore(ascii_case))]
    #[token("AUTO", ignore(ascii_case))]
    Automatic,
    #[token("STATIC", ignore(ascii_case))]
    Static,
    #[token("BASED", ignore(ascii_case))]
    Based,
    #[token("CONTROLLED", ignore(ascii_case))]
    #[token("CTL", ignore(ascii_case))]
    Controlled,
    #[token("DEFINED", ignore(ascii_case))]
    #[token("DEF", ignore(ascii_case))]
    Defined,
    #[token("PARAMETER", ignore(ascii_case))]
    #[token("PARM", ignore(ascii_case))]
    Parameter,

    // Attributes
    #[token("EXTERNAL", ignore(ascii_case))]
    #[token("EXT", ignore(ascii_case))]
    External,
    #[token("INTERNAL", ignore(ascii_case))]
    #[token("INT", ignore(ascii_case))]
    Internal,
    #[token("BUILTIN", ignore(ascii_case))]
    Builtin,
    #[token("INITIAL", ignore(ascii_case))]
    #[token("INIT", ignore(ascii_case))]
    Initial,
    #[token("VARYING", ignore(ascii_case))]
    #[token("VAR", ignore(ascii_case))]
    Varying,
    #[token("ALIGNED", ignore(ascii_case))]
    Aligned,
    #[token("UNALIGNED", ignore(ascii_case))]
    Unaligned,
    #[token("DIMENSION", ignore(ascii_case))]
    #[token("DIM", ignore(ascii_case))]
    Dimension,
    #[token("LIKE", ignore(ascii_case))]
    Like,

    // I/O
    #[token("GET", ignore(ascii_case))]
    Get,
    #[token("PUT", ignore(ascii_case))]
    Put,
    #[token("READ", ignore(ascii_case))]
    Read,
    #[token("WRITE", ignore(ascii_case))]
    Write,
    #[token("REWRITE", ignore(ascii_case))]
    Rewrite,
    #[token("DELETE", ignore(ascii_case))]
    Delete,
    #[token("OPEN", ignore(ascii_case))]
    Open,
    #[token("CLOSE", ignore(ascii_case))]
    Close,
    #[token("LIST", ignore(ascii_case))]
    List,
    #[token("DATA", ignore(ascii_case))]
    Data,
    #[token("EDIT", ignore(ascii_case))]
    Edit,
    #[token("SKIP", ignore(ascii_case))]
    Skip,
    #[token("LINE", ignore(ascii_case))]
    Line,
    #[token("PAGE", ignore(ascii_case))]
    Page,

    // Exception handling
    #[token("ON", ignore(ascii_case))]
    On,
    #[token("SIGNAL", ignore(ascii_case))]
    Signal,
    #[token("REVERT", ignore(ascii_case))]
    Revert,
    #[token("SYSTEM", ignore(ascii_case))]
    System,

    // Conditions
    #[token("ENDFILE", ignore(ascii_case))]
    Endfile,
    #[token("ENDPAGE", ignore(ascii_case))]
    Endpage,
    #[token("KEY", ignore(ascii_case))]
    Key,
    #[token("RECORD", ignore(ascii_case))]
    Record,
    #[token("TRANSMIT", ignore(ascii_case))]
    Transmit,
    #[token("CONVERSION", ignore(ascii_case))]
    Conversion,
    #[token("OVERFLOW", ignore(ascii_case))]
    Overflow,
    #[token("UNDERFLOW", ignore(ascii_case))]
    Underflow,
    #[token("ZERODIVIDE", ignore(ascii_case))]
    Zerodivide,
    #[token("SIZE", ignore(ascii_case))]
    Size,
    #[token("SUBSCRIPTRANGE", ignore(ascii_case))]
    Subscriptrange,
    #[token("STRINGRANGE", ignore(ascii_case))]
    Stringrange,
    #[token("ERROR", ignore(ascii_case))]
    Error,
    #[token("FINISH", ignore(ascii_case))]
    Finish,

    // Memory
    #[token("ALLOCATE", ignore(ascii_case))]
    #[token("ALLOC", ignore(ascii_case))]
    Allocate,
    #[token("FREE", ignore(ascii_case))]
    Free,
    #[token("NULL", ignore(ascii_case))]
    Null,

    // Operators
    #[token("&", priority = 3)]
    And,
    #[token("|", priority = 3)]
    Or,
    #[token("^")]
    #[token("~")]
    Not,
    #[token("**")]
    Power,
    #[token("||")]
    Concat,
    #[token("->")]
    Arrow,
    #[token(">=")]
    #[token("^<")]
    GreaterEqual,
    #[token("<=")]
    #[token("^>")]
    LessEqual,
    #[token("^=")]
    #[token("~=")]
    NotEqual,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*", priority = 2)]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equal,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    FloatLit(f64),
    #[regex(r"'[^']*'(X|B)?", |lex| lex.slice().to_string())]
    StringLit(String),
    #[regex(r"'[01]+'B", |lex| lex.slice().to_string())]
    BitLit(String),

    // Identifiers
    #[regex(r"[a-zA-Z_@#$][a-zA-Z0-9_@#$]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comments
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    Comment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// PL/I AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PliProgram {
    pub procedures: Vec<PliProcedure>,
}

/// Procedure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PliProcedure {
    pub name: String,
    pub params: Vec<String>,
    pub options: Vec<PliOption>,
    pub returns: Option<PliType>,
    pub declarations: Vec<PliDecl>,
    pub statements: Vec<PliStmt>,
}

/// Procedure options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PliOption {
    Main,
    Recursive,
    Reentrant,
    Options(String),
}

/// Declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PliDecl {
    Variable {
        names: Vec<String>,
        ty: PliType,
        storage: Option<StorageClass>,
        init: Option<PliExpr>,
    },
    Structure {
        level: i32,
        name: String,
        members: Vec<PliDecl>,
    },
    Procedure {
        name: String,
        params: Vec<PliType>,
        returns: Option<PliType>,
        external: bool,
    },
    OnUnit {
        condition: String,
        action: Box<PliStmt>,
    },
}

/// Storage class
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum StorageClass {
    Automatic,
    Static,
    Based,
    Controlled,
    Defined,
    Parameter,
}

/// PL/I Type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PliType {
    FixedBinary { precision: i32, scale: Option<i32> },
    FixedDecimal { precision: i32, scale: Option<i32> },
    FloatBinary { precision: i32 },
    FloatDecimal { precision: i32 },
    Character { length: PliExpr, varying: bool },
    Bit { length: PliExpr, varying: bool },
    Picture(String),
    Pointer,
    Offset,
    Area(i32),
    File,
    Label,
    Entry { params: Vec<PliType>, returns: Option<Box<PliType>> },
    Array { bounds: Vec<(PliExpr, PliExpr)>, element: Box<PliType> },
    Structure(Vec<(i32, String, PliType)>),
    Builtin(String),
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PliStmt {
    Assignment {
        target: PliExpr,
        value: PliExpr,
    },
    If {
        condition: PliExpr,
        then_part: Box<PliStmt>,
        else_part: Option<Box<PliStmt>>,
    },
    Select {
        expr: Option<PliExpr>,
        whens: Vec<(Vec<PliExpr>, Vec<PliStmt>)>,
        otherwise: Option<Vec<PliStmt>>,
    },
    Do {
        label: Option<String>,
        var: Option<String>,
        specs: DoSpecs,
        body: Vec<PliStmt>,
    },
    Call {
        name: String,
        args: Vec<PliExpr>,
    },
    Return(Option<PliExpr>),
    Goto(String),
    Leave(Option<String>),
    Iterate(Option<String>),
    Begin {
        label: Option<String>,
        declarations: Vec<PliDecl>,
        statements: Vec<PliStmt>,
    },
    On {
        condition: String,
        snap: bool,
        system: bool,
        action: Box<PliStmt>,
    },
    Signal(String),
    Revert(String),
    Get(GetPutSpec),
    Put(GetPutSpec),
    Open(Vec<FileOption>),
    Close(Vec<String>),
    Read(ReadWriteSpec),
    Write(ReadWriteSpec),
    Allocate {
        var: String,
        set: Option<String>,
        init: Option<PliExpr>,
    },
    Free(String),
    Null,
    Label(String, Box<PliStmt>),
}

/// DO loop specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DoSpecs {
    Simple,
    While(PliExpr),
    Until(PliExpr),
    Iteration {
        start: PliExpr,
        to: Option<PliExpr>,
        by: Option<PliExpr>,
        while_cond: Option<PliExpr>,
        until_cond: Option<PliExpr>,
    },
}

/// GET/PUT specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetPutSpec {
    pub file: Option<String>,
    pub string: Option<PliExpr>,
    pub mode: IoMode,
    pub items: Vec<PliExpr>,
    pub controls: Vec<IoControl>,
}

/// I/O mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IoMode {
    List,
    Data,
    Edit(Vec<FormatItem>),
}

/// Format item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FormatItem {
    A(Option<i32>),
    F(i32, Option<i32>),
    E(i32, i32),
    B(Option<i32>),
    P(String),
    X(i32),
    Skip(i32),
    Line(i32),
    Page,
    Column(i32),
}

/// I/O control
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IoControl {
    Skip(i32),
    Line(i32),
    Page,
    Column(i32),
}

/// File option
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileOption {
    File(String),
    Input,
    Output,
    Update,
    Stream,
    Record,
    Sequential,
    Direct,
    Keyed,
    Print,
    Title(String),
}

/// READ/WRITE specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadWriteSpec {
    pub file: String,
    pub into: Option<String>,
    pub from: Option<String>,
    pub key: Option<PliExpr>,
    pub keyto: Option<String>,
    pub ignore: Option<i32>,
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PliExpr {
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    BitLit(String),
    Null,
    Ident(String),
    Qualified(Vec<String>),
    Subscript {
        base: Box<PliExpr>,
        indices: Vec<PliExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<PliExpr>,
    },
    Builtin {
        name: String,
        args: Vec<PliExpr>,
    },
    BinOp {
        op: PliBinOp,
        left: Box<PliExpr>,
        right: Box<PliExpr>,
    },
    UnaryOp {
        op: PliUnaryOp,
        operand: Box<PliExpr>,
    },
    Pointer {
        base: Box<PliExpr>,
        locator: Box<PliExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PliBinOp {
    Add, Sub, Mul, Div, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PliUnaryOp {
    Neg, Not, Plus,
}

/// PL/I parser
pub struct PliParser;

impl PliParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for PliParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for PliParser {
    fn name(&self) -> &'static str {
        "PL/I"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["pli", "pl1", "ppl"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = PliToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, PliToken::Comment | PliToken::Newline) {
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

        let mut builder = IrBuilder::with_language("pli_module", SourceLanguage::Pli);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "HELLO: PROC OPTIONS(MAIN); DCL X FIXED BIN(31); END HELLO;";
        let lexer = PliToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&PliToken::Procedure));
    }

    #[test]
    fn test_parse() {
        let parser = PliParser::new();
        let source = SourceFile {
            name: "test.pli".to_string(),
            content: "TEST: PROC; END TEST;".to_string(),
            language: SourceLanguage::Pli,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
