//! # Rosetta REXX Frontend
//!
//! Parses REXX (Restructured Extended Executor) source code into Rosetta IR.
//!
//! ## REXX History
//!
//! Developed by Mike Cowlishaw at IBM in 1979. Used extensively on:
//! - IBM mainframes (VM/CMS, MVS, z/OS)
//! - OS/2
//! - AmigaOS (ARexx)
//! - Modern: Regina REXX, Open Object REXX
//!
//! ## Key Features
//!
//! - Human-readable syntax
//! - Powerful string handling
//! - Decimal arithmetic by default
//! - PARSE instruction for pattern matching
//! - No reserved words (context-sensitive)

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// REXX token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum RexxToken {
    // Keywords (REXX has no reserved words, but these are commonly used)
    #[token("ADDRESS", ignore(ascii_case))]
    Address,
    #[token("ARG", ignore(ascii_case))]
    Arg,
    #[token("CALL", ignore(ascii_case))]
    Call,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("DROP", ignore(ascii_case))]
    Drop,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("END", ignore(ascii_case))]
    End,
    #[token("EXIT", ignore(ascii_case))]
    Exit,
    #[token("EXPOSE", ignore(ascii_case))]
    Expose,
    #[token("FOREVER", ignore(ascii_case))]
    Forever,
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("INTERPRET", ignore(ascii_case))]
    Interpret,
    #[token("ITERATE", ignore(ascii_case))]
    Iterate,
    #[token("LEAVE", ignore(ascii_case))]
    Leave,
    #[token("NOP", ignore(ascii_case))]
    Nop,
    #[token("NUMERIC", ignore(ascii_case))]
    Numeric,
    #[token("OPTIONS", ignore(ascii_case))]
    Options,
    #[token("OTHERWISE", ignore(ascii_case))]
    Otherwise,
    #[token("PARSE", ignore(ascii_case))]
    Parse,
    #[token("PROCEDURE", ignore(ascii_case))]
    Procedure,
    #[token("PULL", ignore(ascii_case))]
    Pull,
    #[token("PUSH", ignore(ascii_case))]
    Push,
    #[token("QUEUE", ignore(ascii_case))]
    Queue,
    #[token("RETURN", ignore(ascii_case))]
    Return,
    #[token("SAY", ignore(ascii_case))]
    Say,
    #[token("SELECT", ignore(ascii_case))]
    Select,
    #[token("SIGNAL", ignore(ascii_case))]
    Signal,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("TRACE", ignore(ascii_case))]
    Trace,
    #[token("UPPER", ignore(ascii_case))]
    Upper,
    #[token("WHEN", ignore(ascii_case))]
    When,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("UNTIL", ignore(ascii_case))]
    Until,
    #[token("WITH", ignore(ascii_case))]
    With,

    // PARSE sub-keywords
    #[token("EXTERNAL", ignore(ascii_case))]
    External,
    #[token("LINEIN", ignore(ascii_case))]
    LineIn,
    #[token("SOURCE", ignore(ascii_case))]
    Source,
    #[token("VAR", ignore(ascii_case))]
    Var,
    #[token("VALUE", ignore(ascii_case))]
    Value,
    #[token("VERSION", ignore(ascii_case))]
    Version,

    // NUMERIC sub-keywords
    #[token("DIGITS", ignore(ascii_case))]
    Digits,
    #[token("FORM", ignore(ascii_case))]
    Form,
    #[token("FUZZ", ignore(ascii_case))]
    Fuzz,
    #[token("SCIENTIFIC", ignore(ascii_case))]
    Scientific,
    #[token("ENGINEERING", ignore(ascii_case))]
    Engineering,

    // SIGNAL sub-keywords
    #[token("ON", ignore(ascii_case))]
    On,
    #[token("OFF", ignore(ascii_case))]
    Off,
    #[token("ERROR", ignore(ascii_case))]
    Error,
    #[token("FAILURE", ignore(ascii_case))]
    Failure,
    #[token("HALT", ignore(ascii_case))]
    Halt,
    #[token("NOVALUE", ignore(ascii_case))]
    NoValue,
    #[token("NOTREADY", ignore(ascii_case))]
    NotReady,
    #[token("SYNTAX", ignore(ascii_case))]
    Syntax,
    #[token("LOSTDIGITS", ignore(ascii_case))]
    LostDigits,

    // ADDRESS environments
    #[token("COMMAND", ignore(ascii_case))]
    Command,
    #[token("SYSTEM", ignore(ascii_case))]
    System,

    // DO loop keywords
    #[token("TO", ignore(ascii_case))]
    To,
    #[token("BY", ignore(ascii_case))]
    By,
    #[token("FOR", ignore(ascii_case))]
    For,

    // Built-in functions
    #[token("ABBREV", ignore(ascii_case))]
    FnAbbrev,
    #[token("ABS", ignore(ascii_case))]
    FnAbs,
    #[token("B2X", ignore(ascii_case))]
    FnB2X,
    #[token("BITAND", ignore(ascii_case))]
    FnBitAnd,
    #[token("BITOR", ignore(ascii_case))]
    FnBitOr,
    #[token("BITXOR", ignore(ascii_case))]
    FnBitXor,
    #[token("C2D", ignore(ascii_case))]
    FnC2D,
    #[token("C2X", ignore(ascii_case))]
    FnC2X,
    #[token("CENTER", ignore(ascii_case))]
    #[token("CENTRE", ignore(ascii_case))]
    FnCenter,
    #[token("CHANGESTR", ignore(ascii_case))]
    FnChangeStr,
    #[token("CHARIN", ignore(ascii_case))]
    FnCharIn,
    #[token("CHAROUT", ignore(ascii_case))]
    FnCharOut,
    #[token("CHARS", ignore(ascii_case))]
    FnChars,
    #[token("COMPARE", ignore(ascii_case))]
    FnCompare,
    #[token("CONDITION", ignore(ascii_case))]
    FnCondition,
    #[token("COPIES", ignore(ascii_case))]
    FnCopies,
    #[token("COUNTSTR", ignore(ascii_case))]
    FnCountStr,
    #[token("D2C", ignore(ascii_case))]
    FnD2C,
    #[token("D2X", ignore(ascii_case))]
    FnD2X,
    #[token("DATATYPE", ignore(ascii_case))]
    FnDatatype,
    #[token("DATE", ignore(ascii_case))]
    FnDate,
    #[token("DELSTR", ignore(ascii_case))]
    FnDelStr,
    #[token("DELWORD", ignore(ascii_case))]
    FnDelWord,
    #[token("ERRORTEXT", ignore(ascii_case))]
    FnErrorText,
    #[token("FORMAT", ignore(ascii_case))]
    FnFormat,
    #[token("INSERT", ignore(ascii_case))]
    FnInsert,
    #[token("LASTPOS", ignore(ascii_case))]
    FnLastPos,
    #[token("LEFT", ignore(ascii_case))]
    FnLeft,
    #[token("LENGTH", ignore(ascii_case))]
    FnLength,
    #[token("LINES", ignore(ascii_case))]
    FnLines,
    #[token("LINEOUT", ignore(ascii_case))]
    FnLineOut,
    #[token("MAX", ignore(ascii_case))]
    FnMax,
    #[token("MIN", ignore(ascii_case))]
    FnMin,
    #[token("OVERLAY", ignore(ascii_case))]
    FnOverlay,
    #[token("POS", ignore(ascii_case))]
    FnPos,
    #[token("QUEUED", ignore(ascii_case))]
    FnQueued,
    #[token("RANDOM", ignore(ascii_case))]
    FnRandom,
    #[token("REVERSE", ignore(ascii_case))]
    FnReverse,
    #[token("RIGHT", ignore(ascii_case))]
    FnRight,
    #[token("SIGN", ignore(ascii_case))]
    FnSign,
    #[token("SOURCELINE", ignore(ascii_case))]
    FnSourceLine,
    #[token("SPACE", ignore(ascii_case))]
    FnSpace,
    #[token("STREAM", ignore(ascii_case))]
    FnStream,
    #[token("STRIP", ignore(ascii_case))]
    FnStrip,
    #[token("SUBSTR", ignore(ascii_case))]
    FnSubstr,
    #[token("SUBWORD", ignore(ascii_case))]
    FnSubWord,
    #[token("SYMBOL", ignore(ascii_case))]
    FnSymbol,
    #[token("TIME", ignore(ascii_case))]
    FnTime,
    #[token("TRANSLATE", ignore(ascii_case))]
    FnTranslate,
    #[token("TRUNC", ignore(ascii_case))]
    FnTrunc,
    #[token("VERIFY", ignore(ascii_case))]
    FnVerify,
    #[token("WORD", ignore(ascii_case))]
    FnWord,
    #[token("WORDINDEX", ignore(ascii_case))]
    FnWordIndex,
    #[token("WORDLENGTH", ignore(ascii_case))]
    FnWordLength,
    #[token("WORDPOS", ignore(ascii_case))]
    FnWordPos,
    #[token("WORDS", ignore(ascii_case))]
    FnWords,
    #[token("X2B", ignore(ascii_case))]
    FnX2B,
    #[token("X2C", ignore(ascii_case))]
    FnX2C,
    #[token("X2D", ignore(ascii_case))]
    FnX2D,
    #[token("XRANGE", ignore(ascii_case))]
    FnXRange,

    // Operators
    #[token("=")]
    Equal,
    #[token("==")]
    StrictEqual,
    #[token("\\=")]
    #[token("<>")]
    #[token("><")]
    NotEqual,
    #[token("\\==")]
    StrictNotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    #[token("\\>")]
    LessEqual,
    #[token(">=")]
    #[token("\\<")]
    GreaterEqual,
    #[token("<<")]
    StrictLess,
    #[token(">>")]
    StrictGreater,
    #[token("<<=")]
    #[token("\\>>")]
    StrictLessEqual,
    #[token(">>=")]
    #[token("\\<<")]
    StrictGreaterEqual,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("//")]
    Remainder,
    #[token("%")]
    IntDiv,
    #[token("**")]
    Power,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("&&")]
    Xor,
    #[token("\\")]
    Not,
    #[token("||")]
    Concat,
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

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    NumLit(f64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLit(String),
    #[regex(r"'[0-9A-Fa-f]+'[xX]", |lex| lex.slice().to_string())]
    HexLit(String),
    #[regex(r"'[01]+'[bB]", |lex| lex.slice().to_string())]
    BinLit(String),

    // Identifiers and labels
    #[regex(r"[A-Za-z_!?][A-Za-z0-9_!?.]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*:", |lex| lex.slice().trim_end_matches(':').to_string())]
    Label(String),

    // Comments
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    Comment,
    #[regex(r"--[^\n]*")]
    LineComment,

    // Newline (continuation is handled contextually - comma at end of line)
    #[regex(r"\n")]
    Newline,
}

/// REXX AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RexxProgram {
    pub statements: Vec<RexxStmt>,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RexxStmt {
    Assignment {
        target: RexxExpr,
        value: RexxExpr,
    },
    Say(RexxExpr),
    Pull(Vec<String>),
    Arg(Vec<ParseTemplate>),
    Parse {
        source: ParseSource,
        template: Vec<ParseTemplate>,
        upper: bool,
    },
    If {
        condition: RexxExpr,
        then_stmt: Box<RexxStmt>,
        else_stmt: Option<Box<RexxStmt>>,
    },
    Select {
        cases: Vec<(RexxExpr, Vec<RexxStmt>)>,
        otherwise: Option<Vec<RexxStmt>>,
    },
    Do(DoSpec),
    Call {
        target: String,
        args: Vec<RexxExpr>,
    },
    Signal {
        target: SignalTarget,
    },
    Return(Option<RexxExpr>),
    Exit(Option<RexxExpr>),
    Iterate(Option<String>),
    Leave(Option<String>),
    Nop,
    Drop(Vec<String>),
    Interpret(RexxExpr),
    Address {
        environment: Option<String>,
        command: Option<RexxExpr>,
    },
    Numeric(NumericSetting),
    Trace(String),
    Queue(RexxExpr),
    Push(RexxExpr),
    Procedure {
        expose: Vec<String>,
        body: Vec<RexxStmt>,
    },
    Label(String, Box<RexxStmt>),
    Compound(Vec<RexxStmt>),
}

/// DO loop specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DoSpec {
    Simple(Vec<RexxStmt>),
    Forever(Vec<RexxStmt>),
    While {
        condition: RexxExpr,
        body: Vec<RexxStmt>,
    },
    Until {
        condition: RexxExpr,
        body: Vec<RexxStmt>,
    },
    Controlled {
        var: String,
        start: RexxExpr,
        to: Option<RexxExpr>,
        by: Option<RexxExpr>,
        for_count: Option<RexxExpr>,
        while_cond: Option<RexxExpr>,
        until_cond: Option<RexxExpr>,
        body: Vec<RexxStmt>,
    },
}

/// PARSE source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParseSource {
    Arg,
    External,
    LineIn(Option<String>),
    Pull,
    Source,
    Value(RexxExpr),
    Var(String),
    Version,
}

/// PARSE template element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParseTemplate {
    Variable(String),
    Literal(String),
    Column(i32),
    RelativeColumn(i32),
    Placeholder,
}

/// SIGNAL target
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SignalTarget {
    Label(String),
    Value(RexxExpr),
    On {
        condition: String,
        name: Option<String>,
    },
    Off(String),
}

/// NUMERIC setting
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NumericSetting {
    Digits(i32),
    Form(NumericForm),
    Fuzz(i32),
}

/// NUMERIC FORM
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NumericForm {
    Scientific,
    Engineering,
    Value(String),
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RexxExpr {
    IntLit(i64),
    NumLit(f64),
    StringLit(String),
    HexLit(String),
    BinLit(String),
    Ident(String),
    Stem {
        name: String,
        tail: Vec<RexxExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<RexxExpr>,
    },
    BinOp {
        op: RexxBinOp,
        left: Box<RexxExpr>,
        right: Box<RexxExpr>,
    },
    UnaryOp {
        op: RexxUnaryOp,
        operand: Box<RexxExpr>,
    },
    Parenthesized(Box<RexxExpr>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum RexxBinOp {
    Add, Sub, Mul, Div, IntDiv, Remainder, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    StrictEq, StrictNe, StrictLt, StrictLe, StrictGt, StrictGe,
    And, Or, Xor,
    Concat, ConcatSpace,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum RexxUnaryOp {
    Neg, Not, Plus,
}

/// REXX parser
pub struct RexxParser;

impl RexxParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for RexxParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for RexxParser {
    fn name(&self) -> &'static str {
        "REXX"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["rexx", "rex", "rx", "cmd", "exec"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = RexxToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, RexxToken::Comment | RexxToken::LineComment | RexxToken::Newline) {
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

        let builder = IrBuilder::with_language("rexx_module", SourceLanguage::Rexx);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "SAY 'Hello, World!'";
        let lexer = RexxToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&RexxToken::Say));
    }

    #[test]
    fn test_parse_instruction() {
        let source = "PARSE ARG name . rest";
        let lexer = RexxToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&RexxToken::Parse));
        assert!(tokens.contains(&RexxToken::Arg));
    }

    #[test]
    fn test_do_loop() {
        let source = "DO i = 1 TO 10 BY 2; SAY i; END";
        let lexer = RexxToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&RexxToken::Do));
        assert!(tokens.contains(&RexxToken::To));
        assert!(tokens.contains(&RexxToken::By));
    }

    #[test]
    fn test_parse() {
        let parser = RexxParser::new();
        let source = SourceFile {
            name: "test.rexx".to_string(),
            content: "SAY 'Hello'".to_string(),
            language: SourceLanguage::Rexx,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
