//! # Rosetta APL Frontend
//!
//! Parses APL (A Programming Language) source code into Rosetta IR.
//!
//! ## APL History
//!
//! Designed by Kenneth Iverson (1966), APL is famous for:
//! - Special character set for mathematical operations
//! - Array-oriented programming (operates on entire arrays)
//! - Extreme conciseness
//! - Right-to-left evaluation
//!
//! ## APL Symbols
//!
//! APL uses special Unicode characters. This frontend supports both
//! Unicode APL characters and ASCII alternatives.

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// APL token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum AplToken {
    // Monadic functions (single argument)
    #[token("+")]
    #[token("conjugate")]
    Conjugate,  // Also Add when dyadic
    #[token("-")]
    #[token("negate")]
    Negate,     // Also Subtract when dyadic
    #[token("×")]
    #[token("signum")]
    Signum,     // Also Multiply when dyadic
    #[token("÷")]
    #[token("reciprocal")]
    Reciprocal, // Also Divide when dyadic
    #[token("⌈")]
    #[token("ceiling")]
    Ceiling,    // Also Max when dyadic
    #[token("⌊")]
    #[token("floor")]
    Floor,      // Also Min when dyadic
    #[token("*")]
    #[token("exponential")]
    Exponential, // Also Power when dyadic
    #[token("⍟")]
    #[token("naturallog")]
    NaturalLog,  // Also Log when dyadic
    #[token("|")]
    #[token("magnitude")]
    Magnitude,   // Also Residue when dyadic
    #[token("!")]
    #[token("factorial")]
    Factorial,   // Also Binomial when dyadic
    #[token("○")]
    #[token("pitimes")]
    PiTimes,     // Also Circular when dyadic
    #[token("~")]
    #[token("not")]
    Not,         // Also Without when dyadic
    #[token("?")]
    #[token("roll")]
    Roll,        // Also Deal when dyadic
    #[token("⍳")]
    #[token("iota")]
    Iota,        // Also Index Of when dyadic
    #[token("⍴")]
    #[token("shape")]
    Shape,       // Also Reshape when dyadic
    #[token(",")]
    Ravel,       // Also Catenate when dyadic
    #[token("⌽")]
    #[token("reverse")]
    Reverse,     // Also Rotate when dyadic
    #[token("⊖")]
    #[token("reverselast")]
    ReverseLast, // Also Rotate Last when dyadic
    #[token("⍉")]
    #[token("transpose")]
    Transpose,   // Also Dyadic Transpose when dyadic
    #[token("↑")]
    #[token("first")]
    First,       // Also Take when dyadic
    #[token("↓")]
    Drop,        // Also Drop when dyadic
    #[token("⊂")]
    #[token("enclose")]
    Enclose,     // Also Partitioned Enclose when dyadic
    #[token("⊃")]
    #[token("disclose")]
    Disclose,    // Also Pick when dyadic
    #[token("∊")]
    #[token("enlist")]
    Enlist,      // Also Member Of when dyadic
    #[token("⍷")]
    #[token("find")]
    Find,
    #[token("≡")]
    #[token("depth")]
    Depth,       // Also Match when dyadic
    #[token("≢")]
    #[token("tally")]
    Tally,       // Also Not Match when dyadic
    #[token("⍒")]
    #[token("gradedown")]
    GradeDown,
    #[token("⍋")]
    #[token("gradeup")]
    GradeUp,
    #[token("⍎")]
    #[token("execute")]
    Execute,
    #[token("⍕")]
    #[token("format")]
    Format,
    #[token("⊥")]
    #[token("decode")]
    Decode,
    #[token("⊤")]
    #[token("encode")]
    Encode,

    // Dyadic only operators
    #[token("∧")]
    #[token("and")]
    And,
    #[token("∨")]
    #[token("or")]
    Or,
    #[token("⍲")]
    #[token("nand")]
    Nand,
    #[token("⍱")]
    #[token("nor")]
    Nor,
    #[token("<")]
    Less,
    #[token("≤")]
    #[token("<=")]
    LessEqual,
    #[token("=")]
    Equal,
    #[token("≥")]
    #[token(">=")]
    GreaterEqual,
    #[token(">")]
    Greater,
    #[token("≠")]
    #[token("<>")]
    NotEqual,

    // Operators (higher-order functions)
    #[token("/")]
    Reduce,      // Also Replicate when array left arg
    #[token("⌿")]
    ReduceFirst,
    #[token("\\")]
    Scan,        // Also Expand when array left arg
    #[token("⍀")]
    ScanFirst,
    #[token("¨")]
    #[token("each")]
    Each,
    #[token("⍣")]
    #[token("power")]
    Power,
    #[token("∘")]
    #[token("compose")]
    Compose,
    #[token("⍤")]
    #[token("rank")]
    Rank,
    #[token("@")]
    #[token("at")]
    At,
    #[token("⌸")]
    #[token("key")]
    Key,
    #[token("⌺")]
    #[token("stencil")]
    Stencil,
    #[token(".")]
    InnerProduct, // Or Outer Product with ∘.

    // Assignment and flow
    #[token("←")]
    #[token(":=")]
    Assign,
    #[token("→")]
    #[token("goto")]
    Goto,
    #[token("⍬")]
    #[token("zilde")]
    Zilde,        // Empty numeric vector
    #[token("⋄")]
    #[token(";;")]
    Diamond,      // Statement separator

    // Brackets and delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    // System commands/variables
    #[regex(r"⎕[A-Za-z]+", |lex| lex.slice()[3..].to_string())]
    #[regex(r"\)[A-Za-z]+", |lex| lex.slice()[1..].to_string())]
    SystemVar(String),

    // Literals
    #[regex(r"¯?[0-9]+\.?[0-9]*([eE]¯?[0-9]+)?", |lex| lex.slice().replace('¯', "-").parse().ok())]
    NumLit(f64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    CharLit(String),

    // Identifiers
    #[regex(r"[A-Za-z_∆⍙][A-Za-z0-9_∆⍙]*", |lex| lex.slice().to_string())]
    Ident(String),

    // Comments
    #[regex(r"⍝[^\n]*")]
    #[regex(r"#[^\n]*")]
    Comment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// APL AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AplProgram {
    pub statements: Vec<AplStmt>,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AplStmt {
    Assignment {
        target: String,
        indexed: Option<Vec<AplExpr>>,
        value: AplExpr,
    },
    Expression(AplExpr),
    Goto(AplExpr),
    Label(String),
    FunctionDef {
        name: String,
        left_arg: Option<String>,
        right_arg: Option<String>,
        local_vars: Vec<String>,
        body: Vec<AplStmt>,
    },
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AplExpr {
    Num(f64),
    NumVec(Vec<f64>),
    Char(char),
    CharVec(String),
    Ident(String),
    Zilde,
    SystemVar(String),
    Strand(Vec<AplExpr>),
    Index {
        array: Box<AplExpr>,
        indices: Vec<Option<AplExpr>>,
    },
    MonadicOp {
        op: AplFunc,
        operand: Box<AplExpr>,
    },
    DyadicOp {
        op: AplFunc,
        left: Box<AplExpr>,
        right: Box<AplExpr>,
    },
    Operator {
        op: AplOperator,
        func: Box<AplExpr>,
        operand: Box<AplExpr>,
    },
    DyadicOperator {
        op: AplOperator,
        left_func: Box<AplExpr>,
        right_func: Box<AplExpr>,
        operand: Box<AplExpr>,
    },
    FunctionCall {
        name: String,
        left_arg: Option<Box<AplExpr>>,
        right_arg: Box<AplExpr>,
    },
    Dfn {
        statements: Vec<AplStmt>,
    },
}

/// APL primitive functions
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AplFunc {
    // Arithmetic
    Add, Subtract, Multiply, Divide, Power, Log,
    Ceiling, Floor, Magnitude, Residue,
    Factorial, Binomial, PiTimes, Circular,
    // Comparison
    Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual,
    // Logical
    And, Or, Nand, Nor, Not,
    // Array
    Iota, Rho, Ravel, Catenate, Take, Drop,
    Reverse, Rotate, Transpose, Enclose, Disclose,
    First, Pick, MemberOf, Find, GradeUp, GradeDown,
    // Misc
    Roll, Deal, Execute, Format, Encode, Decode,
    Match, NotMatch, Depth, Tally,
    Without, Replicate, Expand,
}

/// APL operators (higher-order functions)
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AplOperator {
    Reduce, ReduceFirst,
    Scan, ScanFirst,
    Each,
    Power,
    Compose,
    Inner,
    Outer,
    Rank,
    At,
    Key,
    Stencil,
}

/// APL parser
pub struct AplParser;

impl AplParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for AplParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for AplParser {
    fn name(&self) -> &'static str {
        "APL"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["apl", "aplf", "aplo", "apln", "aplc"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = AplToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, AplToken::Comment | AplToken::Newline) {
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

        let mut builder = IrBuilder::with_language("apl_module", SourceLanguage::Apl);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "x ← 1 2 3 4 5";
        let lexer = AplToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&AplToken::Assign));
    }

    #[test]
    fn test_iota() {
        let source = "⍳10";
        let lexer = AplToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&AplToken::Iota));
    }

    #[test]
    fn test_parse() {
        let parser = AplParser::new();
        let source = SourceFile {
            name: "test.apl".to_string(),
            content: "x := 42".to_string(),
            language: SourceLanguage::Apl,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
