//! # Rosetta ALGOL Frontend
//!
//! Parses ALGOL 60/68 source code into Rosetta IR.
//!
//! ## ALGOL History
//!
//! ALGOL (ALGOrithmic Language) was developed 1958-1960 as an international
//! standard for expressing algorithms. It introduced:
//! - Block structure with begin/end
//! - Lexical scoping
//! - Recursive procedures
//! - BNF notation (for defining its syntax)
//!
//! ALGOL 68 (1968) added orthogonal design, user-defined types, and more.

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// ALGOL 60 token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum AlgolToken {
    // Reserved words (using 'stropping' convention)
    #[token("begin", ignore(ascii_case))]
    #[token("'begin'")]
    Begin,
    #[token("end", ignore(ascii_case))]
    #[token("'end'")]
    End,
    #[token("if", ignore(ascii_case))]
    #[token("'if'")]
    If,
    #[token("then", ignore(ascii_case))]
    #[token("'then'")]
    Then,
    #[token("else", ignore(ascii_case))]
    #[token("'else'")]
    Else,
    #[token("for", ignore(ascii_case))]
    #[token("'for'")]
    For,
    #[token("do", ignore(ascii_case))]
    #[token("'do'")]
    Do,
    #[token("while", ignore(ascii_case))]
    #[token("'while'")]
    While,
    #[token("step", ignore(ascii_case))]
    #[token("'step'")]
    Step,
    #[token("until", ignore(ascii_case))]
    #[token("'until'")]
    Until,
    #[token("goto", ignore(ascii_case))]
    #[token("'goto'")]
    #[token("go to", ignore(ascii_case))]
    Goto,
    #[token("procedure", ignore(ascii_case))]
    #[token("'procedure'")]
    Procedure,
    #[token("value", ignore(ascii_case))]
    #[token("'value'")]
    Value,
    #[token("switch", ignore(ascii_case))]
    #[token("'switch'")]
    Switch,
    #[token("own", ignore(ascii_case))]
    #[token("'own'")]
    Own,
    #[token("comment", ignore(ascii_case))]
    Comment_,

    // Types (ALGOL 60)
    #[token("integer", ignore(ascii_case))]
    #[token("'integer'")]
    Integer,
    #[token("real", ignore(ascii_case))]
    #[token("'real'")]
    Real,
    #[token("boolean", ignore(ascii_case))]
    #[token("'boolean'")]
    Boolean,
    #[token("array", ignore(ascii_case))]
    #[token("'array'")]
    Array,
    #[token("string", ignore(ascii_case))]
    #[token("'string'")]
    String_,
    #[token("label", ignore(ascii_case))]
    #[token("'label'")]
    Label,

    // Boolean literals
    #[token("true", ignore(ascii_case))]
    #[token("'true'")]
    True,
    #[token("false", ignore(ascii_case))]
    #[token("'false'")]
    False,

    // Logical operators
    #[token("and", ignore(ascii_case))]
    #[token("'and'")]
    #[token("/\\")]
    And,
    #[token("or", ignore(ascii_case))]
    #[token("'or'")]
    #[token("\\/")]
    Or,
    #[token("not", ignore(ascii_case))]
    #[token("'not'")]
    #[token("~")]
    Not,
    #[token("imp", ignore(ascii_case))]
    #[token("'imp'")]
    #[token("=>")]
    Implies,
    #[token("equiv", ignore(ascii_case))]
    #[token("'equiv'")]
    #[token("<=>")]
    Equiv,

    // Relational operators
    #[token("<=")]
    #[token("=<")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<>")]
    #[token("~=")]
    NotEqual,

    // Arithmetic operators
    #[token(":=")]
    Assign,
    #[token("**")]
    #[token("^")]
    Power,
    #[token("//")]
    IntDiv,
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
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
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
    #[regex(r"[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    #[regex(r"\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    RealLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    #[regex(r"`[^']*'", |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StringLit(String),

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comment (ALGOL 60 style: from 'comment' to ';')
    #[regex(r"comment[^;]*;")]
    Comment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// ALGOL AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlgolProgram {
    pub blocks: Vec<AlgolBlock>,
}

/// Block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlgolBlock {
    pub declarations: Vec<AlgolDecl>,
    pub statements: Vec<AlgolStmt>,
}

/// Declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AlgolDecl {
    Variable {
        ty: AlgolType,
        names: Vec<String>,
        own: bool,
    },
    Array {
        ty: AlgolType,
        name: String,
        bounds: Vec<(AlgolExpr, AlgolExpr)>,
        own: bool,
    },
    Switch {
        name: String,
        designators: Vec<AlgolExpr>,
    },
    Procedure {
        name: String,
        ty: Option<AlgolType>,
        value_params: Vec<String>,
        params: Vec<(String, ParamSpec)>,
        body: Box<AlgolStmt>,
    },
}

/// Parameter specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParamSpec {
    Value(AlgolType),
    Name(AlgolType),
    Array { ty: AlgolType, dims: usize },
    Procedure { ty: Option<AlgolType> },
    Label,
    Switch,
    String,
}

/// Type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AlgolType {
    Integer,
    Real,
    Boolean,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AlgolStmt {
    Assignment {
        targets: Vec<AlgolExpr>,
        value: AlgolExpr,
    },
    Goto(AlgolExpr),
    Conditional {
        condition: AlgolExpr,
        then_part: Box<AlgolStmt>,
        else_part: Option<Box<AlgolStmt>>,
    },
    For {
        var: String,
        elements: Vec<ForElement>,
        body: Box<AlgolStmt>,
    },
    Compound(AlgolBlock),
    ProcedureCall {
        name: String,
        args: Vec<AlgolExpr>,
    },
    Label(String, Box<AlgolStmt>),
    Dummy,
}

/// For-list element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ForElement {
    Single(AlgolExpr),
    Step {
        start: AlgolExpr,
        step: AlgolExpr,
        until: AlgolExpr,
    },
    While {
        start: AlgolExpr,
        condition: AlgolExpr,
    },
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AlgolExpr {
    IntLit(i64),
    RealLit(f64),
    BoolLit(bool),
    StringLit(String),
    Ident(String),
    Subscript {
        array: String,
        indices: Vec<AlgolExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<AlgolExpr>,
    },
    BinOp {
        op: AlgolBinOp,
        left: Box<AlgolExpr>,
        right: Box<AlgolExpr>,
    },
    UnaryOp {
        op: AlgolUnaryOp,
        operand: Box<AlgolExpr>,
    },
    Conditional {
        condition: Box<AlgolExpr>,
        then_expr: Box<AlgolExpr>,
        else_expr: Box<AlgolExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AlgolBinOp {
    Add, Sub, Mul, Div, IntDiv, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Implies, Equiv,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AlgolUnaryOp {
    Neg, Not, Plus,
}

/// ALGOL parser
pub struct AlgolParser {
    version: AlgolVersion,
}

#[derive(Debug, Clone, Copy)]
pub enum AlgolVersion {
    Algol60,
    Algol68,
}

impl AlgolParser {
    pub fn algol60() -> Self {
        Self { version: AlgolVersion::Algol60 }
    }

    pub fn algol68() -> Self {
        Self { version: AlgolVersion::Algol68 }
    }
}

impl Frontend for AlgolParser {
    fn name(&self) -> &'static str {
        match self.version {
            AlgolVersion::Algol60 => "ALGOL 60",
            AlgolVersion::Algol68 => "ALGOL 68",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["alg", "a60", "a68"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = AlgolToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, AlgolToken::Comment | AlgolToken::Newline) {
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

        let mut builder = IrBuilder::with_language("algol_module", SourceLanguage::Algol60);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "begin integer x; x := 42 end";
        let lexer = AlgolToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&AlgolToken::Begin));
        assert!(tokens.contains(&AlgolToken::Integer));
    }

    #[test]
    fn test_parse() {
        let parser = AlgolParser::algol60();
        let source = SourceFile {
            name: "test.a60".to_string(),
            content: "begin end".to_string(),
            language: SourceLanguage::Algol60,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
