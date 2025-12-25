//! # Rosetta Pascal Frontend
//!
//! Parses Pascal source code (Standard Pascal, Turbo Pascal, Free Pascal) into Rosetta IR.
//!
//! ## Pascal History
//!
//! Designed by Niklaus Wirth (1970) for teaching structured programming.
//! Turbo Pascal (Borland, 1983) made it popular on PCs.
//!
//! ## Pascal to Rust Mapping
//!
//! | Pascal | Rust |
//! |--------|------|
//! | integer | i32 |
//! | real | f64 |
//! | boolean | bool |
//! | char | char |
//! | string | String |
//! | array[a..b] of T | [T; N] |
//! | record | struct |
//! | ^T (pointer) | Box<T> |
//! | set of T | HashSet<T> |
//! | file of T | File |
//! | procedure | fn(...) |
//! | function | fn(...) -> T |
//! | unit | mod |

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// Pascal token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum PascalToken {
    // Keywords
    #[token("program", ignore(ascii_case))]
    Program,
    #[token("unit", ignore(ascii_case))]
    Unit,
    #[token("interface", ignore(ascii_case))]
    Interface,
    #[token("implementation", ignore(ascii_case))]
    Implementation,
    #[token("uses", ignore(ascii_case))]
    Uses,
    #[token("const", ignore(ascii_case))]
    Const,
    #[token("var", ignore(ascii_case))]
    Var,
    #[token("type", ignore(ascii_case))]
    Type,
    #[token("label", ignore(ascii_case))]
    Label,
    #[token("procedure", ignore(ascii_case))]
    Procedure,
    #[token("function", ignore(ascii_case))]
    Function,
    #[token("begin", ignore(ascii_case))]
    Begin,
    #[token("end", ignore(ascii_case))]
    End,

    // Control flow
    #[token("if", ignore(ascii_case))]
    If,
    #[token("then", ignore(ascii_case))]
    Then,
    #[token("else", ignore(ascii_case))]
    Else,
    #[token("case", ignore(ascii_case))]
    Case,
    #[token("of", ignore(ascii_case))]
    Of,
    #[token("for", ignore(ascii_case))]
    For,
    #[token("to", ignore(ascii_case))]
    To,
    #[token("downto", ignore(ascii_case))]
    Downto,
    #[token("do", ignore(ascii_case))]
    Do,
    #[token("while", ignore(ascii_case))]
    While,
    #[token("repeat", ignore(ascii_case))]
    Repeat,
    #[token("until", ignore(ascii_case))]
    Until,
    #[token("with", ignore(ascii_case))]
    With,
    #[token("goto", ignore(ascii_case))]
    Goto,

    // Types
    #[token("array", ignore(ascii_case))]
    Array,
    #[token("record", ignore(ascii_case))]
    Record,
    #[token("set", ignore(ascii_case))]
    Set,
    #[token("file", ignore(ascii_case))]
    File,
    #[token("packed", ignore(ascii_case))]
    Packed,
    #[token("object", ignore(ascii_case))]
    Object,
    #[token("class", ignore(ascii_case))]
    Class,
    #[token("constructor", ignore(ascii_case))]
    Constructor,
    #[token("destructor", ignore(ascii_case))]
    Destructor,
    #[token("inherited", ignore(ascii_case))]
    Inherited,
    #[token("virtual", ignore(ascii_case))]
    Virtual,
    #[token("override", ignore(ascii_case))]
    Override,

    // Built-in types
    #[token("integer", ignore(ascii_case))]
    TyInteger,
    #[token("real", ignore(ascii_case))]
    TyReal,
    #[token("boolean", ignore(ascii_case))]
    TyBoolean,
    #[token("char", ignore(ascii_case))]
    TyChar,
    #[token("string", ignore(ascii_case))]
    TyString,
    #[token("byte", ignore(ascii_case))]
    TyByte,
    #[token("word", ignore(ascii_case))]
    TyWord,
    #[token("longint", ignore(ascii_case))]
    TyLongint,
    #[token("shortint", ignore(ascii_case))]
    TyShortint,
    #[token("single", ignore(ascii_case))]
    TySingle,
    #[token("double", ignore(ascii_case))]
    TyDouble,
    #[token("extended", ignore(ascii_case))]
    TyExtended,
    #[token("pointer", ignore(ascii_case))]
    TyPointer,

    // Operators and keywords
    #[token("and", ignore(ascii_case))]
    And,
    #[token("or", ignore(ascii_case))]
    Or,
    #[token("not", ignore(ascii_case))]
    Not,
    #[token("xor", ignore(ascii_case))]
    Xor,
    #[token("div", ignore(ascii_case))]
    Div,
    #[token("mod", ignore(ascii_case))]
    Mod,
    #[token("shl", ignore(ascii_case))]
    Shl,
    #[token("shr", ignore(ascii_case))]
    Shr,
    #[token("in", ignore(ascii_case))]
    In,
    #[token("nil", ignore(ascii_case))]
    Nil,

    // Boolean literals
    #[token("true", ignore(ascii_case))]
    True,
    #[token("false", ignore(ascii_case))]
    False,

    // Symbols
    #[token(":=")]
    Assign,
    #[token("..")]
    Range,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<>")]
    NotEqual,
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
    #[token(".")]
    Dot,
    #[token("^")]
    Caret,
    #[token("@")]
    At,
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
    RealLit(f64),
    #[regex(r"\$[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[1..], 16).ok())]
    HexLit(i64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    StringLit(String),
    #[regex(r"#[0-9]+", |lex| lex.slice()[1..].parse::<u8>().ok().map(|c| c as char))]
    CharCode(char),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comments
    #[regex(r"\{[^}]*\}")]
    BraceComment,
    #[regex(r"\(\*([^*]|\*[^)])*\*\)")]
    ParenComment,
    #[regex(r"//[^\n]*")]
    LineComment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// Pascal AST types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PascalAst {
    Program {
        name: String,
        uses: Vec<String>,
        declarations: Vec<PascalDecl>,
        body: Vec<PascalStmt>,
    },
    Unit {
        name: String,
        interface: Vec<PascalDecl>,
        implementation: Vec<PascalDecl>,
        initialization: Option<Vec<PascalStmt>>,
        finalization: Option<Vec<PascalStmt>>,
    },
}

/// Declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PascalDecl {
    Const {
        name: String,
        ty: Option<PascalType>,
        value: PascalExpr,
    },
    Var {
        names: Vec<String>,
        ty: PascalType,
        init: Option<PascalExpr>,
    },
    Type {
        name: String,
        def: PascalType,
    },
    Procedure {
        name: String,
        params: Vec<PascalParam>,
        local_decls: Vec<PascalDecl>,
        body: Vec<PascalStmt>,
    },
    Function {
        name: String,
        params: Vec<PascalParam>,
        return_type: PascalType,
        local_decls: Vec<PascalDecl>,
        body: Vec<PascalStmt>,
    },
    Label(Vec<String>),
}

/// Parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PascalParam {
    pub names: Vec<String>,
    pub ty: PascalType,
    pub is_var: bool,
    pub is_const: bool,
    pub is_out: bool,
}

/// Type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PascalType {
    Integer,
    Real,
    Boolean,
    Char,
    String(Option<i32>),
    Byte,
    Word,
    Longint,
    Shortint,
    Single,
    Double,
    Extended,
    Pointer,
    Named(String),
    Array {
        index_ranges: Vec<(PascalExpr, PascalExpr)>,
        element_type: Box<PascalType>,
        packed: bool,
    },
    Record {
        fields: Vec<RecordField>,
        variant: Option<Box<RecordVariant>>,
        packed: bool,
    },
    Set {
        base_type: Box<PascalType>,
    },
    File {
        element_type: Option<Box<PascalType>>,
    },
    Pointer_ {
        target_type: Box<PascalType>,
    },
    Subrange {
        low: Box<PascalExpr>,
        high: Box<PascalExpr>,
    },
    Enum {
        values: Vec<String>,
    },
    Object {
        parent: Option<String>,
        fields: Vec<RecordField>,
        methods: Vec<Box<PascalDecl>>,
    },
    Class {
        parent: Option<String>,
        fields: Vec<RecordField>,
        methods: Vec<Box<PascalDecl>>,
    },
    ProcedureType {
        params: Vec<PascalParam>,
    },
    FunctionType {
        params: Vec<PascalParam>,
        return_type: Box<PascalType>,
    },
}

/// Record field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordField {
    pub names: Vec<String>,
    pub ty: PascalType,
}

/// Record variant part
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordVariant {
    pub tag_field: Option<String>,
    pub tag_type: PascalType,
    pub variants: Vec<(Vec<PascalExpr>, Vec<RecordField>)>,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PascalStmt {
    Assignment {
        target: PascalExpr,
        value: PascalExpr,
    },
    If {
        condition: PascalExpr,
        then_part: Box<PascalStmt>,
        else_part: Option<Box<PascalStmt>>,
    },
    Case {
        selector: PascalExpr,
        branches: Vec<(Vec<PascalExpr>, PascalStmt)>,
        else_part: Option<Vec<PascalStmt>>,
    },
    For {
        var: String,
        start: PascalExpr,
        end: PascalExpr,
        downto: bool,
        body: Box<PascalStmt>,
    },
    While {
        condition: PascalExpr,
        body: Box<PascalStmt>,
    },
    Repeat {
        body: Vec<PascalStmt>,
        condition: PascalExpr,
    },
    With {
        vars: Vec<PascalExpr>,
        body: Box<PascalStmt>,
    },
    Compound(Vec<PascalStmt>),
    ProcedureCall {
        name: String,
        args: Vec<PascalExpr>,
    },
    Goto(String),
    Label(String, Box<PascalStmt>),
    Empty,
    Try {
        body: Vec<PascalStmt>,
        except: Option<Vec<PascalStmt>>,
        finally: Option<Vec<PascalStmt>>,
    },
    Raise(Option<PascalExpr>),
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PascalExpr {
    IntLit(i64),
    RealLit(f64),
    StringLit(String),
    CharLit(char),
    BoolLit(bool),
    Nil,
    Ident(String),
    Index {
        base: Box<PascalExpr>,
        indices: Vec<PascalExpr>,
    },
    Field {
        base: Box<PascalExpr>,
        field: String,
    },
    Deref(Box<PascalExpr>),
    AddrOf(Box<PascalExpr>),
    FunctionCall {
        name: String,
        args: Vec<PascalExpr>,
    },
    BinOp {
        op: PascalBinOp,
        left: Box<PascalExpr>,
        right: Box<PascalExpr>,
    },
    UnaryOp {
        op: PascalUnaryOp,
        operand: Box<PascalExpr>,
    },
    SetLit(Vec<PascalExpr>),
    Range {
        low: Box<PascalExpr>,
        high: Box<PascalExpr>,
    },
    TypeCast {
        ty: PascalType,
        expr: Box<PascalExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PascalBinOp {
    Add, Sub, Mul, Div, IntDiv, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor,
    Shl, Shr,
    In,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PascalUnaryOp {
    Neg, Not, Plus,
}

/// Pascal parser
pub struct PascalParser {
    dialect: PascalDialect,
}

/// Pascal dialect
#[derive(Debug, Clone, Copy)]
pub enum PascalDialect {
    Standard,
    TurboPascal,
    FreePascal,
    Delphi,
}

impl PascalParser {
    pub fn standard() -> Self {
        Self { dialect: PascalDialect::Standard }
    }

    pub fn turbo() -> Self {
        Self { dialect: PascalDialect::TurboPascal }
    }

    pub fn free_pascal() -> Self {
        Self { dialect: PascalDialect::FreePascal }
    }

    pub fn delphi() -> Self {
        Self { dialect: PascalDialect::Delphi }
    }
}

impl Frontend for PascalParser {
    fn name(&self) -> &'static str {
        match self.dialect {
            PascalDialect::Standard => "Standard Pascal",
            PascalDialect::TurboPascal => "Turbo Pascal",
            PascalDialect::FreePascal => "Free Pascal",
            PascalDialect::Delphi => "Delphi",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        match self.dialect {
            PascalDialect::Delphi => &["pas", "dpr", "dpk"],
            _ => &["pas", "pp", "p"],
        }
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = PascalToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, PascalToken::BraceComment | PascalToken::ParenComment |
                                   PascalToken::LineComment | PascalToken::Newline) {
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

        let mut builder = IrBuilder::with_language("pascal_module", SourceLanguage::Pascal);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let source = "program Hello; begin writeln('Hello'); end.";
        let mut lexer = PascalToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(PascalToken::Program)));
        assert_eq!(lexer.next(), Some(Ok(PascalToken::Ident("Hello".to_string()))));
    }

    #[test]
    fn test_parse_simple() {
        let parser = PascalParser::turbo();
        let source = SourceFile {
            name: "test.pas".to_string(),
            content: "program Test; begin end.".to_string(),
            language: SourceLanguage::Pascal,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_hex_literal() {
        let source = "$FF $1A2B";
        let lexer = PascalToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert_eq!(tokens[0], PascalToken::HexLit(255));
        assert_eq!(tokens[1], PascalToken::HexLit(0x1A2B));
    }
}
