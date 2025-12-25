//! # Rosetta Ada Frontend
//!
//! Parses Ada source code (Ada 83/95/2005/2012) into Rosetta IR.
//!
//! ## Ada Language Features
//!
//! Ada is a strongly-typed, military-grade language designed for:
//! - Embedded systems and real-time computing
//! - Safety-critical systems (avionics, medical devices)
//! - Concurrent programming with tasks
//!
//! ## Ada to Rust Mapping
//!
//! | Ada | Rust |
//! |-----|------|
//! | Integer | i32 |
//! | Float | f32 |
//! | Long_Float | f64 |
//! | Boolean | bool |
//! | Character | char |
//! | String | String |
//! | array(1..N) of T | [T; N] |
//! | access T | Box<T> |
//! | record | struct |
//! | task | async fn / tokio::task |
//! | package | mod |
//! | procedure | fn(...) |
//! | function | fn(...) -> T |
//! | generic | <T> generics |

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// Ada token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum AdaToken {
    // Keywords
    #[token("abort", ignore(ascii_case))]
    Abort,
    #[token("abs", ignore(ascii_case))]
    Abs,
    #[token("abstract", ignore(ascii_case))]
    Abstract,
    #[token("accept", ignore(ascii_case))]
    Accept,
    #[token("access", ignore(ascii_case))]
    Access,
    #[token("aliased", ignore(ascii_case))]
    Aliased,
    #[token("all", ignore(ascii_case))]
    All,
    #[token("and", ignore(ascii_case))]
    And,
    #[token("array", ignore(ascii_case))]
    Array,
    #[token("at", ignore(ascii_case))]
    At,
    #[token("begin", ignore(ascii_case))]
    Begin,
    #[token("body", ignore(ascii_case))]
    Body,
    #[token("case", ignore(ascii_case))]
    Case,
    #[token("constant", ignore(ascii_case))]
    Constant,
    #[token("declare", ignore(ascii_case))]
    Declare,
    #[token("delay", ignore(ascii_case))]
    Delay,
    #[token("delta", ignore(ascii_case))]
    Delta,
    #[token("digits", ignore(ascii_case))]
    Digits,
    #[token("do", ignore(ascii_case))]
    Do,
    #[token("else", ignore(ascii_case))]
    Else,
    #[token("elsif", ignore(ascii_case))]
    Elsif,
    #[token("end", ignore(ascii_case))]
    End,
    #[token("entry", ignore(ascii_case))]
    Entry,
    #[token("exception", ignore(ascii_case))]
    Exception,
    #[token("exit", ignore(ascii_case))]
    Exit,
    #[token("for", ignore(ascii_case))]
    For,
    #[token("function", ignore(ascii_case))]
    Function,
    #[token("generic", ignore(ascii_case))]
    Generic,
    #[token("goto", ignore(ascii_case))]
    Goto,
    #[token("if", ignore(ascii_case))]
    If,
    #[token("in", ignore(ascii_case))]
    In,
    #[token("interface", ignore(ascii_case))]
    Interface,
    #[token("is", ignore(ascii_case))]
    Is,
    #[token("limited", ignore(ascii_case))]
    Limited,
    #[token("loop", ignore(ascii_case))]
    Loop,
    #[token("mod", ignore(ascii_case))]
    Mod,
    #[token("new", ignore(ascii_case))]
    New,
    #[token("not", ignore(ascii_case))]
    Not,
    #[token("null", ignore(ascii_case))]
    Null,
    #[token("of", ignore(ascii_case))]
    Of,
    #[token("or", ignore(ascii_case))]
    Or,
    #[token("others", ignore(ascii_case))]
    Others,
    #[token("out", ignore(ascii_case))]
    Out,
    #[token("overriding", ignore(ascii_case))]
    Overriding,
    #[token("package", ignore(ascii_case))]
    Package,
    #[token("pragma", ignore(ascii_case))]
    Pragma,
    #[token("private", ignore(ascii_case))]
    Private,
    #[token("procedure", ignore(ascii_case))]
    Procedure,
    #[token("protected", ignore(ascii_case))]
    Protected,
    #[token("raise", ignore(ascii_case))]
    Raise,
    #[token("range", ignore(ascii_case))]
    Range,
    #[token("record", ignore(ascii_case))]
    Record,
    #[token("rem", ignore(ascii_case))]
    Rem,
    #[token("renames", ignore(ascii_case))]
    Renames,
    #[token("requeue", ignore(ascii_case))]
    Requeue,
    #[token("return", ignore(ascii_case))]
    Return,
    #[token("reverse", ignore(ascii_case))]
    Reverse,
    #[token("select", ignore(ascii_case))]
    Select,
    #[token("separate", ignore(ascii_case))]
    Separate,
    #[token("some", ignore(ascii_case))]
    Some,
    #[token("subtype", ignore(ascii_case))]
    Subtype,
    #[token("synchronized", ignore(ascii_case))]
    Synchronized,
    #[token("tagged", ignore(ascii_case))]
    Tagged,
    #[token("task", ignore(ascii_case))]
    Task,
    #[token("terminate", ignore(ascii_case))]
    Terminate,
    #[token("then", ignore(ascii_case))]
    Then,
    #[token("type", ignore(ascii_case))]
    Type,
    #[token("until", ignore(ascii_case))]
    Until,
    #[token("use", ignore(ascii_case))]
    Use,
    #[token("when", ignore(ascii_case))]
    When,
    #[token("while", ignore(ascii_case))]
    While,
    #[token("with", ignore(ascii_case))]
    With,
    #[token("xor", ignore(ascii_case))]
    Xor,

    // Built-in types
    #[token("Integer", ignore(ascii_case))]
    TyInteger,
    #[token("Float", ignore(ascii_case))]
    TyFloat,
    #[token("Boolean", ignore(ascii_case))]
    TyBoolean,
    #[token("Character", ignore(ascii_case))]
    TyCharacter,
    #[token("String", ignore(ascii_case))]
    TyString,
    #[token("Natural", ignore(ascii_case))]
    TyNatural,
    #[token("Positive", ignore(ascii_case))]
    TyPositive,

    // Boolean literals
    #[token("True", ignore(ascii_case))]
    True,
    #[token("False", ignore(ascii_case))]
    False,

    // Operators
    #[token(":=")]
    Assign,
    #[token("=>")]
    Arrow,
    #[token("..")]
    Range_,
    #[token("**")]
    Power,
    #[token("/=")]
    NotEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<=")]
    LessEqual,
    #[token("<<")]
    LabelStart,
    #[token(">>")]
    LabelEnd,
    #[token("<>")]
    Box,
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
    #[token("'")]
    Tick,
    #[token("|")]
    Bar,
    #[token("&")]
    Ampersand,
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
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().replace('_', "").parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?", |lex| lex.slice().replace('_', "").parse().ok())]
    RealLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLit(String),
    #[regex(r"'.'", |lex| lex.slice().chars().nth(1))]
    CharLit(char),

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comments
    #[regex(r"--[^\n]*")]
    Comment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// Ada AST types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaAst {
    CompilationUnit {
        context_clauses: Vec<ContextClause>,
        unit: Box<AdaUnit>,
    },
}

/// Context clause (with/use)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContextClause {
    With(Vec<String>),
    Use(Vec<String>),
}

/// Ada program unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaUnit {
    Package {
        name: String,
        is_body: bool,
        declarations: Vec<AdaDecl>,
        private_part: Option<Vec<AdaDecl>>,
    },
    Procedure {
        name: String,
        params: Vec<AdaParam>,
        declarations: Vec<AdaDecl>,
        statements: Vec<AdaStmt>,
    },
    Function {
        name: String,
        params: Vec<AdaParam>,
        return_type: AdaType,
        declarations: Vec<AdaDecl>,
        statements: Vec<AdaStmt>,
    },
    Task {
        name: String,
        entries: Vec<AdaEntry>,
        body: Vec<AdaStmt>,
    },
    Generic {
        formal_params: Vec<GenericParam>,
        unit: Box<AdaUnit>,
    },
}

/// Generic formal parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenericParam {
    Type { name: String, is_private: bool },
    Object { name: String, ty: AdaType, mode: ParamMode },
    Subprogram { name: String, signature: String },
}

/// Task entry declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdaEntry {
    pub name: String,
    pub params: Vec<AdaParam>,
}

/// Parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdaParam {
    pub name: String,
    pub ty: AdaType,
    pub mode: ParamMode,
    pub default: Option<AdaExpr>,
}

/// Parameter mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ParamMode {
    In,
    Out,
    InOut,
    Access,
}

/// Ada type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaType {
    Integer,
    Float,
    Boolean,
    Character,
    String,
    Natural,
    Positive,
    Named(String),
    Array {
        index_type: Box<AdaType>,
        element_type: Box<AdaType>,
    },
    Record {
        fields: Vec<(String, AdaType)>,
    },
    Access(Box<AdaType>),
    Range {
        low: Box<AdaExpr>,
        high: Box<AdaExpr>,
    },
    Subtype {
        base: Box<AdaType>,
        constraint: Option<Box<AdaExpr>>,
    },
    Tagged {
        parent: Option<Box<AdaType>>,
        fields: Vec<(String, AdaType)>,
    },
}

/// Declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaDecl {
    Variable {
        names: Vec<String>,
        ty: AdaType,
        init: Option<AdaExpr>,
        is_constant: bool,
    },
    Type {
        name: String,
        def: AdaType,
    },
    Subtype {
        name: String,
        base: AdaType,
        constraint: Option<AdaExpr>,
    },
    Procedure {
        name: String,
        params: Vec<AdaParam>,
    },
    Function {
        name: String,
        params: Vec<AdaParam>,
        return_type: AdaType,
    },
    Exception(String),
    Pragma {
        name: String,
        args: Vec<AdaExpr>,
    },
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaStmt {
    Assignment {
        target: AdaExpr,
        value: AdaExpr,
    },
    If {
        condition: AdaExpr,
        then_part: Vec<AdaStmt>,
        elsif_parts: Vec<(AdaExpr, Vec<AdaStmt>)>,
        else_part: Option<Vec<AdaStmt>>,
    },
    Case {
        selector: AdaExpr,
        alternatives: Vec<(Vec<AdaExpr>, Vec<AdaStmt>)>,
        others: Option<Vec<AdaStmt>>,
    },
    Loop {
        label: Option<String>,
        scheme: LoopScheme,
        body: Vec<AdaStmt>,
    },
    Block {
        label: Option<String>,
        declarations: Vec<AdaDecl>,
        statements: Vec<AdaStmt>,
        exception_handlers: Vec<ExceptionHandler>,
    },
    Return(Option<AdaExpr>),
    Exit {
        loop_name: Option<String>,
        condition: Option<AdaExpr>,
    },
    Null,
    Call {
        name: String,
        args: Vec<AdaExpr>,
    },
    Raise {
        exception: Option<String>,
        message: Option<AdaExpr>,
    },
    Accept {
        entry: String,
        params: Vec<AdaParam>,
        body: Vec<AdaStmt>,
    },
    Select {
        alternatives: Vec<SelectAlternative>,
    },
    Delay {
        is_until: bool,
        duration: AdaExpr,
    },
    Requeue {
        entry: String,
        with_abort: bool,
    },
}

/// Loop scheme
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoopScheme {
    Basic,
    While(AdaExpr),
    For {
        var: String,
        range: AdaExpr,
        reverse: bool,
    },
}

/// Exception handler
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExceptionHandler {
    pub exceptions: Vec<String>,
    pub statements: Vec<AdaStmt>,
}

/// Select alternative
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SelectAlternative {
    Accept {
        entry: String,
        params: Vec<AdaParam>,
        body: Vec<AdaStmt>,
    },
    Delay {
        duration: AdaExpr,
        body: Vec<AdaStmt>,
    },
    Terminate,
    Else(Vec<AdaStmt>),
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdaExpr {
    IntLit(i64),
    RealLit(f64),
    StringLit(String),
    CharLit(char),
    BoolLit(bool),
    Null,
    Name(String),
    Qualified {
        ty: String,
        expr: Box<AdaExpr>,
    },
    Attribute {
        prefix: Box<AdaExpr>,
        attribute: String,
        args: Option<Vec<AdaExpr>>,
    },
    Index {
        prefix: Box<AdaExpr>,
        indices: Vec<AdaExpr>,
    },
    Slice {
        prefix: Box<AdaExpr>,
        range: Box<AdaExpr>,
    },
    Selected {
        prefix: Box<AdaExpr>,
        selector: String,
    },
    Call {
        name: Box<AdaExpr>,
        args: Vec<AdaExpr>,
    },
    BinOp {
        op: AdaBinOp,
        left: Box<AdaExpr>,
        right: Box<AdaExpr>,
    },
    UnaryOp {
        op: AdaUnaryOp,
        operand: Box<AdaExpr>,
    },
    Range {
        low: Box<AdaExpr>,
        high: Box<AdaExpr>,
    },
    Aggregate(Vec<AggregateComponent>),
    Allocator {
        ty: AdaType,
        init: Option<Box<AdaExpr>>,
    },
    IfExpr {
        condition: Box<AdaExpr>,
        then_expr: Box<AdaExpr>,
        else_expr: Box<AdaExpr>,
    },
    CaseExpr {
        selector: Box<AdaExpr>,
        alternatives: Vec<(Vec<AdaExpr>, AdaExpr)>,
    },
    Quantified {
        quantifier: Quantifier,
        var: String,
        range: Box<AdaExpr>,
        predicate: Box<AdaExpr>,
    },
}

/// Aggregate component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AggregateComponent {
    Positional(AdaExpr),
    Named { choices: Vec<AdaExpr>, value: AdaExpr },
    Others(AdaExpr),
}

/// Quantifier
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Quantifier {
    ForAll,
    ForSome,
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AdaBinOp {
    Add, Sub, Mul, Div, Mod, Rem, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor, AndThen, OrElse,
    Concat,
    In, NotIn,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AdaUnaryOp {
    Neg, Not, Abs,
}

/// Ada parser
pub struct AdaParser {
    version: AdaVersion,
}

/// Ada language version
#[derive(Debug, Clone, Copy)]
pub enum AdaVersion {
    Ada83,
    Ada95,
    Ada2005,
    Ada2012,
}

impl AdaParser {
    pub fn ada83() -> Self {
        Self { version: AdaVersion::Ada83 }
    }

    pub fn ada95() -> Self {
        Self { version: AdaVersion::Ada95 }
    }

    pub fn ada2005() -> Self {
        Self { version: AdaVersion::Ada2005 }
    }

    pub fn ada2012() -> Self {
        Self { version: AdaVersion::Ada2012 }
    }
}

impl Frontend for AdaParser {
    fn name(&self) -> &'static str {
        match self.version {
            AdaVersion::Ada83 => "Ada 83",
            AdaVersion::Ada95 => "Ada 95",
            AdaVersion::Ada2005 => "Ada 2005",
            AdaVersion::Ada2012 => "Ada 2012",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["ada", "adb", "ads"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = AdaToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, AdaToken::Comment | AdaToken::Newline) {
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

        let mut builder = IrBuilder::with_language("ada_module", SourceLanguage::Ada);

        // Parse and convert to IR
        // TODO: Full parsing implementation

        Ok(builder.build().into_rosetta_ir())
    }
}

/// Convert Ada AST to Rosetta IR
pub struct AdaToIr {
    builder: IrBuilder,
}

impl AdaToIr {
    pub fn new(name: &str) -> Self {
        Self {
            builder: IrBuilder::with_language(name, SourceLanguage::Ada),
        }
    }

    pub fn convert(&mut self, _ast: &AdaAst) -> rosetta_core::Result<rosetta_ir::IrModule> {
        // TODO: Full conversion
        Ok(self.builder.build_clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let source = "procedure Main is begin null; end Main;";
        let mut lexer = AdaToken::lexer(source);

        assert_eq!(lexer.next(), Some(Ok(AdaToken::Procedure)));
        assert_eq!(lexer.next(), Some(Ok(AdaToken::Ident("Main".to_string()))));
    }

    #[test]
    fn test_parse_simple() {
        let parser = AdaParser::ada2012();
        let source = SourceFile {
            name: "test.adb".to_string(),
            content: "procedure Hello is begin null; end Hello;".to_string(),
            language: SourceLanguage::Ada,
        };
        let result = parser.parse(&source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_task_syntax() {
        let source = "task body Worker is begin accept Start; end Worker;";
        let lexer = AdaToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&AdaToken::Task));
        assert!(tokens.contains(&AdaToken::Accept));
    }
}
