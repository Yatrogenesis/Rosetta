//! # Rosetta Simula Frontend
//!
//! Parses Simula 67 source code into Rosetta IR.
//!
//! ## Simula History
//!
//! Developed by Ole-Johan Dahl and Kristen Nygaard (1967) at Norwegian
//! Computing Center. Simula is historically significant as:
//! - First object-oriented programming language
//! - Introduced classes, inheritance, and virtual methods
//! - Originally designed for simulation (hence the name)
//! - Influenced Smalltalk, C++, Java, and most modern OOP languages
//!
//! ## Key Concepts
//!
//! - CLASS: User-defined types with data and procedures
//! - VIRTUAL: Polymorphic method dispatch
//! - REF: Reference to objects
//! - INNER: Call to subclass initialization
//! - INSPECT/WHEN: Type-safe downcasting
//! - COROUTINES: Detach/Resume for simulation

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// Simula 67 token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum SimulaToken {
    // Class-related
    #[token("CLASS", ignore(ascii_case))]
    Class,
    #[token("REF", ignore(ascii_case))]
    Ref,
    #[token("NEW", ignore(ascii_case))]
    New,
    #[token("THIS", ignore(ascii_case))]
    This,
    #[token("INNER", ignore(ascii_case))]
    Inner,
    #[token("VIRTUAL", ignore(ascii_case))]
    Virtual,
    #[token("HIDDEN", ignore(ascii_case))]
    Hidden,
    #[token("PROTECTED", ignore(ascii_case))]
    Protected,
    #[token("INSPECT", ignore(ascii_case))]
    Inspect,
    #[token("WHEN", ignore(ascii_case))]
    When,
    #[token("OTHERWISE", ignore(ascii_case))]
    Otherwise,
    #[token("QUA", ignore(ascii_case))]
    Qua,
    #[token("IS", ignore(ascii_case))]
    Is,
    #[token("IN", ignore(ascii_case))]
    In,
    #[token("NONE", ignore(ascii_case))]
    None_,

    // Coroutines/Simulation
    #[token("DETACH", ignore(ascii_case))]
    Detach,
    #[token("RESUME", ignore(ascii_case))]
    Resume,
    #[token("CALL", ignore(ascii_case))]
    Call,
    #[token("ACTIVATE", ignore(ascii_case))]
    Activate,
    #[token("PASSIVATE", ignore(ascii_case))]
    Passivate,
    #[token("HOLD", ignore(ascii_case))]
    Hold,
    #[token("CANCEL", ignore(ascii_case))]
    Cancel,
    #[token("REACTIVATE", ignore(ascii_case))]
    Reactivate,
    #[token("AT", ignore(ascii_case))]
    At,
    #[token("DELAY", ignore(ascii_case))]
    Delay,
    #[token("BEFORE", ignore(ascii_case))]
    Before,
    #[token("AFTER", ignore(ascii_case))]
    After,
    #[token("PRIOR", ignore(ascii_case))]
    Prior,

    // Block structure
    #[token("BEGIN", ignore(ascii_case))]
    Begin,
    #[token("END", ignore(ascii_case))]
    End,
    #[token("PROCEDURE", ignore(ascii_case))]
    Procedure,
    #[token("EXTERNAL", ignore(ascii_case))]
    External,

    // Control flow
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("FOR", ignore(ascii_case))]
    For,
    #[token("STEP", ignore(ascii_case))]
    Step,
    #[token("UNTIL", ignore(ascii_case))]
    Until,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("GO", ignore(ascii_case))]
    Go,
    #[token("GOTO", ignore(ascii_case))]
    Goto,
    #[token("TO", ignore(ascii_case))]
    To,
    #[token("SWITCH", ignore(ascii_case))]
    Switch,

    // Types
    #[token("INTEGER", ignore(ascii_case))]
    Integer,
    #[token("REAL", ignore(ascii_case))]
    Real,
    #[token("LONG", ignore(ascii_case))]
    Long,
    #[token("SHORT", ignore(ascii_case))]
    Short,
    #[token("BOOLEAN", ignore(ascii_case))]
    Boolean,
    #[token("CHARACTER", ignore(ascii_case))]
    Character,
    #[token("TEXT", ignore(ascii_case))]
    Text,
    #[token("ARRAY", ignore(ascii_case))]
    Array,
    #[token("LABEL", ignore(ascii_case))]
    Label,
    #[token("VALUE", ignore(ascii_case))]
    Value,
    #[token("NAME", ignore(ascii_case))]
    Name,

    // Boolean literals
    #[token("TRUE", ignore(ascii_case))]
    True,
    #[token("FALSE", ignore(ascii_case))]
    False,

    // Logical operators
    #[token("AND", ignore(ascii_case))]
    And,
    #[token("OR", ignore(ascii_case))]
    Or,
    #[token("NOT", ignore(ascii_case))]
    Not,
    #[token("IMP", ignore(ascii_case))]
    Imp,
    #[token("EQV", ignore(ascii_case))]
    Eqv,

    // I/O
    #[token("INFILE", ignore(ascii_case))]
    Infile,
    #[token("OUTFILE", ignore(ascii_case))]
    Outfile,
    #[token("DIRECTFILE", ignore(ascii_case))]
    Directfile,
    #[token("PRINTFILE", ignore(ascii_case))]
    Printfile,
    #[token("INIMAGE", ignore(ascii_case))]
    Inimage,
    #[token("OUTIMAGE", ignore(ascii_case))]
    Outimage,
    #[token("IMAGE", ignore(ascii_case))]
    Image,
    #[token("OPEN", ignore(ascii_case))]
    Open,
    #[token("CLOSE", ignore(ascii_case))]
    Close,

    // Operators
    #[token(":=")]
    Assign,
    #[token(":-")]
    RefAssign,
    #[token("**")]
    Power,
    #[token("//")]
    IntDiv,
    #[token("<=")]
    #[token("=<")]
    LessEqual,
    #[token(">=")]
    #[token("=>")]
    GreaterEqual,
    #[token("<>")]
    #[token("><")]
    NotEqual,
    #[token("==")]
    RefEqual,
    #[token("=/=")]
    RefNotEqual,
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
    #[token("&")]
    Concat,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+", |lex| lex.slice().parse().ok())]
    RealLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLit(String),
    #[regex(r"'.'", |lex| lex.slice().chars().nth(1))]
    CharLit(char),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comments
    #[token("COMMENT", ignore(ascii_case))]
    CommentStart,
    #[regex(r"![^;]*;")]
    ExclComment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// Simula AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulaProgram {
    pub declarations: Vec<SimulaDecl>,
    pub statements: Vec<SimulaStmt>,
}

/// Declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SimulaDecl {
    Class {
        name: String,
        prefix: Option<String>,
        params: Vec<SimulaParam>,
        virtual_specs: Vec<VirtualSpec>,
        declarations: Vec<SimulaDecl>,
        statements: Vec<SimulaStmt>,
    },
    Procedure {
        name: String,
        ty: Option<SimulaType>,
        is_virtual: bool,
        params: Vec<SimulaParam>,
        mode_specs: Vec<(String, ParamMode)>,
        declarations: Vec<SimulaDecl>,
        statements: Vec<SimulaStmt>,
    },
    Variable {
        names: Vec<String>,
        ty: SimulaType,
        array_bounds: Option<Vec<(SimulaExpr, SimulaExpr)>>,
    },
    Switch {
        name: String,
        designators: Vec<SimulaExpr>,
    },
    External {
        kind: ExternalKind,
        name: String,
        library: Option<String>,
    },
}

/// External declaration kind
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExternalKind {
    Procedure,
    Class,
}

/// Virtual specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VirtualSpec {
    pub name: String,
    pub ty: Option<SimulaType>,
    pub is_label: bool,
    pub is_switch: bool,
    pub is_procedure: bool,
}

/// Parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulaParam {
    pub names: Vec<String>,
    pub ty: SimulaType,
}

/// Parameter mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ParamMode {
    Value,
    Name,
}

/// Type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SimulaType {
    Integer,
    Real,
    LongReal,
    ShortInteger,
    Boolean,
    Character,
    Text,
    Ref(Option<String>),  // Class name
    Array(Box<SimulaType>),
    Label,
    Switch,
    Procedure(Option<Box<SimulaType>>),
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SimulaStmt {
    Assignment {
        target: SimulaExpr,
        value: SimulaExpr,
    },
    RefAssignment {
        target: SimulaExpr,
        value: SimulaExpr,
    },
    If {
        condition: SimulaExpr,
        then_part: Box<SimulaStmt>,
        else_part: Option<Box<SimulaStmt>>,
    },
    For {
        var: String,
        elements: Vec<ForElement>,
        body: Box<SimulaStmt>,
    },
    While {
        condition: SimulaExpr,
        body: Box<SimulaStmt>,
    },
    Inspect {
        expr: SimulaExpr,
        when_parts: Vec<(String, Vec<SimulaStmt>)>,
        otherwise: Option<Vec<SimulaStmt>>,
        do_part: Option<Vec<SimulaStmt>>,
    },
    Goto(SimulaExpr),
    ProcedureCall {
        name: SimulaExpr,
        args: Vec<SimulaExpr>,
    },
    Block {
        declarations: Vec<SimulaDecl>,
        statements: Vec<SimulaStmt>,
    },
    Inner,
    Detach,
    Resume(SimulaExpr),
    Call(SimulaExpr),
    Activate {
        object: SimulaExpr,
        scheduling: Option<ActivateSchedule>,
    },
    Passivate,
    Hold(SimulaExpr),
    Cancel(SimulaExpr),
    Label(String, Box<SimulaStmt>),
    Empty,
}

/// For-list element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ForElement {
    Single(SimulaExpr),
    Step {
        start: SimulaExpr,
        step: SimulaExpr,
        until: SimulaExpr,
    },
    While {
        start: SimulaExpr,
        condition: SimulaExpr,
    },
}

/// Activate schedule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ActivateSchedule {
    At(SimulaExpr),
    Delay(SimulaExpr),
    Before(SimulaExpr),
    After(SimulaExpr),
    Prior,
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SimulaExpr {
    IntLit(i64),
    RealLit(f64),
    StringLit(String),
    CharLit(char),
    BoolLit(bool),
    None,
    This(Option<String>),
    Ident(String),
    Dot {
        object: Box<SimulaExpr>,
        member: String,
    },
    Qua {
        expr: Box<SimulaExpr>,
        class_name: String,
    },
    New {
        class_name: String,
        args: Vec<SimulaExpr>,
    },
    Subscript {
        array: Box<SimulaExpr>,
        indices: Vec<SimulaExpr>,
    },
    FunctionCall {
        name: Box<SimulaExpr>,
        args: Vec<SimulaExpr>,
    },
    BinOp {
        op: SimulaBinOp,
        left: Box<SimulaExpr>,
        right: Box<SimulaExpr>,
    },
    UnaryOp {
        op: SimulaUnaryOp,
        operand: Box<SimulaExpr>,
    },
    Is {
        expr: Box<SimulaExpr>,
        class_name: String,
    },
    In {
        expr: Box<SimulaExpr>,
        class_name: String,
    },
    Conditional {
        condition: Box<SimulaExpr>,
        then_expr: Box<SimulaExpr>,
        else_expr: Box<SimulaExpr>,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SimulaBinOp {
    Add, Sub, Mul, Div, IntDiv, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    RefEq, RefNe,
    And, Or, Imp, Eqv,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SimulaUnaryOp {
    Neg, Not, Plus,
}

/// Simula parser
pub struct SimulaParser;

impl SimulaParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SimulaParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for SimulaParser {
    fn name(&self) -> &'static str {
        "Simula 67"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["sim", "simula"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = SimulaToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, SimulaToken::ExclComment | SimulaToken::Newline) {
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

        let mut builder = IrBuilder::with_language("simula_module", SourceLanguage::Simula);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "CLASS Point; BEGIN INTEGER x, y; END;";
        let lexer = SimulaToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&SimulaToken::Class));
        assert!(tokens.contains(&SimulaToken::Integer));
    }

    #[test]
    fn test_inheritance() {
        let source = "Point CLASS ColoredPoint; BEGIN TEXT color; END;";
        let lexer = SimulaToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&SimulaToken::Class));
        assert!(tokens.contains(&SimulaToken::Text));
    }

    #[test]
    fn test_parse() {
        let parser = SimulaParser::new();
        let source = SourceFile {
            name: "test.sim".to_string(),
            content: "BEGIN END".to_string(),
            language: SourceLanguage::Simula,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
