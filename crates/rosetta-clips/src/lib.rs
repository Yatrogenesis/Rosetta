//! # Rosetta CLIPS Frontend
//!
//! Parses CLIPS (C Language Integrated Production System) source code into Rosetta IR.
//!
//! ## CLIPS History
//!
//! Developed at NASA's Johnson Space Center (1985) as a public domain
//! alternative to commercial expert system shells like OPS5 and ART.
//!
//! ## Key Features
//!
//! - Rule-based production system (forward chaining)
//! - Object-oriented programming (COOL)
//! - Deftemplate for structured facts
//! - Pattern matching with wildcards and constraints
//! - Salience for rule priority

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// CLIPS token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum ClipsToken {
    // Constructs
    #[token("defrule")]
    Defrule,
    #[token("deffacts")]
    Deffacts,
    #[token("deftemplate")]
    Deftemplate,
    #[token("deffunction")]
    Deffunction,
    #[token("defglobal")]
    Defglobal,
    #[token("defclass")]
    Defclass,
    #[token("definstances")]
    Definstances,
    #[token("defmessage-handler")]
    DefmessageHandler,
    #[token("defmethod")]
    Defmethod,
    #[token("defgeneric")]
    Defgeneric,
    #[token("defmodule")]
    Defmodule,

    // Rule structure
    #[token("=>")]
    Arrow,
    #[token("declare")]
    Declare,
    #[token("salience")]
    Salience,
    #[token("auto-focus")]
    AutoFocus,

    // Pattern elements
    #[token("test")]
    Test,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("exists")]
    Exists,
    #[token("forall")]
    Forall,
    #[token("logical")]
    Logical,
    #[token("object")]
    Object,

    // Constraints
    #[token("&")]
    Ampersand,
    #[token("|")]
    Bar,
    #[token("~")]
    Tilde,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,

    // Template slots
    #[token("slot")]
    Slot,
    #[token("multislot")]
    Multislot,
    #[token("type")]
    Type,
    #[token("default")]
    Default,
    #[token("default-dynamic")]
    DefaultDynamic,
    #[token("range")]
    Range,
    #[token("cardinality")]
    Cardinality,
    #[token("allowed-values")]
    AllowedValues,
    #[token("allowed-symbols")]
    AllowedSymbols,
    #[token("allowed-strings")]
    AllowedStrings,
    #[token("allowed-numbers")]
    AllowedNumbers,
    #[token("allowed-integers")]
    AllowedIntegers,
    #[token("allowed-floats")]
    AllowedFloats,

    // Class attributes
    #[token("is-a")]
    IsA,
    #[token("role")]
    Role,
    #[token("pattern-match")]
    PatternMatch,
    #[token("reactive")]
    Reactive,
    #[token("non-reactive")]
    NonReactive,
    #[token("abstract")]
    Abstract,
    #[token("concrete")]
    Concrete,

    // Slot facets
    #[token("visibility")]
    Visibility,
    #[token("public")]
    Public,
    #[token("private")]
    Private,
    #[token("access")]
    Access,
    #[token("read-write")]
    ReadWrite,
    #[token("read-only")]
    ReadOnly,
    #[token("initialize-only")]
    InitializeOnly,
    #[token("storage")]
    Storage,
    #[token("local")]
    Local,
    #[token("shared")]
    Shared,
    #[token("propagation")]
    Propagation,
    #[token("inherit")]
    Inherit,
    #[token("no-inherit")]
    NoInherit,
    #[token("source")]
    Source,
    #[token("exclusive")]
    Exclusive,
    #[token("composite")]
    Composite,
    #[token("create-accessor")]
    CreateAccessor,
    #[token("override-message")]
    OverrideMessage,

    // Message handler types
    #[token("primary")]
    Primary,
    #[token("around")]
    Around,
    #[token("before")]
    Before,
    #[token("after")]
    After,

    // Actions
    #[token("assert")]
    Assert,
    #[token("retract")]
    Retract,
    #[token("modify")]
    Modify,
    #[token("duplicate")]
    Duplicate,
    #[token("bind")]
    Bind,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("loop-for-count")]
    LoopForCount,
    #[token("progn")]
    Progn,
    #[token("progn$")]
    PrognDollar,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("foreach")]
    Foreach,

    // Built-in functions
    #[token("printout")]
    Printout,
    #[token("read")]
    Read,
    #[token("readline")]
    Readline,
    #[token("open")]
    Open,
    #[token("close")]
    Close,
    #[token("format")]
    Format,
    #[token("halt")]
    Halt,
    #[token("run")]
    Run,
    #[token("reset")]
    Reset,
    #[token("clear")]
    Clear,
    #[token("facts")]
    Facts,
    #[token("rules")]
    Rules,
    #[token("agenda")]
    Agenda,
    #[token("focus")]
    Focus,
    #[token("get-focus")]
    GetFocus,
    #[token("pop-focus")]
    PopFocus,
    #[token("refresh")]
    Refresh,
    #[token("watch")]
    Watch,
    #[token("unwatch")]
    Unwatch,
    #[token("dribble-on")]
    DribbleOn,
    #[token("dribble-off")]
    DribbleOff,
    #[token("batch")]
    Batch,
    #[token("batch*")]
    BatchStar,
    #[token("load")]
    Load,
    #[token("save")]
    Save,
    #[token("exit")]
    Exit,

    // Math functions
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*", priority = 2)]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    Power,
    #[token("div")]
    Div,
    #[token("mod")]
    Mod,
    #[token("max")]
    Max,
    #[token("min")]
    Min,
    #[token("abs")]
    Abs,
    #[token("sqrt")]
    Sqrt,
    #[token("exp")]
    Exp,
    #[token("log")]
    Log,
    #[token("log10")]
    Log10,
    #[token("sin")]
    Sin,
    #[token("cos")]
    Cos,
    #[token("tan")]
    Tan,
    #[token("round")]
    Round,
    #[token("truncate")]
    Truncate,
    #[token("float")]
    Float,
    #[token("integer")]
    Integer,

    // Comparison
    #[token("eq")]
    Eq,
    #[token("neq")]
    Neq,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,

    // String functions
    #[token("str-cat")]
    StrCat,
    #[token("str-index")]
    StrIndex,
    #[token("str-length")]
    StrLength,
    #[token("sub-string")]
    SubString,
    #[token("upcase")]
    Upcase,
    #[token("lowcase")]
    Lowcase,
    #[token("sym-cat")]
    SymCat,

    // Multifield functions
    #[token("create$")]
    CreateDollar,
    #[token("nth$")]
    NthDollar,
    #[token("first$")]
    FirstDollar,
    #[token("rest$")]
    RestDollar,
    #[token("length$")]
    LengthDollar,
    #[token("member$")]
    MemberDollar,
    #[token("subseq$")]
    SubseqDollar,
    #[token("delete$")]
    DeleteDollar,
    #[token("insert$")]
    InsertDollar,
    #[token("replace$")]
    ReplaceDollar,
    #[token("implode$")]
    ImplodeDollar,
    #[token("explode$")]
    ExplodeDollar,

    // Type functions
    #[token("numberp")]
    Numberp,
    #[token("integerp")]
    Integerp,
    #[token("floatp")]
    Floatp,
    #[token("stringp")]
    Stringp,
    #[token("symbolp")]
    Symbolp,
    #[token("multifieldp")]
    Multifieldp,
    #[token("lexemep")]
    Lexemep,

    // Fact functions
    #[token("fact-index")]
    FactIndex,
    #[token("fact-existp")]
    FactExistp,
    #[token("fact-slot-value")]
    FactSlotValue,
    #[token("fact-slot-names")]
    FactSlotNames,

    // Instance functions
    #[token("make-instance")]
    MakeInstance,
    #[token("instance-name")]
    InstanceName,
    #[token("instance-address")]
    InstanceAddress,
    #[token("instance-existp")]
    InstanceExistp,
    #[token("send")]
    Send,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // Wildcards and variables
    #[token("?")]
    Question,
    #[token("$?")]
    MultiWildcard,
    #[regex(r"\?[a-zA-Z_][a-zA-Z0-9_\-]*", |lex| lex.slice()[1..].to_string())]
    Variable(String),
    #[regex(r"\$\?[a-zA-Z_][a-zA-Z0-9_\-]*", |lex| lex.slice()[2..].to_string())]
    MultiVariable(String),
    #[regex(r"\?\*[a-zA-Z_][a-zA-Z0-9_\-]*\*", |lex| {
        let s = lex.slice();
        s[2..s.len()-1].to_string()
    })]
    GlobalVariable(String),

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    FloatLit(f64),
    #[regex(r#""[^"]*""#, |lex| lex.slice().trim_matches('"').to_string())]
    StringLit(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_\-]*", |lex| lex.slice().to_string(), priority = 1)]
    Symbol(String),

    // Comments
    #[regex(r";[^\n]*")]
    Comment,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// CLIPS AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClipsProgram {
    pub constructs: Vec<ClipsConstruct>,
}

/// CLIPS construct
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClipsConstruct {
    Defrule {
        name: String,
        doc: Option<String>,
        declarations: Vec<RuleDeclaration>,
        patterns: Vec<ClipsPattern>,
        actions: Vec<ClipsAction>,
    },
    Deffacts {
        name: String,
        doc: Option<String>,
        facts: Vec<ClipsFact>,
    },
    Deftemplate {
        name: String,
        doc: Option<String>,
        slots: Vec<TemplateSlot>,
    },
    Deffunction {
        name: String,
        doc: Option<String>,
        params: Vec<String>,
        wildcard_param: Option<String>,
        body: Vec<ClipsAction>,
    },
    Defglobal {
        module: Option<String>,
        globals: Vec<(String, ClipsExpr)>,
    },
    Defclass {
        name: String,
        doc: Option<String>,
        superclasses: Vec<String>,
        role: ClassRole,
        pattern_match: bool,
        slots: Vec<ClassSlot>,
    },
    Definstances {
        name: String,
        doc: Option<String>,
        instances: Vec<InstanceSpec>,
    },
    DefmessageHandler {
        class: String,
        name: String,
        handler_type: HandlerType,
        params: Vec<String>,
        wildcard_param: Option<String>,
        body: Vec<ClipsAction>,
    },
    Defgeneric {
        name: String,
        doc: Option<String>,
    },
    Defmethod {
        name: String,
        index: Option<i32>,
        params: Vec<MethodParam>,
        wildcard_param: Option<(String, Option<String>)>,
        body: Vec<ClipsAction>,
    },
    Defmodule {
        name: String,
        doc: Option<String>,
        exports: Vec<ModuleExport>,
        imports: Vec<ModuleImport>,
    },
}

/// Rule declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleDeclaration {
    Salience(i32),
    AutoFocus(bool),
}

/// Template slot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateSlot {
    pub name: String,
    pub is_multi: bool,
    pub slot_type: Option<SlotType>,
    pub default_value: Option<DefaultValue>,
    pub range: Option<(ClipsExpr, ClipsExpr)>,
    pub cardinality: Option<(i32, Option<i32>)>,
    pub allowed_values: Option<Vec<ClipsExpr>>,
}

/// Slot type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SlotType {
    Symbol,
    String,
    Number,
    Integer,
    Float,
    InstanceName,
    InstanceAddress,
    FactAddress,
    ExternalAddress,
}

/// Default value specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DefaultValue {
    None_,
    Derive,
    Static(ClipsExpr),
    Dynamic(ClipsExpr),
}

/// Class role
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ClassRole {
    Concrete,
    Abstract,
}

/// Class slot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassSlot {
    pub name: String,
    pub is_multi: bool,
    pub facets: Vec<SlotFacet>,
}

/// Slot facet
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SlotFacet {
    Type(SlotType),
    Default(DefaultValue),
    Visibility(Visibility),
    Access(AccessMode),
    Storage(StorageMode),
    Propagation(PropagationMode),
    Source(SourceMode),
    CreateAccessor(CreateAccessorMode),
    OverrideMessage(String),
}

/// Visibility
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Visibility { Public, Private }

/// Access mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AccessMode { ReadWrite, ReadOnly, InitializeOnly }

/// Storage mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum StorageMode { Local, Shared }

/// Propagation mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PropagationMode { Inherit, NoInherit }

/// Source mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SourceMode { Exclusive, Composite }

/// Create accessor mode
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum CreateAccessorMode { None_, Read, Write, ReadWrite }

/// Handler type
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum HandlerType { Primary, Around, Before, After }

/// Instance spec
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceSpec {
    pub name: String,
    pub class: String,
    pub slots: Vec<(String, ClipsExpr)>,
}

/// Method parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MethodParam {
    pub name: String,
    pub type_constraint: Option<String>,
    pub query: Option<ClipsExpr>,
}

/// Module export
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ModuleExport {
    All,
    Constructs(Vec<(String, Vec<String>)>),
}

/// Module import
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleImport {
    pub module: String,
    pub constructs: Vec<(String, Vec<String>)>,
}

/// Pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClipsPattern {
    Ordered {
        name: String,
        elements: Vec<PatternElement>,
    },
    Template {
        name: String,
        slots: Vec<(String, PatternElement)>,
    },
    Object {
        is_a: Option<String>,
        name: Option<PatternElement>,
        slots: Vec<(String, PatternElement)>,
    },
    Test(ClipsExpr),
    And(Vec<ClipsPattern>),
    Or(Vec<ClipsPattern>),
    Not(Box<ClipsPattern>),
    Exists(Box<ClipsPattern>),
    Forall {
        patterns: Vec<ClipsPattern>,
    },
    Logical(Vec<ClipsPattern>),
    Assign {
        var: String,
        pattern: Box<ClipsPattern>,
    },
}

/// Pattern element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternElement {
    Literal(ClipsExpr),
    SingleWildcard,
    MultiWildcard,
    Variable(String),
    MultiVariable(String),
    Constraint {
        connective: ConstraintConnective,
        elements: Vec<PatternElement>,
    },
    Predicate {
        negated: bool,
        expr: ClipsExpr,
    },
    Return(ClipsExpr),
}

/// Constraint connective
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ConstraintConnective { And, Or }

/// Fact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClipsFact {
    Ordered {
        name: String,
        values: Vec<ClipsExpr>,
    },
    Template {
        name: String,
        slots: Vec<(String, ClipsExpr)>,
    },
}

/// Action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClipsAction {
    Assert(ClipsFact),
    Retract(ClipsExpr),
    Modify {
        fact: ClipsExpr,
        slots: Vec<(String, ClipsExpr)>,
    },
    Duplicate {
        fact: ClipsExpr,
        slots: Vec<(String, ClipsExpr)>,
    },
    Bind {
        var: String,
        value: ClipsExpr,
    },
    If {
        condition: ClipsExpr,
        then_actions: Vec<ClipsAction>,
        else_actions: Option<Vec<ClipsAction>>,
    },
    While {
        condition: ClipsExpr,
        do_actions: Vec<ClipsAction>,
    },
    LoopForCount {
        var: String,
        range: (ClipsExpr, Option<ClipsExpr>),
        do_actions: Vec<ClipsAction>,
    },
    Foreach {
        var: String,
        list: ClipsExpr,
        do_actions: Vec<ClipsAction>,
    },
    Switch {
        expr: ClipsExpr,
        cases: Vec<(Vec<ClipsExpr>, Vec<ClipsAction>)>,
        default: Option<Vec<ClipsAction>>,
    },
    Progn(Vec<ClipsAction>),
    Return(Option<ClipsExpr>),
    Break,
    FunctionCall {
        name: String,
        args: Vec<ClipsExpr>,
    },
    Send {
        object: ClipsExpr,
        message: String,
        args: Vec<ClipsExpr>,
    },
    MakeInstance {
        name: Option<ClipsExpr>,
        class: String,
        slots: Vec<(String, ClipsExpr)>,
    },
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClipsExpr {
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    Symbol(String),
    Variable(String),
    MultiVariable(String),
    GlobalVariable(String),
    Multifield(Vec<ClipsExpr>),
    FunctionCall {
        name: String,
        args: Vec<ClipsExpr>,
    },
}

/// CLIPS parser
pub struct ClipsParser;

impl ClipsParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ClipsParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend for ClipsParser {
    fn name(&self) -> &'static str {
        "CLIPS"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["clp", "clips"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = ClipsToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, ClipsToken::Comment | ClipsToken::Newline) {
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

        let mut builder = IrBuilder::with_language("clips_module", SourceLanguage::Clips);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "(defrule hello => (printout t \"Hello World\" crlf))";
        let lexer = ClipsToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&ClipsToken::Defrule));
        assert!(tokens.contains(&ClipsToken::Arrow));
    }

    #[test]
    fn test_variables() {
        let source = "?x $?list ?*global*";
        let lexer = ClipsToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(matches!(tokens.get(0), Some(ClipsToken::Variable(_))));
        assert!(matches!(tokens.get(1), Some(ClipsToken::MultiVariable(_))));
    }

    #[test]
    fn test_parse() {
        let parser = ClipsParser::new();
        let source = SourceFile {
            name: "test.clp".to_string(),
            content: "(deffacts init (start))".to_string(),
            language: SourceLanguage::Clips,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
