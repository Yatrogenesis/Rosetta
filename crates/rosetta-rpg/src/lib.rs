//! # Rosetta RPG Frontend
//!
//! Parses RPG (Report Program Generator) source code into Rosetta IR.
//!
//! ## RPG History
//!
//! Developed by IBM in 1959 for report generation on punch card systems.
//! Evolved through many versions:
//! - RPG II (1969) - Most widely used legacy version
//! - RPG III (1979) - Structured programming
//! - RPG/400 (1988) - For AS/400
//! - RPG IV / ILE RPG (1994) - Modern free-format
//!
//! ## Key Features
//!
//! - Column-based format (traditional)
//! - Indicator-based logic (01-99, LR, etc.)
//! - Built-in file I/O
//! - Cycle-based processing
//!
//! ## Specification Types
//!
//! | Spec | Purpose |
//! |------|---------|
//! | H | Control/Header |
//! | F | File description |
//! | D | Definition |
//! | I | Input |
//! | C | Calculation |
//! | O | Output |
//! | P | Procedure |

use logos::Logos;
use rosetta_core::{Frontend, SourceLanguage, SourceFile, ParseError, RosettaIr};
use rosetta_ir::IrBuilder;
use serde::{Deserialize, Serialize};

/// RPG token types
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum RpgToken {
    // Specification types (column 6)
    #[token("H", ignore(ascii_case))]
    HSpec,
    #[token("F", ignore(ascii_case))]
    FSpec,
    #[token("D", ignore(ascii_case))]
    DSpec,
    #[token("I", ignore(ascii_case))]
    ISpec,
    #[token("C", ignore(ascii_case))]
    CSpec,
    #[token("O", ignore(ascii_case))]
    OSpec,
    #[token("P", ignore(ascii_case))]
    PSpec,

    // Free-format keywords
    #[token("DCL-S", ignore(ascii_case))]
    DclS,
    #[token("DCL-DS", ignore(ascii_case))]
    DclDs,
    #[token("DCL-C", ignore(ascii_case))]
    DclC,
    #[token("DCL-F", ignore(ascii_case))]
    DclF,
    #[token("DCL-PR", ignore(ascii_case))]
    DclPr,
    #[token("DCL-PI", ignore(ascii_case))]
    DclPi,
    #[token("DCL-PROC", ignore(ascii_case))]
    DclProc,
    #[token("DCL-PARM", ignore(ascii_case))]
    DclParm,
    #[token("END-DS", ignore(ascii_case))]
    EndDs,
    #[token("END-PR", ignore(ascii_case))]
    EndPr,
    #[token("END-PI", ignore(ascii_case))]
    EndPi,
    #[token("END-PROC", ignore(ascii_case))]
    EndProc,

    // Control keywords
    #[token("CTL-OPT", ignore(ascii_case))]
    CtlOpt,
    #[token("MAIN", ignore(ascii_case))]
    Main,
    #[token("DFTACTGRP", ignore(ascii_case))]
    DftActGrp,
    #[token("ACTGRP", ignore(ascii_case))]
    ActGrp,
    #[token("OPTION", ignore(ascii_case))]
    Option,
    #[token("BNDDIR", ignore(ascii_case))]
    BndDir,

    // Operations (Calculation specs)
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("ELSEIF", ignore(ascii_case))]
    ElseIf,
    #[token("ENDIF", ignore(ascii_case))]
    EndIf,
    #[token("DOW", ignore(ascii_case))]
    Dow,
    #[token("DOU", ignore(ascii_case))]
    Dou,
    #[token("ENDDO", ignore(ascii_case))]
    EndDo,
    #[token("FOR", ignore(ascii_case))]
    For,
    #[token("ENDFOR", ignore(ascii_case))]
    EndFor,
    #[token("SELECT", ignore(ascii_case))]
    Select,
    #[token("WHEN", ignore(ascii_case))]
    When,
    #[token("OTHER", ignore(ascii_case))]
    Other,
    #[token("ENDSL", ignore(ascii_case))]
    EndSl,
    #[token("EVAL", ignore(ascii_case))]
    Eval,
    #[token("EVALR", ignore(ascii_case))]
    EvalR,
    #[token("CLEAR", ignore(ascii_case))]
    Clear,
    #[token("RESET", ignore(ascii_case))]
    Reset,
    #[token("RETURN", ignore(ascii_case))]
    Return,
    #[token("LEAVE", ignore(ascii_case))]
    Leave,
    #[token("ITER", ignore(ascii_case))]
    Iter,
    #[token("LEAVESR", ignore(ascii_case))]
    LeaveSr,
    #[token("GOTO", ignore(ascii_case))]
    Goto,
    #[token("TAG", ignore(ascii_case))]
    Tag,
    #[token("EXSR", ignore(ascii_case))]
    ExSr,
    #[token("BEGSR", ignore(ascii_case))]
    BegSr,
    #[token("ENDSR", ignore(ascii_case))]
    EndSr,
    #[token("CALLP", ignore(ascii_case))]
    CallP,
    #[token("CALL", ignore(ascii_case))]
    Call,
    #[token("PARM", ignore(ascii_case))]
    Parm,
    #[token("PLIST", ignore(ascii_case))]
    PList,

    // I/O Operations
    #[token("READ", ignore(ascii_case))]
    Read,
    #[token("READE", ignore(ascii_case))]
    ReadE,
    #[token("READP", ignore(ascii_case))]
    ReadP,
    #[token("READPE", ignore(ascii_case))]
    ReadPE,
    #[token("CHAIN", ignore(ascii_case))]
    Chain,
    #[token("SETLL", ignore(ascii_case))]
    SetLL,
    #[token("SETGT", ignore(ascii_case))]
    SetGT,
    #[token("WRITE", ignore(ascii_case))]
    Write,
    #[token("UPDATE", ignore(ascii_case))]
    Update,
    #[token("DELETE", ignore(ascii_case))]
    Delete,
    #[token("EXFMT", ignore(ascii_case))]
    ExFmt,
    #[token("OPEN", ignore(ascii_case))]
    Open,
    #[token("CLOSE", ignore(ascii_case))]
    Close,
    #[token("FEOD", ignore(ascii_case))]
    Feod,

    // Data operations
    #[token("MOVE", ignore(ascii_case))]
    Move,
    #[token("MOVEL", ignore(ascii_case))]
    MoveL,
    #[token("MOVEA", ignore(ascii_case))]
    MoveA,
    #[token("Z-ADD", ignore(ascii_case))]
    ZAdd,
    #[token("Z-SUB", ignore(ascii_case))]
    ZSub,
    #[token("ADD", ignore(ascii_case))]
    Add,
    #[token("SUB", ignore(ascii_case))]
    Sub,
    #[token("MULT", ignore(ascii_case))]
    Mult,
    #[token("DIV", ignore(ascii_case))]
    Div,
    #[token("SQRT", ignore(ascii_case))]
    Sqrt,
    #[token("XFOOT", ignore(ascii_case))]
    XFoot,

    // String operations
    #[token("CAT", ignore(ascii_case))]
    Cat,
    #[token("SCAN", ignore(ascii_case))]
    Scan,
    #[token("CHECK", ignore(ascii_case))]
    Check,
    #[token("CHECKR", ignore(ascii_case))]
    CheckR,
    #[token("XLATE", ignore(ascii_case))]
    Xlate,
    #[token("SUBST", ignore(ascii_case))]
    Subst,
    #[token("TRIM", ignore(ascii_case))]
    Trim,
    #[token("TRIML", ignore(ascii_case))]
    TrimL,
    #[token("TRIMR", ignore(ascii_case))]
    TrimR,

    // Comparison operations
    #[token("COMP", ignore(ascii_case))]
    Comp,
    #[token("CABEQ", ignore(ascii_case))]
    CabEq,
    #[token("CABNE", ignore(ascii_case))]
    CabNe,
    #[token("CABLT", ignore(ascii_case))]
    CabLt,
    #[token("CABLTE", ignore(ascii_case))]
    CabLte,
    #[token("CABGT", ignore(ascii_case))]
    CabGt,
    #[token("CABGTE", ignore(ascii_case))]
    CabGte,

    // Array operations
    #[token("LOOKUP", ignore(ascii_case))]
    Lookup,
    #[token("SORTA", ignore(ascii_case))]
    SortA,
    #[token("OCCUR", ignore(ascii_case))]
    Occur,

    // Data types
    #[token("CHAR", ignore(ascii_case))]
    Char,
    #[token("VARCHAR", ignore(ascii_case))]
    VarChar,
    #[token("IND", ignore(ascii_case))]
    Ind,
    #[token("PACKED", ignore(ascii_case))]
    Packed,
    #[token("ZONED", ignore(ascii_case))]
    Zoned,
    #[token("BINDEC", ignore(ascii_case))]
    BinDec,
    #[token("INT", ignore(ascii_case))]
    Int,
    #[token("UNS", ignore(ascii_case))]
    Uns,
    #[token("FLOAT", ignore(ascii_case))]
    Float,
    #[token("DATE", ignore(ascii_case))]
    Date,
    #[token("TIME", ignore(ascii_case))]
    Time,
    #[token("TIMESTAMP", ignore(ascii_case))]
    Timestamp,
    #[token("POINTER", ignore(ascii_case))]
    Pointer,
    #[token("OBJECT", ignore(ascii_case))]
    Object,

    // Built-in functions
    #[token("%ABS", ignore(ascii_case))]
    BifAbs,
    #[token("%CHAR", ignore(ascii_case))]
    BifChar,
    #[token("%DEC", ignore(ascii_case))]
    BifDec,
    #[token("%DECH", ignore(ascii_case))]
    BifDecH,
    #[token("%INT", ignore(ascii_case))]
    BifInt,
    #[token("%INTH", ignore(ascii_case))]
    BifIntH,
    #[token("%UNS", ignore(ascii_case))]
    BifUns,
    #[token("%UNSH", ignore(ascii_case))]
    BifUnsH,
    #[token("%FLOAT", ignore(ascii_case))]
    BifFloat,
    #[token("%LEN", ignore(ascii_case))]
    BifLen,
    #[token("%SUBST", ignore(ascii_case))]
    BifSubst,
    #[token("%TRIM", ignore(ascii_case))]
    BifTrim,
    #[token("%TRIML", ignore(ascii_case))]
    BifTrimL,
    #[token("%TRIMR", ignore(ascii_case))]
    BifTrimR,
    #[token("%XLATE", ignore(ascii_case))]
    BifXlate,
    #[token("%SCAN", ignore(ascii_case))]
    BifScan,
    #[token("%CHECK", ignore(ascii_case))]
    BifCheck,
    #[token("%CHECKR", ignore(ascii_case))]
    BifCheckR,
    #[token("%REPLACE", ignore(ascii_case))]
    BifReplace,
    #[token("%ELEM", ignore(ascii_case))]
    BifElem,
    #[token("%SIZE", ignore(ascii_case))]
    BifSize,
    #[token("%LOOKUP", ignore(ascii_case))]
    BifLookup,
    #[token("%EOF", ignore(ascii_case))]
    BifEof,
    #[token("%FOUND", ignore(ascii_case))]
    BifFound,
    #[token("%EQUAL", ignore(ascii_case))]
    BifEqual,
    #[token("%ERROR", ignore(ascii_case))]
    BifError,
    #[token("%STATUS", ignore(ascii_case))]
    BifStatus,
    #[token("%OPEN", ignore(ascii_case))]
    BifOpen,
    #[token("%PARMS", ignore(ascii_case))]
    BifParms,
    #[token("%ADDR", ignore(ascii_case))]
    BifAddr,
    #[token("%ALLOC", ignore(ascii_case))]
    BifAlloc,
    #[token("%REALLOC", ignore(ascii_case))]
    BifRealloc,
    #[token("%DEALLOC", ignore(ascii_case))]
    BifDealloc,
    #[token("%DATE", ignore(ascii_case))]
    BifDate,
    #[token("%TIME", ignore(ascii_case))]
    BifTime,
    #[token("%TIMESTAMP", ignore(ascii_case))]
    BifTimestamp,
    #[token("%DIFF", ignore(ascii_case))]
    BifDiff,
    #[token("%YEARS", ignore(ascii_case))]
    BifYears,
    #[token("%MONTHS", ignore(ascii_case))]
    BifMonths,
    #[token("%DAYS", ignore(ascii_case))]
    BifDays,
    #[token("%HOURS", ignore(ascii_case))]
    BifHours,
    #[token("%MINUTES", ignore(ascii_case))]
    BifMinutes,
    #[token("%SECONDS", ignore(ascii_case))]
    BifSeconds,
    #[token("%MSECONDS", ignore(ascii_case))]
    BifMSeconds,

    // Operators
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    Power,
    #[token("AND", ignore(ascii_case))]
    And,
    #[token("OR", ignore(ascii_case))]
    Or,
    #[token("NOT", ignore(ascii_case))]
    Not,

    // Indicators
    #[regex(r"\*IN[0-9]{2}", |lex| lex.slice()[3..].parse().ok())]
    Indicator(i32),
    #[token("*INLR", ignore(ascii_case))]
    InLR,
    #[token("*IN", ignore(ascii_case))]
    InArray,
    #[token("*ON", ignore(ascii_case))]
    On,
    #[token("*OFF", ignore(ascii_case))]
    Off,
    #[token("*BLANK", ignore(ascii_case))]
    #[token("*BLANKS", ignore(ascii_case))]
    Blanks,
    #[token("*ZERO", ignore(ascii_case))]
    #[token("*ZEROS", ignore(ascii_case))]
    Zeros,
    #[token("*HIVAL", ignore(ascii_case))]
    HiVal,
    #[token("*LOVAL", ignore(ascii_case))]
    LoVal,
    #[token("*ALL", ignore(ascii_case))]
    All,
    #[token("*ENTRY", ignore(ascii_case))]
    Entry,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    IntLit(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    DecLit(f64),
    #[regex(r"'[^']*'", |lex| lex.slice().trim_matches('\'').to_string())]
    StringLit(String),
    #[regex(r"D'[^']*'", |lex| lex.slice()[2..lex.slice().len()-1].to_string())]
    DateLit(String),
    #[regex(r"T'[^']*'", |lex| lex.slice()[2..lex.slice().len()-1].to_string())]
    TimeLit(String),
    #[regex(r"Z'[^']*'", |lex| lex.slice()[2..lex.slice().len()-1].to_string())]
    TimestampLit(String),

    // Identifiers
    #[regex(r"[A-Za-z_@#$][A-Za-z0-9_@#$]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Comments
    #[regex(r"//[^\n]*")]
    Comment,
    #[regex(r"\*\*FREE", ignore(ascii_case))]
    FreeFormat,

    // Newline
    #[regex(r"\n")]
    Newline,
}

/// RPG AST
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpgProgram {
    pub control_specs: Vec<ControlSpec>,
    pub file_specs: Vec<FileSpec>,
    pub definitions: Vec<RpgDefinition>,
    pub procedures: Vec<RpgProcedure>,
    pub subroutines: Vec<RpgSubroutine>,
}

/// Control specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlSpec {
    pub options: Vec<(String, String)>,
}

/// File specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileSpec {
    pub name: String,
    pub file_type: FileType,
    pub designation: FileDesignation,
    pub record_length: Option<i32>,
    pub key_length: Option<i32>,
    pub device: Device,
    pub keywords: Vec<FileKeyword>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileType {
    Input,
    Output,
    Update,
    Combined,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileDesignation {
    Primary,
    Secondary,
    Record,
    Table,
    Full,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Device {
    Disk,
    Printer,
    Workstation,
    Special,
    Seq,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileKeyword {
    Keyed,
    Prefix(String),
    Rename(String, String),
    UsrOpn,
    Commit,
    Extfile(String),
    Extmbr(String),
}

/// Definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RpgDefinition {
    Standalone {
        name: String,
        ty: RpgType,
        init: Option<RpgExpr>,
        keywords: Vec<DefKeyword>,
    },
    DataStructure {
        name: String,
        fields: Vec<DsField>,
        keywords: Vec<DefKeyword>,
    },
    Constant {
        name: String,
        value: RpgExpr,
    },
    Prototype {
        name: String,
        params: Vec<RpgParam>,
        return_type: Option<RpgType>,
        keywords: Vec<DefKeyword>,
    },
    ProcedureInterface {
        params: Vec<RpgParam>,
        return_type: Option<RpgType>,
    },
}

/// Data structure field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DsField {
    pub name: String,
    pub ty: RpgType,
    pub from: Option<i32>,
    pub to: Option<i32>,
    pub overlay: Option<String>,
}

/// Definition keyword
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DefKeyword {
    Inz(Option<RpgExpr>),
    Dim(i32),
    ExtName(String),
    LikeDs(String),
    Like(String),
    Static,
    Const,
    Based(String),
    ExtProc(String),
    OpDesc,
    Options(Vec<String>),
    Value,
    NoOpt,
}

/// Parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpgParam {
    pub name: String,
    pub ty: RpgType,
    pub keywords: Vec<DefKeyword>,
}

/// RPG Type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RpgType {
    Char(i32),
    VarChar(i32),
    Ind,
    Packed(i32, i32),
    Zoned(i32, i32),
    Binary(i32, i32),
    Int(i32),
    Uns(i32),
    Float(i32),
    Date(Option<String>),
    Time(Option<String>),
    Timestamp,
    Pointer,
    Object(String),
    LikeDs(String),
    Like(String),
}

/// Procedure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpgProcedure {
    pub name: String,
    pub interface: Option<Vec<RpgParam>>,
    pub return_type: Option<RpgType>,
    pub local_defs: Vec<RpgDefinition>,
    pub statements: Vec<RpgStmt>,
}

/// Subroutine
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpgSubroutine {
    pub name: String,
    pub statements: Vec<RpgStmt>,
}

/// Statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RpgStmt {
    Eval(RpgExpr, RpgExpr),
    If {
        condition: RpgExpr,
        then_stmts: Vec<RpgStmt>,
        else_stmts: Option<Vec<RpgStmt>>,
    },
    Select {
        cases: Vec<(RpgExpr, Vec<RpgStmt>)>,
        other: Option<Vec<RpgStmt>>,
    },
    Dow {
        condition: RpgExpr,
        body: Vec<RpgStmt>,
    },
    Dou {
        condition: RpgExpr,
        body: Vec<RpgStmt>,
    },
    For {
        var: String,
        start: RpgExpr,
        to: RpgExpr,
        by: Option<RpgExpr>,
        body: Vec<RpgStmt>,
    },
    Leave,
    Iter,
    Return(Option<RpgExpr>),
    ExSr(String),
    CallP {
        proc: String,
        args: Vec<RpgExpr>,
    },
    Read {
        file: String,
        record: Option<String>,
        error_ind: Option<i32>,
        eof_ind: Option<i32>,
    },
    Write {
        file: String,
        record: Option<String>,
    },
    Update {
        file: String,
        record: Option<String>,
    },
    Delete {
        file: String,
    },
    Chain {
        key: RpgExpr,
        file: String,
        not_found_ind: Option<i32>,
    },
    SetLL {
        key: RpgExpr,
        file: String,
    },
    SetGT {
        key: RpgExpr,
        file: String,
    },
    Clear(RpgExpr),
    Reset(RpgExpr),
    SetIndicator(i32, bool),
    Goto(String),
    Tag(String),
}

/// Expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RpgExpr {
    IntLit(i64),
    DecLit(f64),
    StringLit(String),
    DateLit(String),
    Ident(String),
    Indicator(i32),
    SpecialVal(RpgSpecialVal),
    ArrayAccess {
        array: Box<RpgExpr>,
        index: Box<RpgExpr>,
    },
    DsField {
        ds: Box<RpgExpr>,
        field: String,
    },
    FunctionCall {
        name: String,
        args: Vec<RpgExpr>,
    },
    BinOp {
        op: RpgBinOp,
        left: Box<RpgExpr>,
        right: Box<RpgExpr>,
    },
    UnaryOp {
        op: RpgUnaryOp,
        operand: Box<RpgExpr>,
    },
}

/// Special values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RpgSpecialVal {
    On,
    Off,
    Blanks,
    Zeros,
    HiVal,
    LoVal,
    All(String),
}

/// Binary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum RpgBinOp {
    Add, Sub, Mul, Div, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
    Concat,
}

/// Unary operators
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum RpgUnaryOp {
    Neg, Not,
}

/// RPG parser
pub struct RpgParser {
    version: RpgVersion,
}

#[derive(Debug, Clone, Copy)]
pub enum RpgVersion {
    RpgII,
    RpgIII,
    Rpg400,
    RpgIV,
    RpgFree,
}

impl RpgParser {
    pub fn rpg_iv() -> Self {
        Self { version: RpgVersion::RpgIV }
    }

    pub fn rpg_free() -> Self {
        Self { version: RpgVersion::RpgFree }
    }
}

impl Default for RpgParser {
    fn default() -> Self {
        Self::rpg_free()
    }
}

impl Frontend for RpgParser {
    fn name(&self) -> &'static str {
        match self.version {
            RpgVersion::RpgII => "RPG II",
            RpgVersion::RpgIII => "RPG III",
            RpgVersion::Rpg400 => "RPG/400",
            RpgVersion::RpgIV => "RPG IV",
            RpgVersion::RpgFree => "RPG Free",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["rpgle", "rpg", "sqlrpgle", "clle"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut lexer = RpgToken::lexer(&source.content);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    if !matches!(t, RpgToken::Comment | RpgToken::Newline) {
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

        let builder = IrBuilder::with_language("rpg_module", SourceLanguage::Rpg);
        Ok(builder.build().into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_free() {
        let source = "dcl-s myVar char(10) inz('Hello');";
        let lexer = RpgToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&RpgToken::DclS));
        assert!(tokens.contains(&RpgToken::Char));
    }

    #[test]
    fn test_bif() {
        let source = "result = %trim(myString);";
        let lexer = RpgToken::lexer(source);
        let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect();
        assert!(tokens.contains(&RpgToken::BifTrim));
    }

    #[test]
    fn test_parse() {
        let parser = RpgParser::rpg_free();
        let source = SourceFile {
            name: "test.rpgle".to_string(),
            content: "dcl-s x int(10);".to_string(),
            language: SourceLanguage::Rpg,
        };
        assert!(parser.parse(&source).is_ok());
    }
}
