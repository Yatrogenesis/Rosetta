//! # Rosetta Modula-2 Frontend
//!
//! Parser for Modula-2 (1978), Niklaus Wirth's successor to Pascal.
//!
//! ## Supported Features
//!
//! - Module system (DEFINITION MODULE, IMPLEMENTATION MODULE)
//! - IMPORT/EXPORT declarations
//! - Procedures and functions
//! - Strong typing with CARDINAL, INTEGER, REAL, etc.
//! - Pointer types and dynamic allocation
//! - RECORD, ARRAY, SET types
//! - Control structures (IF, CASE, WHILE, REPEAT, FOR, LOOP)
//! - Coroutines (PROCESS, TRANSFER, NEWPROCESS)

use logos::Logos;
use rosetta_core::{IrType, IrExpr, SourceLanguage, Result, TranspileError, SourceLocation};
use rosetta_ir::{IrModule, IrFunction, IrBuilder, IrParam, Visibility, IrTypeDef, IrGlobal};

/// Modula-2 tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum Modula2Token {
    // Module keywords
    #[token("MODULE", ignore(ascii_case))]
    Module,
    #[token("DEFINITION", ignore(ascii_case))]
    Definition,
    #[token("IMPLEMENTATION", ignore(ascii_case))]
    Implementation,
    #[token("FROM", ignore(ascii_case))]
    From,
    #[token("IMPORT", ignore(ascii_case))]
    Import,
    #[token("EXPORT", ignore(ascii_case))]
    Export,
    #[token("QUALIFIED", ignore(ascii_case))]
    Qualified,

    // Declaration keywords
    #[token("CONST", ignore(ascii_case))]
    Const,
    #[token("TYPE", ignore(ascii_case))]
    Type,
    #[token("VAR", ignore(ascii_case))]
    Var,
    #[token("PROCEDURE", ignore(ascii_case))]
    Procedure,

    // Type keywords
    #[token("ARRAY", ignore(ascii_case))]
    Array,
    #[token("RECORD", ignore(ascii_case))]
    Record,
    #[token("SET", ignore(ascii_case))]
    Set,
    #[token("POINTER", ignore(ascii_case))]
    Pointer,
    #[token("TO", ignore(ascii_case))]
    To,
    #[token("OF", ignore(ascii_case))]
    Of,

    // Built-in types
    #[token("INTEGER", ignore(ascii_case))]
    IntegerType,
    #[token("CARDINAL", ignore(ascii_case))]
    CardinalType,
    #[token("REAL", ignore(ascii_case))]
    RealType,
    #[token("LONGREAL", ignore(ascii_case))]
    LongRealType,
    #[token("BOOLEAN", ignore(ascii_case))]
    BooleanType,
    #[token("CHAR", ignore(ascii_case))]
    CharType,
    #[token("BITSET", ignore(ascii_case))]
    BitsetType,
    #[token("PROC", ignore(ascii_case))]
    ProcType,
    #[token("ADDRESS", ignore(ascii_case))]
    AddressType,
    #[token("WORD", ignore(ascii_case))]
    WordType,
    #[token("BYTE", ignore(ascii_case))]
    ByteType,

    // Control flow
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("ELSIF", ignore(ascii_case))]
    Elsif,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("CASE", ignore(ascii_case))]
    Case,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("REPEAT", ignore(ascii_case))]
    Repeat,
    #[token("UNTIL", ignore(ascii_case))]
    Until,
    #[token("FOR", ignore(ascii_case))]
    For,
    #[token("BY", ignore(ascii_case))]
    By,
    #[token("LOOP", ignore(ascii_case))]
    Loop,
    #[token("EXIT", ignore(ascii_case))]
    Exit,
    #[token("WITH", ignore(ascii_case))]
    With,
    #[token("RETURN", ignore(ascii_case))]
    Return,

    // Structure keywords
    #[token("BEGIN", ignore(ascii_case))]
    Begin,
    #[token("END", ignore(ascii_case))]
    End,

    // Boolean
    #[token("TRUE", ignore(ascii_case))]
    True,
    #[token("FALSE", ignore(ascii_case))]
    False,
    #[token("NIL", ignore(ascii_case))]
    Nil,

    // Logical operators
    #[token("AND", ignore(ascii_case))]
    And,
    #[token("OR", ignore(ascii_case))]
    Or,
    #[token("NOT", ignore(ascii_case))]
    Not,
    #[token("IN", ignore(ascii_case))]
    In,
    #[token("DIV", ignore(ascii_case))]
    Div,
    #[token("MOD", ignore(ascii_case))]
    Mod,

    // Coroutines
    #[token("PROCESS", ignore(ascii_case))]
    Process,
    #[token("TRANSFER", ignore(ascii_case))]
    Transfer,
    #[token("IOTRANSFER", ignore(ascii_case))]
    IoTransfer,
    #[token("NEWPROCESS", ignore(ascii_case))]
    NewProcess,

    // Memory
    #[token("NEW", ignore(ascii_case))]
    New,
    #[token("DISPOSE", ignore(ascii_case))]
    Dispose,
    #[token("SIZE", ignore(ascii_case))]
    Size,

    // Delimiters
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
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token("..")]
    Range,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,

    // Operators
    #[token(":=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equal,
    #[token("#")]
    NotEqual,
    #[token("<>")]
    NotEqual2,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("&")]
    Ampersand,
    #[token("~")]
    Tilde,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+([Ee][+-]?[0-9]+)?", |lex| lex.slice().parse::<f64>().ok())]
    Real(f64),

    #[regex(r"[0-9][0-9A-Fa-f]*[Hh]", |lex| {
        let s = lex.slice();
        i64::from_str_radix(&s[..s.len()-1], 16).ok()
    })]
    HexInteger(i64),

    #[regex(r"[0-7]+[Bb]", |lex| {
        let s = lex.slice();
        i64::from_str_radix(&s[..s.len()-1], 8).ok()
    })]
    OctalInteger(i64),

    #[regex(r#"'[^']*'"#, |lex| {
        let s = lex.slice();
        Some(s[1..s.len()-1].to_string())
    })]
    String(String),

    #[regex(r#""[^"]*""#, |lex| {
        let s = lex.slice();
        Some(s[1..s.len()-1].to_string())
    })]
    String2(String),

    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Comments
    #[regex(r"\(\*[^*]*\*+([^)*][^*]*\*+)*\)")]
    Comment,
}

/// Modula-2 parser
pub struct Modula2Parser {
    tokens: Vec<(Modula2Token, std::ops::Range<usize>)>,
    pos: usize,
    source: String,
}

impl Modula2Parser {
    pub fn new(source: &str) -> Self {
        let mut tokens = Vec::new();
        let lexer = Modula2Token::lexer(source);
        for (token, span) in lexer.spanned() {
            if let Ok(t) = token {
                if !matches!(t, Modula2Token::Comment) {
                    tokens.push((t, span));
                }
            }
        }
        Self {
            tokens,
            pos: 0,
            source: source.to_string(),
        }
    }

    pub fn parse(&mut self) -> Result<IrModule> {
        let mut builder = IrBuilder::with_language("modula2_module", SourceLanguage::Modula2);

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                Modula2Token::Module | Modula2Token::Definition | Modula2Token::Implementation => {
                    self.parse_module_header(&mut builder)?;
                }
                Modula2Token::Const => {
                    self.parse_const_decl(&mut builder)?;
                }
                Modula2Token::Type => {
                    self.parse_type_decl(&mut builder)?;
                }
                Modula2Token::Var => {
                    self.parse_var_decl(&mut builder)?;
                }
                Modula2Token::Procedure => {
                    let func = self.parse_procedure()?;
                    builder.add_function(func);
                }
                Modula2Token::From => {
                    self.parse_import()?;
                }
                Modula2Token::Import => {
                    self.parse_import()?;
                }
                Modula2Token::Export => {
                    self.parse_export()?;
                }
                Modula2Token::Begin => {
                    self.parse_module_body(&mut builder)?;
                }
                Modula2Token::End => {
                    self.pos += 1;
                    // Skip module name and period
                    while self.pos < self.tokens.len() {
                        if matches!(self.tokens[self.pos].0, Modula2Token::Period) {
                            self.pos += 1;
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Ok(builder.build())
    }

    fn parse_module_header(&mut self, builder: &mut IrBuilder) -> Result<()> {
        // Skip DEFINITION/IMPLEMENTATION if present
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Definition, _)) | Some((Modula2Token::Implementation, _))) {
            self.pos += 1;
        }

        // Skip MODULE keyword
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Module, _))) {
            self.pos += 1;
        }

        // Get module name
        if let Some((Modula2Token::Identifier(_name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
        }

        // Skip semicolon
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
            self.pos += 1;
        }

        Ok(())
    }

    fn parse_const_decl(&mut self, builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip CONST

        while self.pos < self.tokens.len() {
            if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
                let const_name = name.clone();
                self.pos += 1;

                // Skip = sign
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Equal, _))) {
                    self.pos += 1;
                }

                let value = self.parse_expression()?;

                // Skip semicolon
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
                    self.pos += 1;
                }

                // Continue if another constant follows
                if !matches!(self.tokens.get(self.pos), Some((Modula2Token::Identifier(_), _))) {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_type_decl(&mut self, builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip TYPE

        while self.pos < self.tokens.len() {
            if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
                let type_name = name.clone();
                self.pos += 1;

                // Skip = sign
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Equal, _))) {
                    self.pos += 1;
                }

                let ir_type = self.parse_type()?;

                // Skip semicolon
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
                    self.pos += 1;
                }

                builder.add_type(IrTypeDef::Alias {
                    name: type_name,
                    target: ir_type,
                });

                // Continue if another type follows
                if !matches!(self.tokens.get(self.pos), Some((Modula2Token::Identifier(_), _))) {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_var_decl(&mut self, builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip VAR

        while self.pos < self.tokens.len() {
            if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
                let var_name = name.clone();
                self.pos += 1;

                // Handle multiple variables with same type
                let mut var_names = vec![var_name];
                while matches!(self.tokens.get(self.pos), Some((Modula2Token::Comma, _))) {
                    self.pos += 1;
                    if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
                        var_names.push(name.clone());
                        self.pos += 1;
                    }
                }

                // Skip colon
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Colon, _))) {
                    self.pos += 1;
                }

                let var_type = self.parse_type()?;

                // Skip semicolon
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
                    self.pos += 1;
                }

                for vn in var_names {
                    builder.add_global(IrGlobal {
                        name: vn,
                        ty: var_type.clone(),
                        is_mutable: true,
                        init: None,
                        visibility: Visibility::Public,
                    });
                }

                // Continue if another variable follows
                if !matches!(self.tokens.get(self.pos), Some((Modula2Token::Identifier(_), _))) {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_procedure(&mut self) -> Result<IrFunction> {
        self.pos += 1; // Skip PROCEDURE

        let name = if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            name.clone()
        } else {
            return Err(TranspileError::ParseError {
                message: "Expected procedure name".to_string(),
                location: self.current_location(),
            });
        };

        let mut params = Vec::new();
        let mut return_type = IrType::Unit;

        // Parse parameters
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::LParen, _))) {
            self.pos += 1;
            params = self.parse_formal_params()?;
            if matches!(self.tokens.get(self.pos), Some((Modula2Token::RParen, _))) {
                self.pos += 1;
            }
        }

        // Parse return type
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Colon, _))) {
            self.pos += 1;
            return_type = self.parse_type()?;
        }

        // Skip semicolon
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
            self.pos += 1;
        }

        // Parse local declarations
        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                Modula2Token::Const => {
                    let mut dummy = IrBuilder::with_language("dummy", SourceLanguage::Modula2);
                    self.parse_const_decl(&mut dummy)?;
                }
                Modula2Token::Type => {
                    let mut dummy = IrBuilder::with_language("dummy", SourceLanguage::Modula2);
                    self.parse_type_decl(&mut dummy)?;
                }
                Modula2Token::Var => {
                    let mut dummy = IrBuilder::with_language("dummy", SourceLanguage::Modula2);
                    self.parse_var_decl(&mut dummy)?;
                }
                Modula2Token::Procedure => {
                    // Nested procedure - skip for now
                    self.skip_procedure()?;
                }
                Modula2Token::Begin => {
                    break;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        // Parse body
        let mut body = Vec::new();
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Begin, _))) {
            self.pos += 1;
            body = self.parse_statement_sequence()?;
        }

        // Skip END and procedure name
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
            if matches!(self.tokens.get(self.pos), Some((Modula2Token::Identifier(_), _))) {
                self.pos += 1;
            }
            if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
                self.pos += 1;
            }
        }

        Ok(IrFunction {
            name,
            generics: vec![],
            params,
            return_type,
            body,
            is_unsafe: false,
            visibility: Visibility::Public,
            attributes: vec![],
            span: None,
        })
    }

    fn parse_formal_params(&mut self) -> Result<Vec<IrParam>> {
        let mut params = Vec::new();

        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, Modula2Token::RParen) {
                break;
            }

            let is_var = if matches!(self.tokens.get(self.pos), Some((Modula2Token::Var, _))) {
                self.pos += 1;
                true
            } else {
                false
            };

            // Get parameter names
            let mut param_names = Vec::new();
            while let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
                param_names.push(name.clone());
                self.pos += 1;
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Comma, _))) {
                    self.pos += 1;
                } else {
                    break;
                }
            }

            // Skip colon
            if matches!(self.tokens.get(self.pos), Some((Modula2Token::Colon, _))) {
                self.pos += 1;
            }

            let param_type = self.parse_type()?;

            for pn in param_names {
                params.push(IrParam {
                    name: pn,
                    ty: param_type.clone(),
                    is_mutable: is_var,
                    by_ref: is_var,
                });
            }

            // Skip semicolon between parameter groups
            if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _))) {
                self.pos += 1;
            }
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> Result<IrType> {
        if self.pos >= self.tokens.len() {
            return Ok(IrType::Unknown);
        }

        match &self.tokens[self.pos].0 {
            Modula2Token::IntegerType => { self.pos += 1; Ok(IrType::Int(32)) }
            Modula2Token::CardinalType => { self.pos += 1; Ok(IrType::Int(32)) }
            Modula2Token::RealType => { self.pos += 1; Ok(IrType::Float(32)) }
            Modula2Token::LongRealType => { self.pos += 1; Ok(IrType::Float(64)) }
            Modula2Token::BooleanType => { self.pos += 1; Ok(IrType::Bool) }
            Modula2Token::CharType => { self.pos += 1; Ok(IrType::Char) }
            Modula2Token::ByteType => { self.pos += 1; Ok(IrType::Int(8)) }
            Modula2Token::WordType => { self.pos += 1; Ok(IrType::Int(16)) }
            Modula2Token::AddressType => { self.pos += 1; Ok(IrType::Int(64)) }
            Modula2Token::BitsetType => { self.pos += 1; Ok(IrType::Int(32)) }

            Modula2Token::Array => {
                self.pos += 1;
                // Skip index type
                while self.pos < self.tokens.len() {
                    if matches!(self.tokens[self.pos].0, Modula2Token::Of) {
                        self.pos += 1;
                        break;
                    }
                    self.pos += 1;
                }
                let elem_type = self.parse_type()?;
                Ok(IrType::Vec(Box::new(elem_type)))
            }

            Modula2Token::Pointer => {
                self.pos += 1;
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::To, _))) {
                    self.pos += 1;
                }
                let target_type = self.parse_type()?;
                Ok(IrType::Box(Box::new(target_type)))
            }

            Modula2Token::Record => {
                self.parse_record_type()
            }

            Modula2Token::Set => {
                self.pos += 1;
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::Of, _))) {
                    self.pos += 1;
                    self.parse_type()?;
                }
                Ok(IrType::Int(32))
            }

            Modula2Token::Identifier(name) => {
                let type_name = name.clone();
                self.pos += 1;
                Ok(IrType::Struct(type_name))
            }

            _ => Ok(IrType::Unknown)
        }
    }

    fn parse_record_type(&mut self) -> Result<IrType> {
        self.pos += 1; // Skip RECORD
        // Skip fields until END
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, Modula2Token::End) {
                self.pos += 1;
                break;
            }
            self.pos += 1;
        }
        Ok(IrType::Any)
    }

    fn parse_statement_sequence(&mut self) -> Result<Vec<IrExpr>> {
        let mut stmts = Vec::new();

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                Modula2Token::End | Modula2Token::Else | Modula2Token::Elsif |
                Modula2Token::Until | Modula2Token::Pipe => {
                    break;
                }
                Modula2Token::Semicolon => {
                    self.pos += 1;
                }
                _ => {
                    let stmt = self.parse_statement()?;
                    stmts.push(stmt);
                }
            }
        }

        Ok(stmts)
    }

    fn parse_statement(&mut self) -> Result<IrExpr> {
        match &self.tokens[self.pos].0 {
            Modula2Token::If => self.parse_if(),
            Modula2Token::While => self.parse_while(),
            Modula2Token::Repeat => self.parse_repeat(),
            Modula2Token::For => self.parse_for(),
            Modula2Token::Loop => self.parse_loop(),
            Modula2Token::Case => self.parse_case(),
            Modula2Token::With => self.parse_with(),
            Modula2Token::Return => self.parse_return(),
            Modula2Token::Exit => {
                self.pos += 1;
                Ok(IrExpr::Return(Box::new(IrExpr::Nil)))
            }
            Modula2Token::Identifier(_) => self.parse_assignment_or_call(),
            _ => {
                self.pos += 1;
                Ok(IrExpr::Nil)
            }
        }
    }

    fn parse_if(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip IF
        let condition = self.parse_expression()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Then, _))) {
            self.pos += 1;
        }

        let then_branch = self.parse_statement_sequence()?;

        let else_branch = if matches!(self.tokens.get(self.pos), Some((Modula2Token::Else, _))) {
            self.pos += 1;
            Some(Box::new(IrExpr::Block(self.parse_statement_sequence()?)))
        } else if matches!(self.tokens.get(self.pos), Some((Modula2Token::Elsif, _))) {
            Some(Box::new(self.parse_if()?))
        } else {
            None
        };

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
        }

        Ok(IrExpr::If {
            condition: Box::new(condition),
            then_branch: Box::new(IrExpr::Block(then_branch)),
            else_branch,
        })
    }

    fn parse_while(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip WHILE
        let condition = self.parse_expression()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Do, _))) {
            self.pos += 1;
        }

        let body = self.parse_statement_sequence()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
        }

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("while_loop".to_string())),
            args: vec![condition, IrExpr::Block(body)],
        })
    }

    fn parse_repeat(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip REPEAT
        let body = self.parse_statement_sequence()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Until, _))) {
            self.pos += 1;
        }

        let condition = self.parse_expression()?;

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("repeat_until".to_string())),
            args: vec![IrExpr::Block(body), condition],
        })
    }

    fn parse_for(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip FOR
        let var = if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            name.clone()
        } else {
            "i".to_string()
        };

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Assign, _))) {
            self.pos += 1;
        }

        let start = self.parse_expression()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::To, _))) {
            self.pos += 1;
        }

        let end = self.parse_expression()?;

        let step = if matches!(self.tokens.get(self.pos), Some((Modula2Token::By, _))) {
            self.pos += 1;
            Some(self.parse_expression()?)
        } else {
            None
        };

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Do, _))) {
            self.pos += 1;
        }

        let body = self.parse_statement_sequence()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
        }

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("for_loop".to_string())),
            args: vec![
                IrExpr::Identifier(var),
                start,
                end,
                step.unwrap_or(IrExpr::Int(1)),
                IrExpr::Block(body),
            ],
        })
    }

    fn parse_loop(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip LOOP
        let body = self.parse_statement_sequence()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
        }

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("infinite_loop".to_string())),
            args: vec![IrExpr::Block(body)],
        })
    }

    fn parse_case(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip CASE
        let selector = self.parse_expression()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Of, _))) {
            self.pos += 1;
        }

        // Skip case labels until END
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, Modula2Token::End) {
                self.pos += 1;
                break;
            }
            self.pos += 1;
        }

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("case_of".to_string())),
            args: vec![selector],
        })
    }

    fn parse_with(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip WITH
        let record = self.parse_expression()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Do, _))) {
            self.pos += 1;
        }

        let body = self.parse_statement_sequence()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::End, _))) {
            self.pos += 1;
        }

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("with_record".to_string())),
            args: vec![record, IrExpr::Block(body)],
        })
    }

    fn parse_return(&mut self) -> Result<IrExpr> {
        self.pos += 1; // Skip RETURN
        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Semicolon, _)) | Some((Modula2Token::End, _))) {
            Ok(IrExpr::Return(Box::new(IrExpr::Nil)))
        } else {
            let value = self.parse_expression()?;
            Ok(IrExpr::Return(Box::new(value)))
        }
    }

    fn parse_assignment_or_call(&mut self) -> Result<IrExpr> {
        let target = self.parse_designator()?;

        if matches!(self.tokens.get(self.pos), Some((Modula2Token::Assign, _))) {
            self.pos += 1;
            let value = self.parse_expression()?;
            if let IrExpr::Identifier(name) = target {
                Ok(IrExpr::Assign {
                    target: name,
                    value: Box::new(value),
                })
            } else {
                Ok(IrExpr::Nil)
            }
        } else if matches!(self.tokens.get(self.pos), Some((Modula2Token::LParen, _))) {
            // Procedure call with arguments
            self.pos += 1;
            let mut args = Vec::new();
            while self.pos < self.tokens.len() {
                if matches!(self.tokens[self.pos].0, Modula2Token::RParen) {
                    self.pos += 1;
                    break;
                }
                if matches!(self.tokens[self.pos].0, Modula2Token::Comma) {
                    self.pos += 1;
                    continue;
                }
                args.push(self.parse_expression()?);
            }
            Ok(IrExpr::Call {
                func: Box::new(target),
                args,
            })
        } else {
            // Procedure call without arguments
            Ok(IrExpr::Call {
                func: Box::new(target),
                args: vec![],
            })
        }
    }

    fn parse_designator(&mut self) -> Result<IrExpr> {
        let mut expr = if let Some((Modula2Token::Identifier(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            IrExpr::Identifier(name.clone())
        } else {
            return Ok(IrExpr::Nil);
        };

        // Handle field access and array indexing
        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                Modula2Token::Period => {
                    self.pos += 1;
                    if let Some((Modula2Token::Identifier(field), _)) = self.tokens.get(self.pos) {
                        let field_name = field.clone();
                        self.pos += 1;
                        expr = IrExpr::FieldAccess {
                            object: Box::new(expr),
                            field: field_name,
                        };
                    }
                }
                Modula2Token::LBracket => {
                    self.pos += 1;
                    let index = self.parse_expression()?;
                    if matches!(self.tokens.get(self.pos), Some((Modula2Token::RBracket, _))) {
                        self.pos += 1;
                    }
                    expr = IrExpr::Index {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Modula2Token::Caret => {
                    self.pos += 1;
                    expr = IrExpr::UnaryOp {
                        op: "^".to_string(),
                        operand: Box::new(expr),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_expression(&mut self) -> Result<IrExpr> {
        let left = self.parse_simple_expr()?;

        if self.pos < self.tokens.len() {
            let op = match &self.tokens[self.pos].0 {
                Modula2Token::Equal => Some("="),
                Modula2Token::NotEqual | Modula2Token::NotEqual2 => Some("<>"),
                Modula2Token::LessThan => Some("<"),
                Modula2Token::GreaterThan => Some(">"),
                Modula2Token::LessEqual => Some("<="),
                Modula2Token::GreaterEqual => Some(">="),
                Modula2Token::In => Some("in"),
                _ => None,
            };

            if let Some(op_str) = op {
                self.pos += 1;
                let right = self.parse_simple_expr()?;
                return Ok(IrExpr::BinaryOp {
                    op: op_str.to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
        }

        Ok(left)
    }

    fn parse_simple_expr(&mut self) -> Result<IrExpr> {
        let sign = if matches!(self.tokens.get(self.pos), Some((Modula2Token::Plus, _))) {
            self.pos += 1;
            None
        } else if matches!(self.tokens.get(self.pos), Some((Modula2Token::Minus, _))) {
            self.pos += 1;
            Some("-")
        } else {
            None
        };

        let mut left = self.parse_term()?;

        if let Some(s) = sign {
            left = IrExpr::UnaryOp {
                op: s.to_string(),
                operand: Box::new(left),
            };
        }

        while self.pos < self.tokens.len() {
            let op = match &self.tokens[self.pos].0 {
                Modula2Token::Plus => "+",
                Modula2Token::Minus => "-",
                Modula2Token::Or => "or",
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_term()?;
            left = IrExpr::BinaryOp {
                op: op.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<IrExpr> {
        let mut left = self.parse_factor()?;

        while self.pos < self.tokens.len() {
            let op = match &self.tokens[self.pos].0 {
                Modula2Token::Star => "*",
                Modula2Token::Slash => "/",
                Modula2Token::Div => "div",
                Modula2Token::Mod => "mod",
                Modula2Token::And | Modula2Token::Ampersand => "and",
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_factor()?;
            left = IrExpr::BinaryOp {
                op: op.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<IrExpr> {
        if self.pos >= self.tokens.len() {
            return Ok(IrExpr::Nil);
        }

        match &self.tokens[self.pos].0 {
            Modula2Token::Integer(n) => {
                let val = *n;
                self.pos += 1;
                Ok(IrExpr::Int(val))
            }
            Modula2Token::HexInteger(n) | Modula2Token::OctalInteger(n) => {
                let val = *n;
                self.pos += 1;
                Ok(IrExpr::Int(val))
            }
            Modula2Token::Real(f) => {
                let val = *f;
                self.pos += 1;
                Ok(IrExpr::Float(val))
            }
            Modula2Token::String(s) | Modula2Token::String2(s) => {
                let val = s.clone();
                self.pos += 1;
                Ok(IrExpr::String(val))
            }
            Modula2Token::True => {
                self.pos += 1;
                Ok(IrExpr::Bool(true))
            }
            Modula2Token::False => {
                self.pos += 1;
                Ok(IrExpr::Bool(false))
            }
            Modula2Token::Nil => {
                self.pos += 1;
                Ok(IrExpr::Nil)
            }
            Modula2Token::Not | Modula2Token::Tilde => {
                self.pos += 1;
                let operand = self.parse_factor()?;
                Ok(IrExpr::UnaryOp {
                    op: "not".to_string(),
                    operand: Box::new(operand),
                })
            }
            Modula2Token::LParen => {
                self.pos += 1;
                let expr = self.parse_expression()?;
                if matches!(self.tokens.get(self.pos), Some((Modula2Token::RParen, _))) {
                    self.pos += 1;
                }
                Ok(expr)
            }
            Modula2Token::LBrace => {
                // Set literal
                self.pos += 1;
                let mut elements = Vec::new();
                while self.pos < self.tokens.len() {
                    if matches!(self.tokens[self.pos].0, Modula2Token::RBrace) {
                        self.pos += 1;
                        break;
                    }
                    if matches!(self.tokens[self.pos].0, Modula2Token::Comma) {
                        self.pos += 1;
                        continue;
                    }
                    elements.push(self.parse_expression()?);
                }
                Ok(IrExpr::List(elements))
            }
            Modula2Token::Identifier(_) => {
                self.parse_designator()
            }
            _ => {
                self.pos += 1;
                Ok(IrExpr::Nil)
            }
        }
    }

    fn parse_import(&mut self) -> Result<()> {
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, Modula2Token::Semicolon) {
                self.pos += 1;
                break;
            }
            self.pos += 1;
        }
        Ok(())
    }

    fn parse_export(&mut self) -> Result<()> {
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, Modula2Token::Semicolon) {
                self.pos += 1;
                break;
            }
            self.pos += 1;
        }
        Ok(())
    }

    fn parse_module_body(&mut self, _builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip BEGIN
        let _stmts = self.parse_statement_sequence()?;
        Ok(())
    }

    fn skip_procedure(&mut self) -> Result<()> {
        let mut depth = 1;
        self.pos += 1; // Skip PROCEDURE
        while self.pos < self.tokens.len() && depth > 0 {
            match &self.tokens[self.pos].0 {
                Modula2Token::Procedure | Modula2Token::Begin => depth += 1,
                Modula2Token::End => {
                    depth -= 1;
                    if depth == 0 {
                        self.pos += 1;
                        // Skip name and semicolon
                        while self.pos < self.tokens.len() {
                            if matches!(self.tokens[self.pos].0, Modula2Token::Semicolon) {
                                self.pos += 1;
                                break;
                            }
                            self.pos += 1;
                        }
                        return Ok(());
                    }
                }
                _ => {}
            }
            self.pos += 1;
        }
        Ok(())
    }

    fn current_location(&self) -> SourceLocation {
        if let Some((_, span)) = self.tokens.get(self.pos) {
            let line = self.source[..span.start].matches('\n').count() + 1;
            SourceLocation { line, column: 1, offset: span.start }
        } else {
            SourceLocation { line: 1, column: 1, offset: 0 }
        }
    }
}

/// Parse Modula-2 source code
pub fn parse(source: &str) -> Result<IrModule> {
    Modula2Parser::new(source).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "MODULE Test; END Test.";
        let mut lexer = Modula2Token::lexer(source);
        assert!(matches!(lexer.next(), Some(Ok(Modula2Token::Module))));
        assert!(matches!(lexer.next(), Some(Ok(Modula2Token::Identifier(_)))));
        assert!(matches!(lexer.next(), Some(Ok(Modula2Token::Semicolon))));
        assert!(matches!(lexer.next(), Some(Ok(Modula2Token::End))));
    }

    #[test]
    fn test_simple_module() {
        let source = "MODULE Test; END Test.";
        let module = parse(source).unwrap();
        assert_eq!(module.source_lang, SourceLanguage::Modula2);
    }

    #[test]
    fn test_procedure() {
        let source = r#"
            MODULE Test;
            PROCEDURE Square(x: INTEGER): INTEGER;
            BEGIN
                RETURN x * x;
            END Square;
            END Test.
        "#;
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "Square");
    }

    #[test]
    fn test_types() {
        let source = r#"
            MODULE Test;
            VAR
                i: INTEGER;
                r: REAL;
                b: BOOLEAN;
            END Test.
        "#;
        let module = parse(source).unwrap();
        assert_eq!(module.globals.len(), 3);
    }
}
