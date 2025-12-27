//! # Rosetta Smalltalk Frontend
//!
//! Parser for Smalltalk-80, the original pure object-oriented language.
//!
//! ## Supported Features
//!
//! - Class definitions (Object subclass: #ClassName)
//! - Instance variables
//! - Method definitions (unary, binary, keyword messages)
//! - Blocks (closures) [ :x | x + 1 ]
//! - Message sending (unary, binary, keyword)
//! - Control structures (ifTrue:, whileTrue:, etc.)
//! - Literals (numbers, strings, symbols, arrays)
//! - Comments "in double quotes"

use logos::Logos;
use rosetta_core::{IrType, IrExpr, SourceLanguage, Result, TranspileError, SourceLocation};
use rosetta_ir::{IrModule, IrFunction, IrBuilder, IrParam, Visibility, IrTypeDef};

/// Smalltalk tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum SmalltalkToken {
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
    #[token(".")]
    Period,
    #[token(";")]
    Semicolon,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,

    // Assignment
    #[token(":=")]
    Assign,

    // Binary operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("//")]
    DoubleSlash,
    #[token("\\\\")]
    Modulo,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("=")]
    Equal,
    #[token("~=")]
    NotEqual,
    #[token("==")]
    IdentityEqual,
    #[token("~~")]
    IdentityNotEqual,
    #[token(",")]
    Comma,
    #[token("@")]
    At,
    #[token("&")]
    Ampersand,

    // Keywords
    #[token("self")]
    Self_,
    #[token("super")]
    Super,
    #[token("nil")]
    Nil,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("thisContext")]
    ThisContext,

    // Class definition keywords
    #[token("subclass:")]
    Subclass,
    #[token("instanceVariableNames:")]
    InstanceVariableNames,
    #[token("classVariableNames:")]
    ClassVariableNames,
    #[token("poolDictionaries:")]
    PoolDictionaries,
    #[token("category:")]
    Category,

    // Block argument
    #[token(":")]
    Colon,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r"-?[0-9]+r[0-9A-Za-z]+", |lex| {
        // Radix notation like 16rFF
        Some(lex.slice().to_string())
    })]
    RadixNumber(String),

    #[regex(r"\$.", |lex| lex.slice().chars().nth(1))]
    Character(char),

    #[regex(r"'[^']*'", |lex| {
        let s = lex.slice();
        Some(s[1..s.len()-1].to_string())
    })]
    String(String),

    #[regex(r"#[A-Za-z_][A-Za-z0-9_:]*", |lex| Some(lex.slice()[1..].to_string()))]
    Symbol(String),

    #[regex(r"#\([^)]*\)", |lex| Some(lex.slice().to_string()))]
    LiteralArray(String),

    // Identifiers
    #[regex(r"[A-Z][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    ClassName(String),

    #[regex(r"[a-z_][A-Za-z0-9_]*:", |lex| lex.slice().to_string())]
    KeywordSelector(String),

    #[regex(r"[a-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Comments
    #[regex(r#""[^"]*""#)]
    Comment,
}

/// Smalltalk parser
pub struct SmalltalkParser {
    tokens: Vec<(SmalltalkToken, std::ops::Range<usize>)>,
    pos: usize,
    source: String,
}

impl SmalltalkParser {
    pub fn new(source: &str) -> Self {
        let mut tokens = Vec::new();
        let lexer = SmalltalkToken::lexer(source);
        for (token, span) in lexer.spanned() {
            if let Ok(t) = token {
                if !matches!(t, SmalltalkToken::Comment) {
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
        let mut builder = IrBuilder::with_language("smalltalk_module", SourceLanguage::Smalltalk);

        while self.pos < self.tokens.len() {
            if self.is_class_definition() {
                let (typedef, methods) = self.parse_class_definition()?;
                builder.add_type(typedef);
                for method in methods {
                    builder.add_function(method);
                }
            } else if self.is_method_definition() {
                let method = self.parse_method()?;
                builder.add_function(method);
            } else {
                self.pos += 1;
            }
        }

        Ok(builder.build())
    }

    fn is_class_definition(&self) -> bool {
        if let Some((SmalltalkToken::ClassName(_), _)) = self.tokens.get(self.pos) {
            if let Some((SmalltalkToken::Subclass, _)) = self.tokens.get(self.pos + 1) {
                return true;
            }
        }
        false
    }

    fn is_method_definition(&self) -> bool {
        matches!(
            self.tokens.get(self.pos),
            Some((SmalltalkToken::Identifier(_), _)) | Some((SmalltalkToken::KeywordSelector(_), _))
        )
    }

    fn parse_class_definition(&mut self) -> Result<(IrTypeDef, Vec<IrFunction>)> {
        let superclass = if let Some((SmalltalkToken::ClassName(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            name.clone()
        } else {
            "Object".to_string()
        };

        // Skip 'subclass:'
        if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::Subclass, _))) {
            self.pos += 1;
        }

        // Get class name (as symbol #ClassName)
        let class_name = if let Some((SmalltalkToken::Symbol(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            name.clone()
        } else {
            return Err(TranspileError::ParseError {
                message: "Expected class name symbol".to_string(),
                location: self.current_location(),
            });
        };

        let mut instance_vars = Vec::new();

        // Parse instance variable names
        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                SmalltalkToken::InstanceVariableNames => {
                    self.pos += 1;
                    if let Some((SmalltalkToken::String(vars), _)) = self.tokens.get(self.pos) {
                        for var in vars.split_whitespace() {
                            instance_vars.push((var.to_string(), IrType::Any));
                        }
                        self.pos += 1;
                    }
                }
                SmalltalkToken::ClassVariableNames | SmalltalkToken::PoolDictionaries | SmalltalkToken::Category => {
                    self.pos += 1;
                    if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::String(_), _))) {
                        self.pos += 1;
                    }
                }
                SmalltalkToken::Period => {
                    self.pos += 1;
                    break;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        // Add superclass as first field for inheritance
        let mut fields = vec![("_super".to_string(), IrType::Struct(superclass))];
        fields.extend(instance_vars);

        let typedef = IrTypeDef::Struct {
            name: class_name,
            fields,
            derives: vec!["Debug".into(), "Clone".into()],
        };

        Ok((typedef, Vec::new()))
    }

    fn parse_method(&mut self) -> Result<IrFunction> {
        let mut name = String::new();
        let mut params = Vec::new();

        // Parse method selector
        match &self.tokens[self.pos].0 {
            // Unary message
            SmalltalkToken::Identifier(id) => {
                name = id.clone();
                self.pos += 1;
            }
            // Binary message
            SmalltalkToken::Plus | SmalltalkToken::Minus | SmalltalkToken::Star |
            SmalltalkToken::Slash | SmalltalkToken::LessThan | SmalltalkToken::GreaterThan |
            SmalltalkToken::Equal | SmalltalkToken::Comma => {
                name = self.token_to_method_name(&self.tokens[self.pos].0);
                self.pos += 1;
                if let Some((SmalltalkToken::Identifier(arg), _)) = self.tokens.get(self.pos) {
                    params.push(IrParam {
                        name: arg.clone(),
                        ty: IrType::Any,
                        is_mutable: false,
                        by_ref: false,
                    });
                    self.pos += 1;
                }
            }
            // Keyword message
            SmalltalkToken::KeywordSelector(kw) => {
                name = kw.clone();
                self.pos += 1;
                if let Some((SmalltalkToken::Identifier(arg), _)) = self.tokens.get(self.pos) {
                    params.push(IrParam {
                        name: arg.clone(),
                        ty: IrType::Any,
                        is_mutable: false,
                        by_ref: false,
                    });
                    self.pos += 1;
                }
                // Parse additional keyword parts
                while let Some((SmalltalkToken::KeywordSelector(kw2), _)) = self.tokens.get(self.pos) {
                    name.push_str(kw2);
                    self.pos += 1;
                    if let Some((SmalltalkToken::Identifier(arg), _)) = self.tokens.get(self.pos) {
                        params.push(IrParam {
                            name: arg.clone(),
                            ty: IrType::Any,
                            is_mutable: false,
                            by_ref: false,
                        });
                        self.pos += 1;
                    }
                }
            }
            _ => {
                return Err(TranspileError::ParseError {
                    message: "Expected method selector".to_string(),
                    location: self.current_location(),
                });
            }
        }

        // Parse temporary variables
        let mut temps = Vec::new();
        if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::Pipe, _))) {
            self.pos += 1;
            while self.pos < self.tokens.len() {
                if matches!(self.tokens[self.pos].0, SmalltalkToken::Pipe) {
                    self.pos += 1;
                    break;
                }
                if let SmalltalkToken::Identifier(id) = &self.tokens[self.pos].0 {
                    temps.push(id.clone());
                }
                self.pos += 1;
            }
        }

        // Parse method body
        let mut body = Vec::new();

        // Declare temps
        for temp in temps {
            body.push(IrExpr::Assign {
                target: temp,
                value: Box::new(IrExpr::Nil),
            });
        }

        // Parse statements
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, SmalltalkToken::Period) {
                self.pos += 1;
                if self.pos >= self.tokens.len() || self.is_method_definition() || self.is_class_definition() {
                    break;
                }
            }
            let expr = self.parse_expression()?;
            body.push(expr);
        }

        // Add self parameter
        params.insert(0, IrParam {
            name: "self".to_string(),
            ty: IrType::Any,
            is_mutable: true,
            by_ref: true,
        });

        Ok(IrFunction {
            name: name.replace(':', "_"),
            generics: vec![],
            params,
            return_type: IrType::Any,
            body,
            is_unsafe: false,
            visibility: Visibility::Public,
            attributes: vec![],
            span: None,
        })
    }

    fn parse_expression(&mut self) -> Result<IrExpr> {
        // Check for return
        if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::Caret, _))) {
            self.pos += 1;
            let value = self.parse_expression()?;
            return Ok(IrExpr::Return(Box::new(value)));
        }

        // Check for assignment
        if let Some((SmalltalkToken::Identifier(name), _)) = self.tokens.get(self.pos) {
            if matches!(self.tokens.get(self.pos + 1), Some((SmalltalkToken::Assign, _))) {
                let target = name.clone();
                self.pos += 2;
                let value = self.parse_expression()?;
                return Ok(IrExpr::Assign {
                    target,
                    value: Box::new(value),
                });
            }
        }

        self.parse_keyword_send()
    }

    fn parse_keyword_send(&mut self) -> Result<IrExpr> {
        let receiver = self.parse_binary_send()?;

        // Check for keyword message
        if let Some((SmalltalkToken::KeywordSelector(kw), _)) = self.tokens.get(self.pos) {
            let mut selector = kw.clone();
            let mut args = Vec::new();
            self.pos += 1;

            let arg = self.parse_binary_send()?;
            args.push(arg);

            // Parse additional keyword parts
            while let Some((SmalltalkToken::KeywordSelector(kw2), _)) = self.tokens.get(self.pos) {
                selector.push_str(kw2);
                self.pos += 1;
                let arg = self.parse_binary_send()?;
                args.push(arg);
            }

            return Ok(IrExpr::Call {
                func: Box::new(IrExpr::FieldAccess {
                    object: Box::new(receiver),
                    field: selector.replace(':', "_"),
                }),
                args,
            });
        }

        Ok(receiver)
    }

    fn parse_binary_send(&mut self) -> Result<IrExpr> {
        let mut left = self.parse_unary_send()?;

        while self.pos < self.tokens.len() {
            let op = match &self.tokens[self.pos].0 {
                SmalltalkToken::Plus => "+",
                SmalltalkToken::Minus => "-",
                SmalltalkToken::Star => "*",
                SmalltalkToken::Slash => "/",
                SmalltalkToken::LessThan => "<",
                SmalltalkToken::GreaterThan => ">",
                SmalltalkToken::LessEqual => "<=",
                SmalltalkToken::GreaterEqual => ">=",
                SmalltalkToken::Equal => "=",
                SmalltalkToken::NotEqual => "~=",
                SmalltalkToken::Comma => ",",
                SmalltalkToken::At => "@",
                _ => break,
            };

            self.pos += 1;
            let right = self.parse_unary_send()?;

            left = IrExpr::BinaryOp {
                op: op.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_send(&mut self) -> Result<IrExpr> {
        let mut receiver = self.parse_primary()?;

        while let Some((SmalltalkToken::Identifier(msg), _)) = self.tokens.get(self.pos) {
            let message = msg.clone();
            self.pos += 1;
            receiver = IrExpr::Call {
                func: Box::new(IrExpr::FieldAccess {
                    object: Box::new(receiver),
                    field: message,
                }),
                args: vec![],
            };
        }

        Ok(receiver)
    }

    fn parse_primary(&mut self) -> Result<IrExpr> {
        if self.pos >= self.tokens.len() {
            return Ok(IrExpr::Nil);
        }

        let (token, _span) = self.tokens[self.pos].clone();
        self.pos += 1;

        match token {
            SmalltalkToken::Integer(n) => Ok(IrExpr::Int(n)),
            SmalltalkToken::Float(f) => Ok(IrExpr::Float(f)),
            SmalltalkToken::Character(c) => Ok(IrExpr::Char(c)),
            SmalltalkToken::String(s) => Ok(IrExpr::String(s)),
            SmalltalkToken::Symbol(s) => Ok(IrExpr::Symbol(s)),
            SmalltalkToken::Nil => Ok(IrExpr::Nil),
            SmalltalkToken::True => Ok(IrExpr::Bool(true)),
            SmalltalkToken::False => Ok(IrExpr::Bool(false)),
            SmalltalkToken::Self_ => Ok(IrExpr::Identifier("self".to_string())),
            SmalltalkToken::Super => Ok(IrExpr::Identifier("super".to_string())),
            SmalltalkToken::Identifier(name) => Ok(IrExpr::Identifier(name)),
            SmalltalkToken::ClassName(name) => Ok(IrExpr::Identifier(name)),

            SmalltalkToken::LParen => {
                let expr = self.parse_expression()?;
                if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::RParen, _))) {
                    self.pos += 1;
                }
                Ok(expr)
            }

            SmalltalkToken::LBracket => self.parse_block(),

            SmalltalkToken::LBrace => {
                // Dynamic array
                let mut elements = Vec::new();
                while self.pos < self.tokens.len() {
                    if matches!(self.tokens[self.pos].0, SmalltalkToken::RBrace) {
                        self.pos += 1;
                        break;
                    }
                    if matches!(self.tokens[self.pos].0, SmalltalkToken::Period) {
                        self.pos += 1;
                        continue;
                    }
                    elements.push(self.parse_expression()?);
                }
                Ok(IrExpr::List(elements))
            }

            _ => {
                self.pos -= 1;
                Ok(IrExpr::Nil)
            }
        }
    }

    fn parse_block(&mut self) -> Result<IrExpr> {
        let mut params = Vec::new();
        let mut body = Vec::new();

        // Check for block parameters [ :x :y | ... ]
        while let Some((SmalltalkToken::Colon, _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            if let Some((SmalltalkToken::Identifier(name), _)) = self.tokens.get(self.pos) {
                params.push(name.clone());
                self.pos += 1;
            }
        }

        // Skip pipe after block params
        if !params.is_empty() {
            if matches!(self.tokens.get(self.pos), Some((SmalltalkToken::Pipe, _))) {
                self.pos += 1;
            }
        }

        // Parse block body
        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, SmalltalkToken::RBracket) {
                self.pos += 1;
                break;
            }
            if matches!(self.tokens[self.pos].0, SmalltalkToken::Period) {
                self.pos += 1;
                continue;
            }
            body.push(self.parse_expression()?);
        }

        // Represent block as a lambda/closure
        let param_types: Vec<IrType> = params.iter().map(|_| IrType::Any).collect();
        let block_type = IrType::Fn(param_types, Box::new(IrType::Any));

        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("block".to_string())),
            args: vec![
                IrExpr::List(params.into_iter().map(IrExpr::Identifier).collect()),
                IrExpr::Block(body),
            ],
        })
    }

    fn token_to_method_name(&self, token: &SmalltalkToken) -> String {
        match token {
            SmalltalkToken::Plus => "add".to_string(),
            SmalltalkToken::Minus => "sub".to_string(),
            SmalltalkToken::Star => "mul".to_string(),
            SmalltalkToken::Slash => "div".to_string(),
            SmalltalkToken::LessThan => "lt".to_string(),
            SmalltalkToken::GreaterThan => "gt".to_string(),
            SmalltalkToken::Equal => "eq".to_string(),
            SmalltalkToken::Comma => "comma".to_string(),
            SmalltalkToken::At => "at".to_string(),
            _ => "unknown".to_string(),
        }
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

/// Parse Smalltalk source code
pub fn parse(source: &str) -> Result<IrModule> {
    SmalltalkParser::new(source).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "Object subclass: #Point";
        let mut lexer = SmalltalkToken::lexer(source);
        assert!(matches!(lexer.next(), Some(Ok(SmalltalkToken::ClassName(_)))));
        assert!(matches!(lexer.next(), Some(Ok(SmalltalkToken::Subclass))));
        assert!(matches!(lexer.next(), Some(Ok(SmalltalkToken::Symbol(_)))));
    }

    #[test]
    fn test_literals() {
        let source = "42 3.14 'hello' #symbol $a";
        let tokens: Vec<_> = SmalltalkToken::lexer(source)
            .filter_map(|t| t.ok())
            .collect();
        assert!(matches!(tokens[0], SmalltalkToken::Integer(42)));
        assert!(matches!(tokens[1], SmalltalkToken::Float(_)));
        assert!(matches!(tokens[2], SmalltalkToken::String(_)));
        assert!(matches!(tokens[3], SmalltalkToken::Symbol(_)));
        assert!(matches!(tokens[4], SmalltalkToken::Character('a')));
    }

    #[test]
    fn test_simple_method() {
        let source = "double ^self * 2";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn test_keyword_method() {
        let source = "add: x ^self + x";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].params.len(), 2); // self + x
    }
}
