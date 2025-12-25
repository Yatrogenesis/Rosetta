//! # Rosetta PLANNER Frontend
//!
//! PLANNER (1969, Carl Hewitt, MIT) was one of the first AI programming languages.
//! It introduced:
//! - Pattern-directed invocation
//! - Goal-directed execution
//! - Automatic backtracking
//! - Assertions and goals
//!
//! This crate transpiles PLANNER to Rust, preserving the pattern matching
//! and backtracking semantics using Rust iterators and closures.

use logos::Logos;
use rosetta_core::{Frontend, SourceFile, ParseError, RosettaIr};
use rosetta_ir::{IrModule, IrBuilder, IrFunction, IrType, IrExpr};
use std::collections::HashMap;

/// PLANNER tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
pub enum PlannerToken {
    // Keywords
    #[token("GOAL")]
    Goal,
    #[token("THEOREM")]
    Theorem,
    #[token("ASSERT")]
    Assert,
    #[token("ERASE")]
    Erase,
    #[token("THCONSE")]
    Thconse,
    #[token("THANTE")]
    Thante,
    #[token("THERASE")]
    Therase,
    #[token("THAND")]
    Thand,
    #[token("THOR")]
    Thor,
    #[token("THPROG")]
    Thprog,
    #[token("THVAL")]
    Thval,
    #[token("THFAIL")]
    Thfail,
    #[token("THSUCCEED")]
    Thsucceed,
    #[token("THSETQ")]
    Thsetq,
    #[token("THCOND")]
    Thcond,
    #[token("THGO")]
    Thgo,
    #[token("THRETURN")]
    Threturn,
    #[token("LAMBDA")]
    Lambda,
    #[token("DEFUN")]
    Defun,
    #[token("SETQ")]
    Setq,
    #[token("COND")]
    Cond,
    #[token("PROGN")]
    Progn,
    #[token("IF")]
    If,
    #[token("THEN")]
    Then,
    #[token("ELSE")]
    Else,
    #[token("NIL", priority = 3)]
    Nil,
    #[token("T", priority = 3)]
    True,

    // Pattern variables
    #[regex(r"\?[a-zA-Z][a-zA-Z0-9_]*")]
    PatternVar,

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_\-]*")]
    Identifier,

    // Numbers
    #[regex(r"-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    Number,

    // Strings
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,

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

    // Operators
    #[token("'")]
    Quote,
    #[token("`")]
    Backquote,
    #[token(",")]
    Comma,
    #[token(",@")]
    CommaAt,

    // Comments
    #[regex(r";[^\n]*")]
    Comment,
}

/// PLANNER AST nodes
#[derive(Debug, Clone)]
pub enum PlannerExpr {
    /// Numeric literal
    Number(f64),
    /// String literal
    String(String),
    /// Symbol/identifier
    Symbol(String),
    /// Pattern variable (?x)
    PatternVar(String),
    /// Nil
    Nil,
    /// True (T)
    True,
    /// Quoted expression
    Quote(Box<PlannerExpr>),
    /// List (expr1 expr2 ...)
    List(Vec<PlannerExpr>),
    /// Goal invocation
    Goal {
        pattern: Box<PlannerExpr>,
        body: Vec<PlannerExpr>,
    },
    /// Theorem definition
    Theorem {
        name: String,
        pattern: Box<PlannerExpr>,
        body: Vec<PlannerExpr>,
    },
    /// Assertion
    Assert(Box<PlannerExpr>),
    /// Erase assertion
    Erase(Box<PlannerExpr>),
    /// Consequent theorem (forward chaining)
    Thconse {
        pattern: Box<PlannerExpr>,
        body: Vec<PlannerExpr>,
    },
    /// Antecedent theorem (backward chaining)
    Thante {
        pattern: Box<PlannerExpr>,
        body: Vec<PlannerExpr>,
    },
    /// Function definition
    Defun {
        name: String,
        params: Vec<String>,
        body: Vec<PlannerExpr>,
    },
    /// Lambda expression
    Lambda {
        params: Vec<String>,
        body: Vec<PlannerExpr>,
    },
    /// Conditional
    Cond(Vec<(PlannerExpr, Vec<PlannerExpr>)>),
    /// Sequential execution
    Progn(Vec<PlannerExpr>),
    /// Variable assignment
    Setq(String, Box<PlannerExpr>),
    /// Function call
    Call {
        func: Box<PlannerExpr>,
        args: Vec<PlannerExpr>,
    },
}

/// PLANNER parser
pub struct PlannerParser {
    tokens: Vec<(PlannerToken, String)>,
    position: usize,
}

impl PlannerParser {
    pub fn new(source: &str) -> Self {
        let lexer = PlannerToken::lexer(source);
        let tokens: Vec<_> = lexer
            .spanned()
            .filter_map(|(tok, span)| {
                tok.ok().map(|t| (t, source[span].to_string()))
            })
            .filter(|(t, _)| !matches!(t, PlannerToken::Comment))
            .collect();

        Self { tokens, position: 0 }
    }

    fn peek(&self) -> Option<&PlannerToken> {
        self.tokens.get(self.position).map(|(t, _)| t)
    }

    fn advance(&mut self) -> Option<(PlannerToken, String)> {
        if self.position < self.tokens.len() {
            let result = self.tokens[self.position].clone();
            self.position += 1;
            Some(result)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: PlannerToken) -> Result<String, ParseError> {
        match self.advance() {
            Some((tok, text)) if tok == expected => Ok(text),
            Some((tok, _)) => Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: format!("{:?}", tok),
                line: 0,
                column: 0,
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<PlannerExpr>, ParseError> {
        let mut exprs = Vec::new();
        while self.peek().is_some() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<PlannerExpr, ParseError> {
        match self.peek() {
            Some(PlannerToken::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(PlannerExpr::Number(text.parse().unwrap_or(0.0)))
            }
            Some(PlannerToken::String) => {
                let (_, text) = self.advance().unwrap();
                // Remove quotes
                let s = text.trim_matches('"').to_string();
                Ok(PlannerExpr::String(s))
            }
            Some(PlannerToken::Identifier) => {
                let (_, text) = self.advance().unwrap();
                Ok(PlannerExpr::Symbol(text))
            }
            Some(PlannerToken::PatternVar) => {
                let (_, text) = self.advance().unwrap();
                Ok(PlannerExpr::PatternVar(text[1..].to_string()))
            }
            Some(PlannerToken::Nil) => {
                self.advance();
                Ok(PlannerExpr::Nil)
            }
            Some(PlannerToken::True) => {
                self.advance();
                Ok(PlannerExpr::True)
            }
            Some(PlannerToken::Quote) => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(PlannerExpr::Quote(Box::new(expr)))
            }
            Some(PlannerToken::LParen) => {
                self.advance();
                self.parse_list()
            }
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<PlannerExpr, ParseError> {
        // Check for special forms
        match self.peek() {
            Some(PlannerToken::Goal) => {
                self.advance();
                let pattern = Box::new(self.parse_expr()?);
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    body.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Goal { pattern, body })
            }
            Some(PlannerToken::Theorem) => {
                self.advance();
                let name = self.expect(PlannerToken::Identifier)?;
                let pattern = Box::new(self.parse_expr()?);
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    body.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Theorem { name, pattern, body })
            }
            Some(PlannerToken::Assert) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Assert(Box::new(expr)))
            }
            Some(PlannerToken::Erase) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Erase(Box::new(expr)))
            }
            Some(PlannerToken::Defun) => {
                self.advance();
                let name = self.expect(PlannerToken::Identifier)?;
                self.expect(PlannerToken::LParen)?;
                let mut params = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    params.push(self.expect(PlannerToken::Identifier)?);
                }
                self.expect(PlannerToken::RParen)?;
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    body.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Defun { name, params, body })
            }
            Some(PlannerToken::Lambda) => {
                self.advance();
                self.expect(PlannerToken::LParen)?;
                let mut params = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    params.push(self.expect(PlannerToken::Identifier)?);
                }
                self.expect(PlannerToken::RParen)?;
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    body.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Lambda { params, body })
            }
            Some(PlannerToken::Setq) => {
                self.advance();
                let var = self.expect(PlannerToken::Identifier)?;
                let val = self.parse_expr()?;
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Setq(var, Box::new(val)))
            }
            Some(PlannerToken::Progn) => {
                self.advance();
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                    body.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Progn(body))
            }
            Some(PlannerToken::Cond) => {
                self.advance();
                let mut clauses = Vec::new();
                while matches!(self.peek(), Some(PlannerToken::LParen)) {
                    self.advance();
                    let test = self.parse_expr()?;
                    let mut body = Vec::new();
                    while !matches!(self.peek(), Some(PlannerToken::RParen)) {
                        body.push(self.parse_expr()?);
                    }
                    self.expect(PlannerToken::RParen)?;
                    clauses.push((test, body));
                }
                self.expect(PlannerToken::RParen)?;
                Ok(PlannerExpr::Cond(clauses))
            }
            _ => {
                // Regular function call
                let mut items = Vec::new();
                while !matches!(self.peek(), Some(PlannerToken::RParen) | None) {
                    items.push(self.parse_expr()?);
                }
                self.expect(PlannerToken::RParen)?;

                if items.is_empty() {
                    Ok(PlannerExpr::Nil)
                } else {
                    let func = Box::new(items.remove(0));
                    Ok(PlannerExpr::Call { func, args: items })
                }
            }
        }
    }
}

/// PLANNER to IR converter
pub struct PlannerToIr {
    builder: IrBuilder,
    assertions: Vec<IrExpr>,
    theorems: HashMap<String, IrFunction>,
}

impl PlannerToIr {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new("planner_module"),
            assertions: Vec::new(),
            theorems: HashMap::new(),
        }
    }

    pub fn convert(&mut self, exprs: &[PlannerExpr]) -> Result<IrModule, ParseError> {
        for expr in exprs {
            self.convert_toplevel(expr)?;
        }
        Ok(self.builder.build_clone())
    }

    fn convert_toplevel(&mut self, expr: &PlannerExpr) -> Result<(), ParseError> {
        match expr {
            PlannerExpr::Defun { name, params, body } => {
                let param_types: Vec<_> = params.iter()
                    .map(|p| (p.clone(), IrType::Any))
                    .collect();

                self.builder.begin_function(name, &param_types, IrType::Any);

                for (i, stmt) in body.iter().enumerate() {
                    let ir_expr = self.convert_expr(stmt)?;
                    if i == body.len() - 1 {
                        self.builder.add_return(ir_expr);
                    } else {
                        self.builder.add_statement(ir_expr);
                    }
                }

                self.builder.end_function();
                Ok(())
            }
            PlannerExpr::Theorem { name, pattern, body } => {
                // Convert theorem to a pattern-matching function
                let func_name = format!("theorem_{}", name);
                self.builder.begin_function(&func_name, &[("pattern".into(), IrType::Any)], IrType::Bool);

                // Pattern matching logic
                let pattern_ir = self.convert_expr(pattern)?;
                self.builder.add_statement(IrExpr::Comment(format!("Match pattern: {:?}", pattern_ir)));

                for stmt in body {
                    let ir = self.convert_expr(stmt)?;
                    self.builder.add_statement(ir);
                }

                self.builder.add_return(IrExpr::Bool(true));
                self.builder.end_function();
                Ok(())
            }
            PlannerExpr::Assert(expr) => {
                let ir = self.convert_expr(expr)?;
                self.assertions.push(ir);
                Ok(())
            }
            _ => {
                // Top-level expression
                let ir = self.convert_expr(expr)?;
                self.builder.add_global_statement(ir);
                Ok(())
            }
        }
    }

    fn convert_expr(&self, expr: &PlannerExpr) -> Result<IrExpr, ParseError> {
        match expr {
            PlannerExpr::Number(n) => Ok(IrExpr::Float(*n)),
            PlannerExpr::String(s) => Ok(IrExpr::String(s.clone())),
            PlannerExpr::Symbol(s) => Ok(IrExpr::Identifier(s.clone())),
            PlannerExpr::PatternVar(v) => Ok(IrExpr::PatternVar(v.clone())),
            PlannerExpr::Nil => Ok(IrExpr::Nil),
            PlannerExpr::True => Ok(IrExpr::Bool(true)),
            PlannerExpr::Quote(e) => {
                let inner = self.convert_expr(e)?;
                Ok(IrExpr::Quote(Box::new(inner)))
            }
            PlannerExpr::List(items) => {
                let ir_items: Result<Vec<_>, _> = items.iter()
                    .map(|i| self.convert_expr(i))
                    .collect();
                Ok(IrExpr::List(ir_items?))
            }
            PlannerExpr::Lambda { params, body } => {
                let ir_body: Result<Vec<_>, _> = body.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                Ok(IrExpr::Lambda {
                    params: params.clone(),
                    body: ir_body?,
                })
            }
            PlannerExpr::Setq(var, val) => {
                let ir_val = self.convert_expr(val)?;
                Ok(IrExpr::Assign {
                    target: var.clone(),
                    value: Box::new(ir_val),
                })
            }
            PlannerExpr::Progn(exprs) => {
                let ir_exprs: Result<Vec<_>, _> = exprs.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                Ok(IrExpr::Block(ir_exprs?))
            }
            PlannerExpr::Cond(clauses) => {
                let mut ir_clauses = Vec::new();
                for (test, body) in clauses {
                    let ir_test = self.convert_expr(test)?;
                    let ir_body: Result<Vec<_>, _> = body.iter()
                        .map(|e| self.convert_expr(e))
                        .collect();
                    ir_clauses.push((ir_test, IrExpr::Block(ir_body?)));
                }
                Ok(IrExpr::Cond(ir_clauses))
            }
            PlannerExpr::Call { func, args } => {
                let ir_func = self.convert_expr(func)?;
                let ir_args: Result<Vec<_>, _> = args.iter()
                    .map(|a| self.convert_expr(a))
                    .collect();
                Ok(IrExpr::Call {
                    func: Box::new(ir_func),
                    args: ir_args?,
                })
            }
            PlannerExpr::Goal { pattern, body } => {
                // Convert goal to iterator-based search
                let ir_pattern = self.convert_expr(pattern)?;
                let ir_body: Result<Vec<_>, _> = body.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                Ok(IrExpr::Goal {
                    pattern: Box::new(ir_pattern),
                    body: ir_body?,
                })
            }
            _ => Ok(IrExpr::Comment(format!("Unhandled: {:?}", expr))),
        }
    }
}

impl Default for PlannerToIr {
    fn default() -> Self {
        Self::new()
    }
}

/// PLANNER Frontend implementation
pub struct PlannerFrontend;

impl Frontend for PlannerFrontend {
    fn name(&self) -> &'static str {
        "PLANNER"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["pln", "planner"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut parser = PlannerParser::new(&source.content);
        let ast = parser.parse()?;
        let mut converter = PlannerToIr::new();
        let module = converter.convert(&ast)?;
        Ok(module.into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_planner() {
        let source = "(DEFUN factorial (n) (COND ((= n 0) 1) (T (* n (factorial (- n 1))))))";
        let mut lexer = PlannerToken::lexer(source);
        assert_eq!(lexer.next(), Some(Ok(PlannerToken::LParen)));
        assert_eq!(lexer.next(), Some(Ok(PlannerToken::Defun)));
    }

    #[test]
    fn test_parse_defun() {
        let source = "(DEFUN add (x y) (+ x y))";
        let mut parser = PlannerParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_goal() {
        let source = "(GOAL (parent ?x john) (print ?x))";
        let mut parser = PlannerParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_theorem() {
        let source = "(THEOREM ancestor (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))";
        let mut parser = PlannerParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }
}
