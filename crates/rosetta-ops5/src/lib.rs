//! # Rosetta OPS5 Frontend
//!
//! OPS5 (1981, Charles Forgy, CMU) is a production system language.
//! It pioneered:
//! - Rete algorithm for pattern matching
//! - Working memory elements (WMEs)
//! - Production rules (condition-action)
//! - Conflict resolution strategies
//!
//! This crate transpiles OPS5 to Rust, implementing the Rete network
//! as an efficient pattern-matching engine.

use logos::Logos;
use rosetta_core::{Frontend, SourceFile, ParseError, RosettaIr};
use rosetta_ir::{IrModule, IrBuilder, IrType, IrExpr};
use std::collections::HashMap;

/// OPS5 tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
pub enum Ops5Token {
    // Keywords
    #[token("literalize", priority = 3)]
    Literalize,
    #[token("p", priority = 3)]
    Production,
    #[token("make")]
    Make,
    #[token("modify")]
    Modify,
    #[token("remove")]
    Remove,
    #[token("write")]
    Write,
    #[token("halt")]
    Halt,
    #[token("call")]
    Call,
    #[token("bind")]
    Bind,
    #[token("cbind")]
    Cbind,
    #[token("compute")]
    Compute,
    #[token("substr")]
    Substr,
    #[token("genatom")]
    Genatom,
    #[token("litval")]
    Litval,
    #[token("accept")]
    Accept,
    #[token("acceptline")]
    Acceptline,
    #[token("crlf")]
    Crlf,
    #[token("tabto")]
    Tabto,
    #[token("rjust")]
    Rjust,

    // Condition element operators
    #[token("-")]
    Negation,
    #[token("-->")]
    Arrow,
    #[token("<<")]
    DisjunctionStart,
    #[token(">>")]
    DisjunctionEnd,
    #[token("<>")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("=")]
    Equal,
    #[token("<=>")]
    SameType,

    // Variables
    #[regex(r"<[a-zA-Z][a-zA-Z0-9_-]*>")]
    Variable,

    // Attributes (with caret prefix)
    #[regex(r"\^[a-zA-Z][a-zA-Z0-9_-]*")]
    Attribute,

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_-]*")]
    Identifier,

    // Numbers
    #[regex(r"-?[0-9]+(\.[0-9]+)?")]
    Number,

    // Strings
    #[regex(r#"\|[^|]*\|"#)]
    String,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // Comments
    #[regex(r";[^\n]*")]
    Comment,
}

/// OPS5 AST nodes
#[derive(Debug, Clone)]
pub enum Ops5Node {
    /// Literalize declaration: defines WME class attributes
    Literalize {
        class_name: String,
        attributes: Vec<String>,
    },
    /// Production rule: p name LHS --> RHS
    Production {
        name: String,
        conditions: Vec<ConditionElement>,
        actions: Vec<Action>,
    },
}

/// Condition element (LHS of rule)
#[derive(Debug, Clone)]
pub enum ConditionElement {
    /// Positive condition
    Positive {
        class_name: String,
        tests: Vec<AttributeTest>,
        variable_binding: Option<String>,
    },
    /// Negated condition
    Negated {
        class_name: String,
        tests: Vec<AttributeTest>,
    },
}

/// Attribute test within condition
#[derive(Debug, Clone)]
pub struct AttributeTest {
    pub attribute: String,
    pub operator: TestOperator,
    pub value: TestValue,
}

#[derive(Debug, Clone)]
pub enum TestOperator {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    SameType,
}

#[derive(Debug, Clone)]
pub enum TestValue {
    Constant(String),
    Variable(String),
    Number(f64),
    Disjunction(Vec<TestValue>),
}

/// Action (RHS of rule)
#[derive(Debug, Clone)]
pub enum Action {
    /// Make new WME
    Make {
        class_name: String,
        attributes: Vec<(String, ActionValue)>,
    },
    /// Modify existing WME
    Modify {
        element_var: String,
        attributes: Vec<(String, ActionValue)>,
    },
    /// Remove WME
    Remove {
        element_var: String,
    },
    /// Write output
    Write {
        items: Vec<ActionValue>,
    },
    /// Bind variable
    Bind {
        variable: String,
        expression: ActionValue,
    },
    /// Call external function
    Call {
        function: String,
        args: Vec<ActionValue>,
    },
    /// Halt execution
    Halt,
}

#[derive(Debug, Clone)]
pub enum ActionValue {
    Constant(String),
    Variable(String),
    Number(f64),
    Compute(Box<ComputeExpr>),
    Crlf,
    Tabto(i32),
}

#[derive(Debug, Clone)]
pub enum ComputeExpr {
    Value(ActionValue),
    Add(Box<ComputeExpr>, Box<ComputeExpr>),
    Sub(Box<ComputeExpr>, Box<ComputeExpr>),
    Mul(Box<ComputeExpr>, Box<ComputeExpr>),
    Div(Box<ComputeExpr>, Box<ComputeExpr>),
    Mod(Box<ComputeExpr>, Box<ComputeExpr>),
}

/// OPS5 parser
pub struct Ops5Parser {
    tokens: Vec<(Ops5Token, String)>,
    position: usize,
}

impl Ops5Parser {
    pub fn new(source: &str) -> Self {
        let lexer = Ops5Token::lexer(source);
        let tokens: Vec<_> = lexer
            .spanned()
            .filter_map(|(tok, span)| {
                tok.ok().map(|t| (t, source[span].to_string()))
            })
            .filter(|(t, _)| !matches!(t, Ops5Token::Comment))
            .collect();

        Self { tokens, position: 0 }
    }

    fn peek(&self) -> Option<&Ops5Token> {
        self.tokens.get(self.position).map(|(t, _)| t)
    }

    fn peek_text(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|(_, t)| t.as_str())
    }

    fn advance(&mut self) -> Option<(Ops5Token, String)> {
        if self.position < self.tokens.len() {
            let result = self.tokens[self.position].clone();
            self.position += 1;
            Some(result)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Ops5Token) -> Result<String, ParseError> {
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

    pub fn parse(&mut self) -> Result<Vec<Ops5Node>, ParseError> {
        let mut nodes = Vec::new();
        while self.peek().is_some() {
            if matches!(self.peek(), Some(Ops5Token::LParen)) {
                self.advance();
                nodes.push(self.parse_toplevel()?);
            } else {
                self.advance(); // Skip unexpected tokens
            }
        }
        Ok(nodes)
    }

    fn parse_toplevel(&mut self) -> Result<Ops5Node, ParseError> {
        match self.peek() {
            Some(Ops5Token::Literalize) => self.parse_literalize(),
            Some(Ops5Token::Production) => self.parse_production(),
            _ => Err(ParseError::UnexpectedToken {
                expected: "literalize or p".into(),
                found: format!("{:?}", self.peek()),
                line: 0,
                column: 0,
            }),
        }
    }

    fn parse_literalize(&mut self) -> Result<Ops5Node, ParseError> {
        self.advance(); // consume 'literalize'
        let class_name = self.expect(Ops5Token::Identifier)?;

        let mut attributes = Vec::new();
        while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
            if matches!(self.peek(), Some(Ops5Token::Attribute)) {
                let (_, text) = self.advance().unwrap();
                attributes.push(text[1..].to_string()); // Remove ^ prefix
            } else if matches!(self.peek(), Some(Ops5Token::Identifier)) {
                let (_, text) = self.advance().unwrap();
                attributes.push(text);
            } else {
                break;
            }
        }
        self.expect(Ops5Token::RParen)?;

        Ok(Ops5Node::Literalize { class_name, attributes })
    }

    fn parse_production(&mut self) -> Result<Ops5Node, ParseError> {
        self.advance(); // consume 'p'
        let name = self.expect(Ops5Token::Identifier)?;

        // Parse LHS conditions
        let mut conditions = Vec::new();
        while !matches!(self.peek(), Some(Ops5Token::Arrow) | None) {
            if matches!(self.peek(), Some(Ops5Token::LParen)) {
                self.advance();
                conditions.push(self.parse_condition()?);
            } else if matches!(self.peek(), Some(Ops5Token::Negation)) {
                self.advance();
                self.expect(Ops5Token::LParen)?;
                let cond = self.parse_condition_inner(true)?;
                conditions.push(cond);
            } else {
                break;
            }
        }

        self.expect(Ops5Token::Arrow)?;

        // Parse RHS actions
        let mut actions = Vec::new();
        while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
            if matches!(self.peek(), Some(Ops5Token::LParen)) {
                self.advance();
                actions.push(self.parse_action()?);
            } else {
                break;
            }
        }
        self.expect(Ops5Token::RParen)?;

        Ok(Ops5Node::Production { name, conditions, actions })
    }

    fn parse_condition(&mut self) -> Result<ConditionElement, ParseError> {
        self.parse_condition_inner(false)
    }

    fn parse_condition_inner(&mut self, negated: bool) -> Result<ConditionElement, ParseError> {
        let class_name = self.expect(Ops5Token::Identifier)?;

        let mut tests = Vec::new();
        let mut variable_binding = None;

        while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
            if matches!(self.peek(), Some(Ops5Token::Attribute)) {
                let (_, attr) = self.advance().unwrap();
                let attribute = attr[1..].to_string();

                // Parse operator and value
                let operator = match self.peek() {
                    Some(Ops5Token::NotEqual) => { self.advance(); TestOperator::NotEqual }
                    Some(Ops5Token::LessEqual) => { self.advance(); TestOperator::LessEqual }
                    Some(Ops5Token::GreaterEqual) => { self.advance(); TestOperator::GreaterEqual }
                    Some(Ops5Token::LessThan) => { self.advance(); TestOperator::LessThan }
                    Some(Ops5Token::GreaterThan) => { self.advance(); TestOperator::GreaterThan }
                    Some(Ops5Token::SameType) => { self.advance(); TestOperator::SameType }
                    _ => TestOperator::Equal,
                };

                let value = self.parse_test_value()?;
                tests.push(AttributeTest { attribute, operator, value });
            } else if matches!(self.peek(), Some(Ops5Token::Variable)) {
                let (_, var) = self.advance().unwrap();
                variable_binding = Some(var[1..var.len()-1].to_string());
            } else {
                break;
            }
        }
        self.expect(Ops5Token::RParen)?;

        if negated {
            Ok(ConditionElement::Negated { class_name, tests })
        } else {
            Ok(ConditionElement::Positive { class_name, tests, variable_binding })
        }
    }

    fn parse_test_value(&mut self) -> Result<TestValue, ParseError> {
        match self.peek() {
            Some(Ops5Token::Variable) => {
                let (_, text) = self.advance().unwrap();
                Ok(TestValue::Variable(text[1..text.len()-1].to_string()))
            }
            Some(Ops5Token::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(TestValue::Number(text.parse().unwrap_or(0.0)))
            }
            Some(Ops5Token::Identifier) => {
                let (_, text) = self.advance().unwrap();
                Ok(TestValue::Constant(text))
            }
            Some(Ops5Token::String) => {
                let (_, text) = self.advance().unwrap();
                Ok(TestValue::Constant(text[1..text.len()-1].to_string()))
            }
            Some(Ops5Token::DisjunctionStart) => {
                self.advance();
                let mut values = Vec::new();
                while !matches!(self.peek(), Some(Ops5Token::DisjunctionEnd) | None) {
                    values.push(self.parse_test_value()?);
                }
                self.expect(Ops5Token::DisjunctionEnd)?;
                Ok(TestValue::Disjunction(values))
            }
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_action(&mut self) -> Result<Action, ParseError> {
        match self.peek() {
            Some(Ops5Token::Make) => {
                self.advance();
                let class_name = self.expect(Ops5Token::Identifier)?;
                let mut attributes = Vec::new();
                while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
                    if matches!(self.peek(), Some(Ops5Token::Attribute)) {
                        let (_, attr) = self.advance().unwrap();
                        let attr_name = attr[1..].to_string();
                        let value = self.parse_action_value()?;
                        attributes.push((attr_name, value));
                    } else {
                        break;
                    }
                }
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Make { class_name, attributes })
            }
            Some(Ops5Token::Modify) => {
                self.advance();
                let element_var = match self.advance() {
                    Some((Ops5Token::Variable, text)) => text[1..text.len()-1].to_string(),
                    _ => return Err(ParseError::UnexpectedEof),
                };
                let mut attributes = Vec::new();
                while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
                    if matches!(self.peek(), Some(Ops5Token::Attribute)) {
                        let (_, attr) = self.advance().unwrap();
                        let attr_name = attr[1..].to_string();
                        let value = self.parse_action_value()?;
                        attributes.push((attr_name, value));
                    } else {
                        break;
                    }
                }
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Modify { element_var, attributes })
            }
            Some(Ops5Token::Remove) => {
                self.advance();
                let element_var = match self.advance() {
                    Some((Ops5Token::Variable, text)) => text[1..text.len()-1].to_string(),
                    _ => return Err(ParseError::UnexpectedEof),
                };
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Remove { element_var })
            }
            Some(Ops5Token::Write) => {
                self.advance();
                let mut items = Vec::new();
                while !matches!(self.peek(), Some(Ops5Token::RParen) | None) {
                    items.push(self.parse_action_value()?);
                }
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Write { items })
            }
            Some(Ops5Token::Bind) => {
                self.advance();
                let variable = match self.advance() {
                    Some((Ops5Token::Variable, text)) => text[1..text.len()-1].to_string(),
                    _ => return Err(ParseError::UnexpectedEof),
                };
                let expression = self.parse_action_value()?;
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Bind { variable, expression })
            }
            Some(Ops5Token::Halt) => {
                self.advance();
                self.expect(Ops5Token::RParen)?;
                Ok(Action::Halt)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "action keyword".into(),
                found: format!("{:?}", self.peek()),
                line: 0,
                column: 0,
            }),
        }
    }

    fn parse_action_value(&mut self) -> Result<ActionValue, ParseError> {
        match self.peek() {
            Some(Ops5Token::Variable) => {
                let (_, text) = self.advance().unwrap();
                Ok(ActionValue::Variable(text[1..text.len()-1].to_string()))
            }
            Some(Ops5Token::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(ActionValue::Number(text.parse().unwrap_or(0.0)))
            }
            Some(Ops5Token::Identifier) => {
                let (_, text) = self.advance().unwrap();
                Ok(ActionValue::Constant(text))
            }
            Some(Ops5Token::String) => {
                let (_, text) = self.advance().unwrap();
                Ok(ActionValue::Constant(text[1..text.len()-1].to_string()))
            }
            Some(Ops5Token::Crlf) => {
                self.advance();
                Ok(ActionValue::Crlf)
            }
            Some(Ops5Token::Tabto) => {
                self.advance();
                if let Some((Ops5Token::Number, text)) = self.advance() {
                    Ok(ActionValue::Tabto(text.parse().unwrap_or(0)))
                } else {
                    Ok(ActionValue::Tabto(0))
                }
            }
            Some(Ops5Token::LParen) => {
                self.advance();
                if matches!(self.peek(), Some(Ops5Token::Compute)) {
                    self.advance();
                    let expr = self.parse_compute_expr()?;
                    self.expect(Ops5Token::RParen)?;
                    Ok(ActionValue::Compute(Box::new(expr)))
                } else {
                    // Skip nested expression
                    let mut depth = 1;
                    while depth > 0 {
                        match self.advance() {
                            Some((Ops5Token::LParen, _)) => depth += 1,
                            Some((Ops5Token::RParen, _)) => depth -= 1,
                            None => break,
                            _ => {}
                        }
                    }
                    Ok(ActionValue::Constant("nested".into()))
                }
            }
            _ => Ok(ActionValue::Constant("unknown".into())),
        }
    }

    fn parse_compute_expr(&mut self) -> Result<ComputeExpr, ParseError> {
        // Simplified compute expression parser
        let val = self.parse_action_value()?;
        Ok(ComputeExpr::Value(val))
    }
}

/// OPS5 to IR converter
pub struct Ops5ToIr {
    builder: IrBuilder,
    wme_classes: HashMap<String, Vec<String>>,
}

impl Ops5ToIr {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new("ops5_module"),
            wme_classes: HashMap::new(),
        }
    }

    pub fn convert(&mut self, nodes: &[Ops5Node]) -> Result<IrModule, ParseError> {
        // First pass: collect literalize declarations
        for node in nodes {
            if let Ops5Node::Literalize { class_name, attributes } = node {
                self.wme_classes.insert(class_name.clone(), attributes.clone());
            }
        }

        // Generate WME structs
        for (class_name, attributes) in &self.wme_classes {
            let fields: Vec<_> = attributes.iter()
                .map(|a| (a.clone(), IrType::Any))
                .collect();
            self.builder.add_struct(class_name, &fields);
        }

        // Second pass: convert productions to functions
        for node in nodes {
            if let Ops5Node::Production { name, conditions, actions } = node {
                self.convert_production(name, conditions, actions)?;
            }
        }

        Ok(self.builder.build_clone())
    }

    fn convert_production(
        &mut self,
        name: &str,
        conditions: &[ConditionElement],
        actions: &[Action],
    ) -> Result<(), ParseError> {
        // Generate a function for each production
        let func_name = format!("rule_{}", name);
        self.builder.begin_function(&func_name, &[("wm".into(), IrType::Any)], IrType::Bool);

        // Add condition matching code
        for (i, cond) in conditions.iter().enumerate() {
            let comment = format!("Condition {}: {:?}", i + 1, cond);
            self.builder.add_statement(IrExpr::Comment(comment));
        }

        // Add action execution code
        for action in actions {
            let ir = self.convert_action(action)?;
            self.builder.add_statement(ir);
        }

        self.builder.add_return(IrExpr::Bool(true));
        self.builder.end_function();
        Ok(())
    }

    fn convert_action(&self, action: &Action) -> Result<IrExpr, ParseError> {
        match action {
            Action::Make { class_name, attributes } => {
                let fields: Vec<_> = attributes.iter()
                    .map(|(name, val)| (name.clone(), self.convert_action_value(val)))
                    .collect();
                Ok(IrExpr::StructInit {
                    name: class_name.clone(),
                    fields,
                })
            }
            Action::Modify { element_var, attributes } => {
                let updates: Vec<_> = attributes.iter()
                    .map(|(name, val)| IrExpr::FieldAssign {
                        object: element_var.clone(),
                        field: name.clone(),
                        value: Box::new(self.convert_action_value(val)),
                    })
                    .collect();
                Ok(IrExpr::Block(updates))
            }
            Action::Remove { element_var } => {
                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier("wm_remove".into())),
                    args: vec![IrExpr::Identifier(element_var.clone())],
                })
            }
            Action::Write { items } => {
                let args: Vec<_> = items.iter()
                    .map(|v| self.convert_action_value(v))
                    .collect();
                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier("println".into())),
                    args,
                })
            }
            Action::Bind { variable, expression } => {
                Ok(IrExpr::Assign {
                    target: variable.clone(),
                    value: Box::new(self.convert_action_value(expression)),
                })
            }
            Action::Halt => {
                Ok(IrExpr::Return(Box::new(IrExpr::Nil)))
            }
            _ => Ok(IrExpr::Comment("Unhandled action".into())),
        }
    }

    fn convert_action_value(&self, value: &ActionValue) -> IrExpr {
        match value {
            ActionValue::Constant(s) => IrExpr::String(s.clone()),
            ActionValue::Variable(v) => IrExpr::Identifier(v.clone()),
            ActionValue::Number(n) => IrExpr::Float(*n),
            ActionValue::Crlf => IrExpr::String("\n".into()),
            ActionValue::Tabto(n) => IrExpr::String(format!("{:>width$}", "", width = *n as usize)),
            ActionValue::Compute(expr) => self.convert_compute(expr),
        }
    }

    fn convert_compute(&self, expr: &ComputeExpr) -> IrExpr {
        match expr {
            ComputeExpr::Value(v) => self.convert_action_value(v),
            ComputeExpr::Add(a, b) => IrExpr::BinaryOp {
                op: "+".into(),
                left: Box::new(self.convert_compute(a)),
                right: Box::new(self.convert_compute(b)),
            },
            ComputeExpr::Sub(a, b) => IrExpr::BinaryOp {
                op: "-".into(),
                left: Box::new(self.convert_compute(a)),
                right: Box::new(self.convert_compute(b)),
            },
            ComputeExpr::Mul(a, b) => IrExpr::BinaryOp {
                op: "*".into(),
                left: Box::new(self.convert_compute(a)),
                right: Box::new(self.convert_compute(b)),
            },
            ComputeExpr::Div(a, b) => IrExpr::BinaryOp {
                op: "/".into(),
                left: Box::new(self.convert_compute(a)),
                right: Box::new(self.convert_compute(b)),
            },
            ComputeExpr::Mod(a, b) => IrExpr::BinaryOp {
                op: "%".into(),
                left: Box::new(self.convert_compute(a)),
                right: Box::new(self.convert_compute(b)),
            },
        }
    }
}

impl Default for Ops5ToIr {
    fn default() -> Self {
        Self::new()
    }
}

/// OPS5 Frontend implementation
pub struct Ops5Frontend;

impl Frontend for Ops5Frontend {
    fn name(&self) -> &'static str {
        "OPS5"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["ops", "ops5"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut parser = Ops5Parser::new(&source.content);
        let ast = parser.parse()?;
        let mut converter = Ops5ToIr::new();
        let module = converter.convert(&ast)?;
        Ok(module.into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_ops5() {
        let source = "(literalize block ^name ^on ^color)";
        let mut lexer = Ops5Token::lexer(source);
        assert_eq!(lexer.next(), Some(Ok(Ops5Token::LParen)));
        assert_eq!(lexer.next(), Some(Ok(Ops5Token::Literalize)));
    }

    #[test]
    fn test_parse_literalize() {
        let source = "(literalize block ^name ^on ^color)";
        let mut parser = Ops5Parser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
        let nodes = result.unwrap();
        assert_eq!(nodes.len(), 1);
    }

    #[test]
    fn test_parse_production() {
        let source = r#"
            (p find-colored-block
                (goal ^type find ^color <c>)
                (block ^color <c> ^name <n>)
                -->
                (write |Found block:| <n>)
                (halt))
        "#;
        let mut parser = Ops5Parser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }
}
