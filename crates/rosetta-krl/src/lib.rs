//! # Rosetta KRL Frontend
//!
//! KRL (Knowledge Representation Language, 1977, Daniel Bobrow & Terry Winograd)
//! was developed at Xerox PARC for AI research.
//!
//! Key concepts:
//! - Frame-based knowledge representation
//! - Slots with procedural attachment
//! - Inheritance hierarchies
//! - Demons (triggers on slot access)
//! - Perspectives (multiple views of objects)
//!
//! This crate transpiles KRL to Rust using structs with trait-based inheritance.

use logos::Logos;
use rosetta_core::{Frontend, SourceFile, ParseError, RosettaIr};
use rosetta_ir::{IrModule, IrBuilder, IrType, IrExpr};
use std::collections::HashMap;

/// KRL tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
pub enum KrlToken {
    // Frame definition keywords
    #[token("Unit")]
    Unit,
    #[token("AbstractUnit")]
    AbstractUnit,
    #[token("IndividualUnit")]
    IndividualUnit,
    #[token("isA")]
    IsA,
    #[token("hasSlot")]
    HasSlot,
    #[token("Slot")]
    Slot,
    #[token("Facet")]
    Facet,

    // Slot facets
    #[token("ValueClass")]
    ValueClass,
    #[token("Value")]
    Value,
    #[token("Default")]
    Default,
    #[token("IfNeeded")]
    IfNeeded,
    #[token("IfAdded")]
    IfAdded,
    #[token("IfRemoved")]
    IfRemoved,
    #[token("Cardinality")]
    Cardinality,
    #[token("Inverse")]
    Inverse,

    // Perspectives
    #[token("Perspective")]
    Perspective,
    #[token("asSelf")]
    AsSelf,
    #[token("asOther")]
    AsOther,

    // Actions
    #[token("Create")]
    Create,
    #[token("Describe")]
    Describe,
    #[token("GetSlot")]
    GetSlot,
    #[token("PutSlot")]
    PutSlot,
    #[token("RemoveSlot")]
    RemoveSlot,

    // Control flow
    #[token("If")]
    If,
    #[token("Then")]
    Then,
    #[token("Else")]
    Else,
    #[token("ForAll")]
    ForAll,
    #[token("ThereExists")]
    ThereExists,
    #[token("Let")]
    Let,
    #[token("Do")]
    Do,
    #[token("Return")]
    Return,

    // Logical operators
    #[token("And")]
    And,
    #[token("Or")]
    Or,
    #[token("Not")]
    Not,
    #[token("Implies")]
    Implies,

    // Comparison
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,

    // Arithmetic
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

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
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("<-")]
    BackArrow,

    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    #[regex(r"-?[0-9]+(\.[0-9]+)?")]
    Number,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,

    // Comments
    #[regex(r"#[^\n]*")]
    Comment,
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    BlockComment,
}

/// KRL AST nodes
#[derive(Debug, Clone)]
pub enum KrlNode {
    /// Unit (frame) definition
    Unit {
        name: String,
        is_abstract: bool,
        parents: Vec<String>,
        slots: Vec<SlotDef>,
        perspectives: Vec<PerspectiveDef>,
    },
    /// Slot definition
    Slot(SlotDef),
    /// Expression
    Expr(KrlExpr),
}

/// Slot definition
#[derive(Debug, Clone)]
pub struct SlotDef {
    pub name: String,
    pub facets: Vec<Facet>,
}

/// Slot facet
#[derive(Debug, Clone)]
pub enum Facet {
    ValueClass(String),
    Value(KrlExpr),
    Default(KrlExpr),
    IfNeeded(Vec<KrlExpr>),
    IfAdded(Vec<KrlExpr>),
    IfRemoved(Vec<KrlExpr>),
    Cardinality { min: usize, max: Option<usize> },
    Inverse(String),
}

/// Perspective definition
#[derive(Debug, Clone)]
pub struct PerspectiveDef {
    pub name: String,
    pub slot_mappings: Vec<(String, String)>,
}

/// KRL expressions
#[derive(Debug, Clone)]
pub enum KrlExpr {
    /// Identifier
    Identifier(String),
    /// Number literal
    Number(f64),
    /// String literal
    String(String),
    /// Slot access: object.slot
    SlotAccess {
        object: Box<KrlExpr>,
        slot: String,
    },
    /// Create new unit instance
    Create {
        unit: String,
        slot_values: Vec<(String, KrlExpr)>,
    },
    /// Get slot value
    GetSlot {
        object: Box<KrlExpr>,
        slot: String,
    },
    /// Put slot value
    PutSlot {
        object: Box<KrlExpr>,
        slot: String,
        value: Box<KrlExpr>,
    },
    /// Binary operation
    BinaryOp {
        op: String,
        left: Box<KrlExpr>,
        right: Box<KrlExpr>,
    },
    /// Unary operation
    UnaryOp {
        op: String,
        operand: Box<KrlExpr>,
    },
    /// Conditional
    If {
        condition: Box<KrlExpr>,
        then_branch: Vec<KrlExpr>,
        else_branch: Option<Vec<KrlExpr>>,
    },
    /// Universal quantifier
    ForAll {
        variable: String,
        collection: Box<KrlExpr>,
        body: Vec<KrlExpr>,
    },
    /// Existential quantifier
    ThereExists {
        variable: String,
        collection: Box<KrlExpr>,
        condition: Box<KrlExpr>,
    },
    /// Let binding
    Let {
        bindings: Vec<(String, KrlExpr)>,
        body: Vec<KrlExpr>,
    },
    /// Procedure call
    Call {
        name: String,
        args: Vec<KrlExpr>,
    },
    /// List literal
    List(Vec<KrlExpr>),
    /// Nil
    Nil,
}

/// KRL parser
pub struct KrlParser {
    tokens: Vec<(KrlToken, String)>,
    position: usize,
}

impl KrlParser {
    pub fn new(source: &str) -> Self {
        let lexer = KrlToken::lexer(source);
        let tokens: Vec<_> = lexer
            .spanned()
            .filter_map(|(tok, span)| {
                tok.ok().map(|t| (t, source[span].to_string()))
            })
            .filter(|(t, _)| !matches!(t, KrlToken::Comment | KrlToken::BlockComment))
            .collect();

        Self { tokens, position: 0 }
    }

    fn peek(&self) -> Option<&KrlToken> {
        self.tokens.get(self.position).map(|(t, _)| t)
    }

    fn advance(&mut self) -> Option<(KrlToken, String)> {
        if self.position < self.tokens.len() {
            let result = self.tokens[self.position].clone();
            self.position += 1;
            Some(result)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: KrlToken) -> Result<String, ParseError> {
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

    pub fn parse(&mut self) -> Result<Vec<KrlNode>, ParseError> {
        let mut nodes = Vec::new();
        while self.peek().is_some() {
            nodes.push(self.parse_toplevel()?);
        }
        Ok(nodes)
    }

    fn parse_toplevel(&mut self) -> Result<KrlNode, ParseError> {
        match self.peek() {
            Some(KrlToken::Unit) | Some(KrlToken::AbstractUnit) | Some(KrlToken::IndividualUnit) => {
                self.parse_unit()
            }
            _ => {
                let expr = self.parse_expr()?;
                Ok(KrlNode::Expr(expr))
            }
        }
    }

    fn parse_unit(&mut self) -> Result<KrlNode, ParseError> {
        let is_abstract = matches!(self.peek(), Some(KrlToken::AbstractUnit));
        self.advance(); // consume Unit/AbstractUnit/IndividualUnit

        let name = self.expect(KrlToken::Identifier)?;

        let mut parents = Vec::new();
        let mut slots = Vec::new();
        let mut perspectives = Vec::new();

        // Parse body
        if matches!(self.peek(), Some(KrlToken::LBrace)) {
            self.advance();

            while !matches!(self.peek(), Some(KrlToken::RBrace) | None) {
                match self.peek() {
                    Some(KrlToken::IsA) => {
                        self.advance();
                        parents.push(self.expect(KrlToken::Identifier)?);
                        if matches!(self.peek(), Some(KrlToken::Semicolon)) {
                            self.advance();
                        }
                    }
                    Some(KrlToken::Slot) | Some(KrlToken::HasSlot) => {
                        self.advance();
                        slots.push(self.parse_slot_def()?);
                    }
                    Some(KrlToken::Perspective) => {
                        self.advance();
                        perspectives.push(self.parse_perspective()?);
                    }
                    _ => {
                        self.advance(); // Skip unknown
                    }
                }
            }

            self.expect(KrlToken::RBrace)?;
        }

        Ok(KrlNode::Unit {
            name,
            is_abstract,
            parents,
            slots,
            perspectives,
        })
    }

    fn parse_slot_def(&mut self) -> Result<SlotDef, ParseError> {
        let name = self.expect(KrlToken::Identifier)?;
        let mut facets = Vec::new();

        if matches!(self.peek(), Some(KrlToken::LBrace)) {
            self.advance();

            while !matches!(self.peek(), Some(KrlToken::RBrace) | None) {
                match self.peek() {
                    Some(KrlToken::ValueClass) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let class = self.expect(KrlToken::Identifier)?;
                        facets.push(Facet::ValueClass(class));
                    }
                    Some(KrlToken::Value) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let val = self.parse_expr()?;
                        facets.push(Facet::Value(val));
                    }
                    Some(KrlToken::Default) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let val = self.parse_expr()?;
                        facets.push(Facet::Default(val));
                    }
                    Some(KrlToken::IfNeeded) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let body = self.parse_body()?;
                        facets.push(Facet::IfNeeded(body));
                    }
                    Some(KrlToken::IfAdded) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let body = self.parse_body()?;
                        facets.push(Facet::IfAdded(body));
                    }
                    Some(KrlToken::IfRemoved) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let body = self.parse_body()?;
                        facets.push(Facet::IfRemoved(body));
                    }
                    Some(KrlToken::Inverse) => {
                        self.advance();
                        self.expect(KrlToken::Colon)?;
                        let inv = self.expect(KrlToken::Identifier)?;
                        facets.push(Facet::Inverse(inv));
                    }
                    _ => {
                        self.advance();
                    }
                }

                if matches!(self.peek(), Some(KrlToken::Comma) | Some(KrlToken::Semicolon)) {
                    self.advance();
                }
            }

            self.expect(KrlToken::RBrace)?;
        }

        Ok(SlotDef { name, facets })
    }

    fn parse_perspective(&mut self) -> Result<PerspectiveDef, ParseError> {
        let name = self.expect(KrlToken::Identifier)?;
        let mut slot_mappings = Vec::new();

        if matches!(self.peek(), Some(KrlToken::LBrace)) {
            self.advance();

            while !matches!(self.peek(), Some(KrlToken::RBrace) | None) {
                let from = self.expect(KrlToken::Identifier)?;
                self.expect(KrlToken::Arrow)?;
                let to = self.expect(KrlToken::Identifier)?;
                slot_mappings.push((from, to));

                if matches!(self.peek(), Some(KrlToken::Comma) | Some(KrlToken::Semicolon)) {
                    self.advance();
                }
            }

            self.expect(KrlToken::RBrace)?;
        }

        Ok(PerspectiveDef { name, slot_mappings })
    }

    fn parse_body(&mut self) -> Result<Vec<KrlExpr>, ParseError> {
        let mut exprs = Vec::new();

        if matches!(self.peek(), Some(KrlToken::LBrace)) {
            self.advance();
            while !matches!(self.peek(), Some(KrlToken::RBrace) | None) {
                exprs.push(self.parse_expr()?);
                if matches!(self.peek(), Some(KrlToken::Semicolon)) {
                    self.advance();
                }
            }
            self.expect(KrlToken::RBrace)?;
        } else {
            exprs.push(self.parse_expr()?);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<KrlExpr, ParseError> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<KrlExpr, ParseError> {
        let mut left = self.parse_and_expr()?;

        while matches!(self.peek(), Some(KrlToken::Or)) {
            self.advance();
            let right = self.parse_and_expr()?;
            left = KrlExpr::BinaryOp {
                op: "or".into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<KrlExpr, ParseError> {
        let mut left = self.parse_comparison()?;

        while matches!(self.peek(), Some(KrlToken::And)) {
            self.advance();
            let right = self.parse_comparison()?;
            left = KrlExpr::BinaryOp {
                op: "and".into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<KrlExpr, ParseError> {
        let left = self.parse_additive()?;

        let op = match self.peek() {
            Some(KrlToken::Equal) => "==",
            Some(KrlToken::NotEqual) => "!=",
            Some(KrlToken::LessThan) => "<",
            Some(KrlToken::GreaterThan) => ">",
            Some(KrlToken::LessEqual) => "<=",
            Some(KrlToken::GreaterEqual) => ">=",
            _ => return Ok(left),
        };

        self.advance();
        let right = self.parse_additive()?;

        Ok(KrlExpr::BinaryOp {
            op: op.into(),
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_additive(&mut self) -> Result<KrlExpr, ParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek() {
                Some(KrlToken::Plus) => "+",
                Some(KrlToken::Minus) => "-",
                _ => break,
            };

            self.advance();
            let right = self.parse_multiplicative()?;
            left = KrlExpr::BinaryOp {
                op: op.into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<KrlExpr, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.peek() {
                Some(KrlToken::Star) => "*",
                Some(KrlToken::Slash) => "/",
                _ => break,
            };

            self.advance();
            let right = self.parse_unary()?;
            left = KrlExpr::BinaryOp {
                op: op.into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<KrlExpr, ParseError> {
        if matches!(self.peek(), Some(KrlToken::Not)) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(KrlExpr::UnaryOp {
                op: "not".into(),
                operand: Box::new(operand),
            });
        }

        if matches!(self.peek(), Some(KrlToken::Minus)) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(KrlExpr::UnaryOp {
                op: "-".into(),
                operand: Box::new(operand),
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<KrlExpr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if matches!(self.peek(), Some(KrlToken::Dot)) {
                self.advance();
                let slot = self.expect(KrlToken::Identifier)?;
                expr = KrlExpr::SlotAccess {
                    object: Box::new(expr),
                    slot,
                };
            } else if matches!(self.peek(), Some(KrlToken::LParen)) {
                // Function call
                self.advance();
                let mut args = Vec::new();
                while !matches!(self.peek(), Some(KrlToken::RParen) | None) {
                    args.push(self.parse_expr()?);
                    if matches!(self.peek(), Some(KrlToken::Comma)) {
                        self.advance();
                    }
                }
                self.expect(KrlToken::RParen)?;

                if let KrlExpr::Identifier(name) = expr {
                    expr = KrlExpr::Call { name, args };
                } else {
                    expr = KrlExpr::Call {
                        name: "apply".into(),
                        args: std::iter::once(expr).chain(args).collect(),
                    };
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<KrlExpr, ParseError> {
        match self.peek() {
            Some(KrlToken::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(KrlExpr::Number(text.parse().unwrap_or(0.0)))
            }
            Some(KrlToken::String) => {
                let (_, text) = self.advance().unwrap();
                Ok(KrlExpr::String(text[1..text.len()-1].to_string()))
            }
            Some(KrlToken::Identifier) => {
                let (_, text) = self.advance().unwrap();
                Ok(KrlExpr::Identifier(text))
            }
            Some(KrlToken::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(KrlToken::RParen)?;
                Ok(expr)
            }
            Some(KrlToken::LBracket) => {
                self.advance();
                let mut items = Vec::new();
                while !matches!(self.peek(), Some(KrlToken::RBracket) | None) {
                    items.push(self.parse_expr()?);
                    if matches!(self.peek(), Some(KrlToken::Comma)) {
                        self.advance();
                    }
                }
                self.expect(KrlToken::RBracket)?;
                Ok(KrlExpr::List(items))
            }
            Some(KrlToken::Create) => {
                self.advance();
                let unit = self.expect(KrlToken::Identifier)?;
                let mut slot_values = Vec::new();

                if matches!(self.peek(), Some(KrlToken::LBrace)) {
                    self.advance();
                    while !matches!(self.peek(), Some(KrlToken::RBrace) | None) {
                        let slot = self.expect(KrlToken::Identifier)?;
                        self.expect(KrlToken::Colon)?;
                        let value = self.parse_expr()?;
                        slot_values.push((slot, value));
                        if matches!(self.peek(), Some(KrlToken::Comma)) {
                            self.advance();
                        }
                    }
                    self.expect(KrlToken::RBrace)?;
                }

                Ok(KrlExpr::Create { unit, slot_values })
            }
            Some(KrlToken::If) => {
                self.advance();
                let condition = Box::new(self.parse_expr()?);
                self.expect(KrlToken::Then)?;
                let then_branch = self.parse_body()?;
                let else_branch = if matches!(self.peek(), Some(KrlToken::Else)) {
                    self.advance();
                    Some(self.parse_body()?)
                } else {
                    None
                };
                Ok(KrlExpr::If { condition, then_branch, else_branch })
            }
            Some(KrlToken::ForAll) => {
                self.advance();
                let variable = self.expect(KrlToken::Identifier)?;
                // Expect 'in' or similar - simplified
                let _ = self.advance();
                let collection = Box::new(self.parse_expr()?);
                self.expect(KrlToken::Do)?;
                let body = self.parse_body()?;
                Ok(KrlExpr::ForAll { variable, collection, body })
            }
            Some(KrlToken::Let) => {
                self.advance();
                let mut bindings = Vec::new();
                while matches!(self.peek(), Some(KrlToken::Identifier)) {
                    let var = self.expect(KrlToken::Identifier)?;
                    self.expect(KrlToken::Equal)?;
                    let val = self.parse_expr()?;
                    bindings.push((var, val));
                    if matches!(self.peek(), Some(KrlToken::Comma)) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let body = self.parse_body()?;
                Ok(KrlExpr::Let { bindings, body })
            }
            _ => Ok(KrlExpr::Nil),
        }
    }
}

/// KRL to IR converter
pub struct KrlToIr {
    builder: IrBuilder,
    units: HashMap<String, Vec<String>>, // unit -> parent units
}

impl KrlToIr {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new("krl_module"),
            units: HashMap::new(),
        }
    }

    pub fn convert(&mut self, nodes: &[KrlNode]) -> Result<IrModule, ParseError> {
        // First pass: collect unit definitions
        for node in nodes {
            if let KrlNode::Unit { name, parents, .. } = node {
                self.units.insert(name.clone(), parents.clone());
            }
        }

        // Second pass: generate code
        for node in nodes {
            self.convert_node(node)?;
        }

        Ok(self.builder.build_clone())
    }

    fn convert_node(&mut self, node: &KrlNode) -> Result<(), ParseError> {
        match node {
            KrlNode::Unit { name, is_abstract, parents, slots, perspectives: _ } => {
                // Generate struct
                let mut fields = Vec::new();
                for slot in slots {
                    fields.push((slot.name.clone(), IrType::Any));
                }

                // Add parent fields via composition
                for parent in parents {
                    fields.push((format!("_{}", parent.to_lowercase()), IrType::Struct(parent.clone())));
                }

                self.builder.add_struct(name, &fields);

                // Generate constructor
                let func_name = format!("new_{}", name.to_lowercase());
                self.builder.begin_function(&func_name, &[], IrType::Struct(name.clone()));

                // Initialize slots with defaults
                let mut inits = Vec::new();
                for slot in slots {
                    let default_val = self.get_default_value(slot);
                    inits.push((slot.name.clone(), default_val));
                }

                self.builder.add_return(IrExpr::StructInit {
                    name: name.clone(),
                    fields: inits,
                });

                self.builder.end_function();

                // Generate slot accessors with demons
                for slot in slots {
                    self.generate_slot_accessors(name, slot, *is_abstract)?;
                }

                Ok(())
            }
            KrlNode::Expr(expr) => {
                let ir = self.convert_expr(expr)?;
                self.builder.add_global_statement(ir);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn get_default_value(&self, slot: &SlotDef) -> IrExpr {
        for facet in &slot.facets {
            if let Facet::Default(expr) | Facet::Value(expr) = facet {
                if let Ok(ir) = self.convert_expr(expr) {
                    return ir;
                }
            }
        }
        IrExpr::Nil
    }

    fn generate_slot_accessors(
        &mut self,
        unit_name: &str,
        slot: &SlotDef,
        _is_abstract: bool,
    ) -> Result<(), ParseError> {
        // Getter
        let getter_name = format!("{}_{}_get", unit_name.to_lowercase(), slot.name);
        self.builder.begin_function(
            &getter_name,
            &[("self".into(), IrType::Struct(unit_name.into()))],
            IrType::Any,
        );

        // Check for IfNeeded demon
        let mut has_if_needed = false;
        for facet in &slot.facets {
            if let Facet::IfNeeded(body) = facet {
                has_if_needed = true;
                let body_ir: Result<Vec<_>, _> = body.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                if let Ok(exprs) = body_ir {
                    for expr in exprs {
                        self.builder.add_statement(expr);
                    }
                }
            }
        }

        if !has_if_needed {
            self.builder.add_return(IrExpr::FieldAccess {
                object: Box::new(IrExpr::Identifier("self".into())),
                field: slot.name.clone(),
            });
        }

        self.builder.end_function();

        // Setter
        let setter_name = format!("{}_{}_set", unit_name.to_lowercase(), slot.name);
        self.builder.begin_function(
            &setter_name,
            &[
                ("self".into(), IrType::MutRef(Box::new(IrType::Struct(unit_name.into())))),
                ("value".into(), IrType::Any),
            ],
            IrType::Unit,
        );

        // Execute IfAdded demon
        for facet in &slot.facets {
            if let Facet::IfAdded(body) = facet {
                let body_ir: Result<Vec<_>, _> = body.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                if let Ok(exprs) = body_ir {
                    for expr in exprs {
                        self.builder.add_statement(expr);
                    }
                }
            }
        }

        self.builder.add_statement(IrExpr::FieldAssign {
            object: "self".into(),
            field: slot.name.clone(),
            value: Box::new(IrExpr::Identifier("value".into())),
        });

        self.builder.end_function();

        Ok(())
    }

    fn convert_expr(&self, expr: &KrlExpr) -> Result<IrExpr, ParseError> {
        match expr {
            KrlExpr::Identifier(s) => Ok(IrExpr::Identifier(s.clone())),
            KrlExpr::Number(n) => Ok(IrExpr::Float(*n)),
            KrlExpr::String(s) => Ok(IrExpr::String(s.clone())),
            KrlExpr::Nil => Ok(IrExpr::Nil),
            KrlExpr::SlotAccess { object, slot } => {
                let obj = self.convert_expr(object)?;
                Ok(IrExpr::FieldAccess {
                    object: Box::new(obj),
                    field: slot.clone(),
                })
            }
            KrlExpr::BinaryOp { op, left, right } => {
                let l = self.convert_expr(left)?;
                let r = self.convert_expr(right)?;
                Ok(IrExpr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(l),
                    right: Box::new(r),
                })
            }
            KrlExpr::UnaryOp { op, operand } => {
                let o = self.convert_expr(operand)?;
                Ok(IrExpr::UnaryOp {
                    op: op.clone(),
                    operand: Box::new(o),
                })
            }
            KrlExpr::Call { name, args } => {
                let arg_irs: Result<Vec<_>, _> = args.iter()
                    .map(|a| self.convert_expr(a))
                    .collect();
                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier(name.clone())),
                    args: arg_irs?,
                })
            }
            KrlExpr::If { condition, then_branch, else_branch } => {
                let cond = self.convert_expr(condition)?;
                let then_ir: Result<Vec<_>, _> = then_branch.iter()
                    .map(|e| self.convert_expr(e))
                    .collect();
                let else_ir: Option<Result<Vec<_>, _>> = else_branch.as_ref()
                    .map(|b| b.iter().map(|e| self.convert_expr(e)).collect());

                Ok(IrExpr::If {
                    condition: Box::new(cond),
                    then_branch: Box::new(IrExpr::Block(then_ir?)),
                    else_branch: else_ir.map(|r| Box::new(IrExpr::Block(r.unwrap_or_default()))),
                })
            }
            KrlExpr::List(items) => {
                let ir_items: Result<Vec<_>, _> = items.iter()
                    .map(|i| self.convert_expr(i))
                    .collect();
                Ok(IrExpr::List(ir_items?))
            }
            KrlExpr::Create { unit, slot_values } => {
                let fields: Result<Vec<_>, _> = slot_values.iter()
                    .map(|(name, val)| Ok((name.clone(), self.convert_expr(val)?)))
                    .collect();
                Ok(IrExpr::StructInit {
                    name: unit.clone(),
                    fields: fields?,
                })
            }
            _ => Ok(IrExpr::Comment(format!("Unhandled: {:?}", expr))),
        }
    }
}

impl Default for KrlToIr {
    fn default() -> Self {
        Self::new()
    }
}

/// KRL Frontend implementation
pub struct KrlFrontend;

impl Frontend for KrlFrontend {
    fn name(&self) -> &'static str {
        "KRL"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["krl"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut parser = KrlParser::new(&source.content);
        let ast = parser.parse()?;
        let mut converter = KrlToIr::new();
        let module = converter.convert(&ast)?;
        Ok(module.into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_krl() {
        let source = "Unit Person { isA Agent; Slot name { ValueClass: String } }";
        let mut lexer = KrlToken::lexer(source);
        assert_eq!(lexer.next(), Some(Ok(KrlToken::Unit)));
        assert_eq!(lexer.next(), Some(Ok(KrlToken::Identifier)));
    }

    #[test]
    fn test_parse_unit() {
        let source = r#"
            Unit Employee {
                isA Person;
                Slot salary { ValueClass: Number; Default: 50000 }
                Slot manager { ValueClass: Employee }
            }
        "#;
        let mut parser = KrlParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }
}
