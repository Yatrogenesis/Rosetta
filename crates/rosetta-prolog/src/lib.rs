//! # Rosetta Prolog Frontend
//!
//! Prolog (1972, Alain Colmerauer & Philippe Roussel) is the canonical
//! logic programming language.
//!
//! Key concepts:
//! - Horn clauses (facts and rules)
//! - Unification
//! - Backtracking search
//! - Cut (!) for control
//! - DCG (Definite Clause Grammars)
//!
//! This crate transpiles Prolog to Rust using iterator-based search
//! and pattern matching.

use logos::Logos;
use rosetta_core::{Frontend, SourceFile, ParseError, RosettaIr};
use rosetta_ir::{IrModule, IrBuilder, IrType, IrExpr};
use std::collections::{HashMap, HashSet};

/// Prolog tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
pub enum PrologToken {
    // Keywords
    #[token(":-")]
    Implies,
    #[token("?-")]
    Query,
    #[token("-->")]
    DcgArrow,
    #[token("->")]
    IfThen,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token("!")]
    Cut,
    #[token("\\+")]
    Negation,
    #[token("not")]
    Not,
    #[token("is")]
    Is,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("fail")]
    Fail,

    // Comparison
    #[token("=")]
    Unify,
    #[token("\\=")]
    NotUnify,
    #[token("==")]
    StructEqual,
    #[token("\\==")]
    NotStructEqual,
    #[token("=..")]
    Univ,
    #[token("=:=")]
    ArithEqual,
    #[token("=\\=")]
    ArithNotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("=<")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("@<")]
    TermLess,
    #[token("@>")]
    TermGreater,

    // Arithmetic
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("//")]
    IntDiv,
    #[token("mod")]
    Mod,
    #[token("^")]
    Power,

    // List
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("|")]
    Pipe,

    // Grouping
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // Variables (start with uppercase or _)
    #[regex(r"[A-Z_][a-zA-Z0-9_]*")]
    Variable,

    // Atoms (lowercase start or quoted)
    #[regex(r"[a-z][a-zA-Z0-9_]*")]
    Atom,
    #[regex(r"'[^']*'")]
    QuotedAtom,

    // Numbers
    #[regex(r"-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    Number,

    // Strings
    #[regex(r#""[^"]*""#)]
    String,

    // Comments
    #[regex(r"%[^\n]*")]
    LineComment,
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    BlockComment,
}

/// Prolog AST nodes
#[derive(Debug, Clone)]
pub enum PrologClause {
    /// Fact: head.
    Fact(Term),
    /// Rule: head :- body.
    Rule {
        head: Term,
        body: Vec<Goal>,
    },
    /// Query: ?- goals.
    Query(Vec<Goal>),
    /// DCG rule: head --> body.
    DcgRule {
        head: Term,
        body: Vec<DcgBody>,
    },
    /// Directive: :- goals.
    Directive(Vec<Goal>),
}

/// Prolog term
#[derive(Debug, Clone)]
pub enum Term {
    /// Atom
    Atom(String),
    /// Variable
    Variable(String),
    /// Anonymous variable
    Anonymous,
    /// Number
    Number(f64),
    /// String
    String(String),
    /// Compound term: functor(args)
    Compound {
        functor: String,
        args: Vec<Term>,
    },
    /// List
    List {
        elements: Vec<Term>,
        tail: Option<Box<Term>>,
    },
    /// Empty list
    EmptyList,
}

/// Goal (body element)
#[derive(Debug, Clone)]
pub enum Goal {
    /// Simple goal (predicate call)
    Call(Term),
    /// Conjunction (,)
    Conjunction(Vec<Goal>),
    /// Disjunction (;)
    Disjunction(Vec<Goal>),
    /// If-then-else (Cond -> Then ; Else)
    IfThenElse {
        condition: Box<Goal>,
        then_branch: Box<Goal>,
        else_branch: Option<Box<Goal>>,
    },
    /// Negation as failure (\+)
    Negation(Box<Goal>),
    /// Cut (!)
    Cut,
    /// True
    True,
    /// Fail
    Fail,
    /// Unification (=)
    Unify(Term, Term),
    /// Arithmetic is
    Is(Term, ArithExpr),
    /// Comparison
    Compare {
        op: String,
        left: Term,
        right: Term,
    },
}

/// DCG body element
#[derive(Debug, Clone)]
pub enum DcgBody {
    /// Terminal
    Terminal(Vec<Term>),
    /// Non-terminal
    NonTerminal(Term),
    /// Push-back notation
    PushBack(Vec<Term>),
    /// Prolog goal in braces
    Goal(Goal),
}

/// Arithmetic expression
#[derive(Debug, Clone)]
pub enum ArithExpr {
    Number(f64),
    Variable(String),
    BinaryOp {
        op: String,
        left: Box<ArithExpr>,
        right: Box<ArithExpr>,
    },
    UnaryOp {
        op: String,
        operand: Box<ArithExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<ArithExpr>,
    },
}

/// Prolog parser
pub struct PrologParser {
    tokens: Vec<(PrologToken, String)>,
    position: usize,
}

impl PrologParser {
    pub fn new(source: &str) -> Self {
        let lexer = PrologToken::lexer(source);
        let tokens: Vec<_> = lexer
            .spanned()
            .filter_map(|(tok, span)| {
                tok.ok().map(|t| (t, source[span].to_string()))
            })
            .filter(|(t, _)| !matches!(t, PrologToken::LineComment | PrologToken::BlockComment))
            .collect();

        Self { tokens, position: 0 }
    }

    fn peek(&self) -> Option<&PrologToken> {
        self.tokens.get(self.position).map(|(t, _)| t)
    }

    fn peek_text(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|(_, t)| t.as_str())
    }

    fn advance(&mut self) -> Option<(PrologToken, String)> {
        if self.position < self.tokens.len() {
            let result = self.tokens[self.position].clone();
            self.position += 1;
            Some(result)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: PrologToken) -> Result<String, ParseError> {
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

    pub fn parse(&mut self) -> Result<Vec<PrologClause>, ParseError> {
        let mut clauses = Vec::new();
        while self.peek().is_some() {
            clauses.push(self.parse_clause()?);
        }
        Ok(clauses)
    }

    fn parse_clause(&mut self) -> Result<PrologClause, ParseError> {
        // Check for query or directive
        if matches!(self.peek(), Some(PrologToken::Query)) {
            self.advance();
            let goals = self.parse_goals()?;
            self.expect(PrologToken::Period)?;
            return Ok(PrologClause::Query(goals));
        }

        if matches!(self.peek(), Some(PrologToken::Implies)) {
            self.advance();
            let goals = self.parse_goals()?;
            self.expect(PrologToken::Period)?;
            return Ok(PrologClause::Directive(goals));
        }

        // Parse head
        let head = self.parse_term()?;

        // Check for rule, DCG, or fact
        match self.peek() {
            Some(PrologToken::Implies) => {
                self.advance();
                let body = self.parse_goals()?;
                self.expect(PrologToken::Period)?;
                Ok(PrologClause::Rule { head, body })
            }
            Some(PrologToken::DcgArrow) => {
                self.advance();
                let body = self.parse_dcg_body()?;
                self.expect(PrologToken::Period)?;
                Ok(PrologClause::DcgRule { head, body })
            }
            Some(PrologToken::Period) => {
                self.advance();
                Ok(PrologClause::Fact(head))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: ":- or --> or .".into(),
                found: format!("{:?}", self.peek()),
                line: 0,
                column: 0,
            }),
        }
    }

    fn parse_goals(&mut self) -> Result<Vec<Goal>, ParseError> {
        let mut goals = vec![self.parse_goal()?];

        while matches!(self.peek(), Some(PrologToken::Comma)) {
            self.advance();
            goals.push(self.parse_goal()?);
        }

        Ok(goals)
    }

    fn parse_goal(&mut self) -> Result<Goal, ParseError> {
        self.parse_disjunction()
    }

    fn parse_disjunction(&mut self) -> Result<Goal, ParseError> {
        let first = self.parse_if_then_else()?;

        if matches!(self.peek(), Some(PrologToken::Semicolon)) {
            let mut alternatives = vec![first];
            while matches!(self.peek(), Some(PrologToken::Semicolon)) {
                self.advance();
                alternatives.push(self.parse_if_then_else()?);
            }
            Ok(Goal::Disjunction(alternatives))
        } else {
            Ok(first)
        }
    }

    fn parse_if_then_else(&mut self) -> Result<Goal, ParseError> {
        let first = self.parse_conjunction()?;

        if matches!(self.peek(), Some(PrologToken::IfThen)) {
            self.advance();
            let then_branch = Box::new(self.parse_conjunction()?);
            let else_branch = if matches!(self.peek(), Some(PrologToken::Semicolon)) {
                self.advance();
                Some(Box::new(self.parse_conjunction()?))
            } else {
                None
            };
            Ok(Goal::IfThenElse {
                condition: Box::new(first),
                then_branch,
                else_branch,
            })
        } else {
            Ok(first)
        }
    }

    fn parse_conjunction(&mut self) -> Result<Goal, ParseError> {
        self.parse_primary_goal()
    }

    fn parse_primary_goal(&mut self) -> Result<Goal, ParseError> {
        match self.peek() {
            Some(PrologToken::Cut) => {
                self.advance();
                Ok(Goal::Cut)
            }
            Some(PrologToken::True) => {
                self.advance();
                Ok(Goal::True)
            }
            Some(PrologToken::Fail | PrologToken::False) => {
                self.advance();
                Ok(Goal::Fail)
            }
            Some(PrologToken::Negation | PrologToken::Not) => {
                self.advance();
                let goal = self.parse_primary_goal()?;
                Ok(Goal::Negation(Box::new(goal)))
            }
            Some(PrologToken::LParen) => {
                self.advance();
                let goal = self.parse_goal()?;
                self.expect(PrologToken::RParen)?;
                Ok(goal)
            }
            _ => {
                let term = self.parse_term()?;

                // Check for unification or comparison
                match self.peek() {
                    Some(PrologToken::Unify) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Unify(term, right))
                    }
                    Some(PrologToken::Is) => {
                        self.advance();
                        let expr = self.parse_arith_expr()?;
                        Ok(Goal::Is(term, expr))
                    }
                    Some(PrologToken::ArithEqual) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Compare { op: "=:=".into(), left: term, right })
                    }
                    Some(PrologToken::LessThan) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Compare { op: "<".into(), left: term, right })
                    }
                    Some(PrologToken::GreaterThan) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Compare { op: ">".into(), left: term, right })
                    }
                    Some(PrologToken::LessEqual) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Compare { op: "=<".into(), left: term, right })
                    }
                    Some(PrologToken::GreaterEqual) => {
                        self.advance();
                        let right = self.parse_term()?;
                        Ok(Goal::Compare { op: ">=".into(), left: term, right })
                    }
                    _ => Ok(Goal::Call(term)),
                }
            }
        }
    }

    fn parse_term(&mut self) -> Result<Term, ParseError> {
        match self.peek() {
            Some(PrologToken::Variable) => {
                let (_, text) = self.advance().unwrap();
                if text == "_" {
                    Ok(Term::Anonymous)
                } else {
                    Ok(Term::Variable(text))
                }
            }
            Some(PrologToken::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(Term::Number(text.parse().unwrap_or(0.0)))
            }
            Some(PrologToken::String) => {
                let (_, text) = self.advance().unwrap();
                Ok(Term::String(text[1..text.len()-1].to_string()))
            }
            Some(PrologToken::Atom | PrologToken::QuotedAtom) => {
                let (_, text) = self.advance().unwrap();
                let atom = if text.starts_with('\'') {
                    text[1..text.len()-1].to_string()
                } else {
                    text
                };

                // Check for compound term
                if matches!(self.peek(), Some(PrologToken::LParen)) {
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(PrologToken::RParen)) {
                        args.push(self.parse_term()?);
                        while matches!(self.peek(), Some(PrologToken::Comma)) {
                            self.advance();
                            args.push(self.parse_term()?);
                        }
                    }
                    self.expect(PrologToken::RParen)?;
                    Ok(Term::Compound { functor: atom, args })
                } else {
                    Ok(Term::Atom(atom))
                }
            }
            Some(PrologToken::LBracket) => {
                self.advance();
                self.parse_list()
            }
            Some(PrologToken::LParen) => {
                self.advance();
                let term = self.parse_term()?;
                self.expect(PrologToken::RParen)?;
                Ok(term)
            }
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Term, ParseError> {
        if matches!(self.peek(), Some(PrologToken::RBracket)) {
            self.advance();
            return Ok(Term::EmptyList);
        }

        let mut elements = vec![self.parse_term()?];

        while matches!(self.peek(), Some(PrologToken::Comma)) {
            self.advance();
            elements.push(self.parse_term()?);
        }

        let tail = if matches!(self.peek(), Some(PrologToken::Pipe)) {
            self.advance();
            Some(Box::new(self.parse_term()?))
        } else {
            None
        };

        self.expect(PrologToken::RBracket)?;

        Ok(Term::List { elements, tail })
    }

    fn parse_dcg_body(&mut self) -> Result<Vec<DcgBody>, ParseError> {
        let mut body = vec![self.parse_dcg_element()?];

        while matches!(self.peek(), Some(PrologToken::Comma)) {
            self.advance();
            body.push(self.parse_dcg_element()?);
        }

        Ok(body)
    }

    fn parse_dcg_element(&mut self) -> Result<DcgBody, ParseError> {
        match self.peek() {
            Some(PrologToken::LBracket) => {
                self.advance();
                let mut terms = Vec::new();
                if !matches!(self.peek(), Some(PrologToken::RBracket)) {
                    terms.push(self.parse_term()?);
                    while matches!(self.peek(), Some(PrologToken::Comma)) {
                        self.advance();
                        terms.push(self.parse_term()?);
                    }
                }
                self.expect(PrologToken::RBracket)?;
                Ok(DcgBody::Terminal(terms))
            }
            Some(PrologToken::LBrace) => {
                self.advance();
                let goal = self.parse_goal()?;
                self.expect(PrologToken::RBrace)?;
                Ok(DcgBody::Goal(goal))
            }
            _ => {
                let term = self.parse_term()?;
                Ok(DcgBody::NonTerminal(term))
            }
        }
    }

    fn parse_arith_expr(&mut self) -> Result<ArithExpr, ParseError> {
        self.parse_arith_additive()
    }

    fn parse_arith_additive(&mut self) -> Result<ArithExpr, ParseError> {
        let mut left = self.parse_arith_multiplicative()?;

        loop {
            let op = match self.peek() {
                Some(PrologToken::Plus) => "+",
                Some(PrologToken::Minus) => "-",
                _ => break,
            };

            self.advance();
            let right = self.parse_arith_multiplicative()?;
            left = ArithExpr::BinaryOp {
                op: op.into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_arith_multiplicative(&mut self) -> Result<ArithExpr, ParseError> {
        let mut left = self.parse_arith_power()?;

        loop {
            let op = match self.peek() {
                Some(PrologToken::Star) => "*",
                Some(PrologToken::Slash) => "/",
                Some(PrologToken::IntDiv) => "//",
                Some(PrologToken::Mod) => "mod",
                _ => break,
            };

            self.advance();
            let right = self.parse_arith_power()?;
            left = ArithExpr::BinaryOp {
                op: op.into(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_arith_power(&mut self) -> Result<ArithExpr, ParseError> {
        let base = self.parse_arith_unary()?;

        if matches!(self.peek(), Some(PrologToken::Power)) {
            self.advance();
            let exp = self.parse_arith_power()?;
            Ok(ArithExpr::BinaryOp {
                op: "^".into(),
                left: Box::new(base),
                right: Box::new(exp),
            })
        } else {
            Ok(base)
        }
    }

    fn parse_arith_unary(&mut self) -> Result<ArithExpr, ParseError> {
        if matches!(self.peek(), Some(PrologToken::Minus)) {
            self.advance();
            let operand = self.parse_arith_unary()?;
            return Ok(ArithExpr::UnaryOp {
                op: "-".into(),
                operand: Box::new(operand),
            });
        }

        self.parse_arith_primary()
    }

    fn parse_arith_primary(&mut self) -> Result<ArithExpr, ParseError> {
        match self.peek() {
            Some(PrologToken::Number) => {
                let (_, text) = self.advance().unwrap();
                Ok(ArithExpr::Number(text.parse().unwrap_or(0.0)))
            }
            Some(PrologToken::Variable) => {
                let (_, text) = self.advance().unwrap();
                Ok(ArithExpr::Variable(text))
            }
            Some(PrologToken::Atom) => {
                let (_, name) = self.advance().unwrap();
                if matches!(self.peek(), Some(PrologToken::LParen)) {
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(PrologToken::RParen)) {
                        args.push(self.parse_arith_expr()?);
                        while matches!(self.peek(), Some(PrologToken::Comma)) {
                            self.advance();
                            args.push(self.parse_arith_expr()?);
                        }
                    }
                    self.expect(PrologToken::RParen)?;
                    Ok(ArithExpr::FunctionCall { name, args })
                } else {
                    Ok(ArithExpr::Variable(name))
                }
            }
            Some(PrologToken::LParen) => {
                self.advance();
                let expr = self.parse_arith_expr()?;
                self.expect(PrologToken::RParen)?;
                Ok(expr)
            }
            _ => Ok(ArithExpr::Number(0.0)),
        }
    }
}

/// Prolog to IR converter
pub struct PrologToIr {
    builder: IrBuilder,
    predicates: HashMap<String, Vec<(Vec<Term>, Vec<Goal>)>>, // predicate -> clauses
}

impl PrologToIr {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new("prolog_module"),
            predicates: HashMap::new(),
        }
    }

    pub fn convert(&mut self, clauses: &[PrologClause]) -> Result<IrModule, ParseError> {
        // First pass: collect predicates
        for clause in clauses {
            match clause {
                PrologClause::Fact(head) => {
                    let (name, args) = self.extract_predicate(head);
                    self.predicates
                        .entry(name)
                        .or_default()
                        .push((args, vec![Goal::True]));
                }
                PrologClause::Rule { head, body } => {
                    let (name, args) = self.extract_predicate(head);
                    self.predicates
                        .entry(name)
                        .or_default()
                        .push((args, body.clone()));
                }
                _ => {}
            }
        }

        // Second pass: generate code - clone predicates to avoid borrow conflict
        let predicates: Vec<_> = self.predicates.iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        for (name, def_clauses) in predicates {
            self.generate_predicate(&name, &def_clauses)?;
        }

        Ok(self.builder.build_clone())
    }

    fn extract_predicate(&self, term: &Term) -> (String, Vec<Term>) {
        match term {
            Term::Atom(name) => (name.clone(), vec![]),
            Term::Compound { functor, args } => (functor.clone(), args.clone()),
            _ => ("unknown".into(), vec![]),
        }
    }

    fn generate_predicate(
        &mut self,
        name: &str,
        clauses: &[(Vec<Term>, Vec<Goal>)],
    ) -> Result<(), ParseError> {
        // Determine arity
        let arity = clauses.first().map(|(args, _)| args.len()).unwrap_or(0);

        // Generate parameter list
        let params: Vec<_> = (0..arity)
            .map(|i| (format!("arg{}", i), IrType::Any))
            .collect();

        let func_name = format!("{}_{}", name, arity);
        self.builder.begin_function(&func_name, &params, IrType::Iterator(Box::new(IrType::Any)));

        // Generate clause matching
        for (i, (args, body)) in clauses.iter().enumerate() {
            self.builder.add_statement(IrExpr::Comment(format!("Clause {}", i + 1)));

            // Generate pattern matching
            let vars = self.collect_variables(args);
            for (j, arg) in args.iter().enumerate() {
                let pattern = self.convert_term(arg)?;
                self.builder.add_statement(IrExpr::PatternMatch {
                    value: Box::new(IrExpr::Identifier(format!("arg{}", j))),
                    pattern: Box::new(pattern),
                });
            }

            // Generate body
            for goal in body {
                let ir = self.convert_goal(goal)?;
                self.builder.add_statement(ir);
            }
        }

        self.builder.end_function();
        Ok(())
    }

    fn collect_variables(&self, terms: &[Term]) -> HashSet<String> {
        let mut vars = HashSet::new();
        for term in terms {
            self.collect_vars_from_term(term, &mut vars);
        }
        vars
    }

    fn collect_vars_from_term(&self, term: &Term, vars: &mut HashSet<String>) {
        match term {
            Term::Variable(v) => { vars.insert(v.clone()); }
            Term::Compound { args, .. } => {
                for arg in args {
                    self.collect_vars_from_term(arg, vars);
                }
            }
            Term::List { elements, tail } => {
                for elem in elements {
                    self.collect_vars_from_term(elem, vars);
                }
                if let Some(t) = tail {
                    self.collect_vars_from_term(t, vars);
                }
            }
            _ => {}
        }
    }

    fn convert_term(&self, term: &Term) -> Result<IrExpr, ParseError> {
        match term {
            Term::Atom(s) => Ok(IrExpr::Symbol(s.clone())),
            Term::Variable(v) => Ok(IrExpr::Identifier(v.clone())),
            Term::Anonymous => Ok(IrExpr::Identifier("_".into())),
            Term::Number(n) => Ok(IrExpr::Float(*n)),
            Term::String(s) => Ok(IrExpr::String(s.clone())),
            Term::EmptyList => Ok(IrExpr::List(vec![])),
            Term::List { elements, tail } => {
                let ir_elems: Result<Vec<_>, _> = elements.iter()
                    .map(|e| self.convert_term(e))
                    .collect();
                let mut list = IrExpr::List(ir_elems?);
                if let Some(t) = tail {
                    let tail_ir = self.convert_term(t)?;
                    list = IrExpr::ListCons {
                        head: Box::new(list),
                        tail: Box::new(tail_ir),
                    };
                }
                Ok(list)
            }
            Term::Compound { functor, args } => {
                let ir_args: Result<Vec<_>, _> = args.iter()
                    .map(|a| self.convert_term(a))
                    .collect();
                Ok(IrExpr::StructInit {
                    name: functor.clone(),
                    fields: ir_args?.into_iter().enumerate()
                        .map(|(i, a)| (format!("_{}", i), a))
                        .collect(),
                })
            }
        }
    }

    fn convert_goal(&self, goal: &Goal) -> Result<IrExpr, ParseError> {
        match goal {
            Goal::True => Ok(IrExpr::Bool(true)),
            Goal::Fail => Ok(IrExpr::Bool(false)),
            Goal::Cut => Ok(IrExpr::Comment("!".into())), // Cut is control flow
            Goal::Call(term) => {
                let (name, args) = match term {
                    Term::Atom(n) => (n.clone(), vec![]),
                    Term::Compound { functor, args } => {
                        let ir_args: Result<Vec<_>, _> = args.iter()
                            .map(|a| self.convert_term(a))
                            .collect();
                        (functor.clone(), ir_args?)
                    }
                    _ => ("call".into(), vec![self.convert_term(term)?]),
                };

                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier(format!("{}_{}", name, args.len()))),
                    args,
                })
            }
            Goal::Conjunction(goals) => {
                let ir_goals: Result<Vec<_>, _> = goals.iter()
                    .map(|g| self.convert_goal(g))
                    .collect();
                Ok(IrExpr::Block(ir_goals?))
            }
            Goal::Disjunction(alts) => {
                let ir_alts: Result<Vec<_>, _> = alts.iter()
                    .map(|g| self.convert_goal(g))
                    .collect();
                Ok(IrExpr::Choice(ir_alts?))
            }
            Goal::Negation(g) => {
                let ir = self.convert_goal(g)?;
                Ok(IrExpr::UnaryOp {
                    op: "!".into(),
                    operand: Box::new(ir),
                })
            }
            Goal::Unify(left, right) => {
                let l = self.convert_term(left)?;
                let r = self.convert_term(right)?;
                Ok(IrExpr::Unify {
                    left: Box::new(l),
                    right: Box::new(r),
                })
            }
            Goal::Is(var, expr) => {
                let v = self.convert_term(var)?;
                let e = self.convert_arith(expr)?;
                Ok(IrExpr::Assign {
                    target: match v {
                        IrExpr::Identifier(s) => s,
                        _ => "result".into(),
                    },
                    value: Box::new(e),
                })
            }
            Goal::Compare { op, left, right } => {
                let l = self.convert_term(left)?;
                let r = self.convert_term(right)?;
                Ok(IrExpr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(l),
                    right: Box::new(r),
                })
            }
            Goal::IfThenElse { condition, then_branch, else_branch } => {
                let cond = self.convert_goal(condition)?;
                let then_ir = self.convert_goal(then_branch)?;
                let else_ir = else_branch.as_ref()
                    .map(|e| self.convert_goal(e))
                    .transpose()?;
                Ok(IrExpr::If {
                    condition: Box::new(cond),
                    then_branch: Box::new(then_ir),
                    else_branch: else_ir.map(Box::new),
                })
            }
        }
    }

    fn convert_arith(&self, expr: &ArithExpr) -> Result<IrExpr, ParseError> {
        match expr {
            ArithExpr::Number(n) => Ok(IrExpr::Float(*n)),
            ArithExpr::Variable(v) => Ok(IrExpr::Identifier(v.clone())),
            ArithExpr::BinaryOp { op, left, right } => {
                let l = self.convert_arith(left)?;
                let r = self.convert_arith(right)?;
                Ok(IrExpr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(l),
                    right: Box::new(r),
                })
            }
            ArithExpr::UnaryOp { op, operand } => {
                let o = self.convert_arith(operand)?;
                Ok(IrExpr::UnaryOp {
                    op: op.clone(),
                    operand: Box::new(o),
                })
            }
            ArithExpr::FunctionCall { name, args } => {
                let ir_args: Result<Vec<_>, _> = args.iter()
                    .map(|a| self.convert_arith(a))
                    .collect();
                Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Identifier(name.clone())),
                    args: ir_args?,
                })
            }
        }
    }
}

impl Default for PrologToIr {
    fn default() -> Self {
        Self::new()
    }
}

/// Prolog Frontend implementation
pub struct PrologFrontend;

impl Frontend for PrologFrontend {
    fn name(&self) -> &'static str {
        "Prolog"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["pl", "pro", "prolog"]
    }

    fn parse(&self, source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        let mut parser = PrologParser::new(&source.content);
        let ast = parser.parse()?;
        let mut converter = PrologToIr::new();
        let module = converter.convert(&ast)?;
        Ok(module.into_rosetta_ir())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_prolog() {
        let source = "parent(tom, mary).";
        let mut lexer = PrologToken::lexer(source);
        assert_eq!(lexer.next(), Some(Ok(PrologToken::Atom)));
    }

    #[test]
    fn test_parse_fact() {
        let source = "parent(tom, mary).";
        let mut parser = PrologParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_rule() {
        let source = "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).";
        let mut parser = PrologParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_list() {
        let source = "append([], L, L).";
        let mut parser = PrologParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_arithmetic() {
        let source = "factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.";
        let mut parser = PrologParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_dcg() {
        let source = "sentence --> noun_phrase, verb_phrase.";
        let mut parser = PrologParser::new(source);
        let result = parser.parse();
        assert!(result.is_ok());
    }
}
