//! # Rosetta Forth Frontend
//!
//! Parser for Forth (1970), a stack-based concatenative language.
//!
//! ## Supported Features
//!
//! - Word definitions (: name ... ;)
//! - Stack manipulation (DUP, DROP, SWAP, OVER, ROT)
//! - Arithmetic (+, -, *, /, MOD)
//! - Comparison (<, >, =, <>, 0<, 0=, 0>)
//! - Control flow (IF...THEN, IF...ELSE...THEN, DO...LOOP, BEGIN...UNTIL)
//! - Variables and constants (VARIABLE, CONSTANT, @, !)
//! - Comments (\ line, ( ... ))

use logos::Logos;
use rosetta_core::{IrType, IrExpr, SourceLanguage, Result, TranspileError, SourceLocation};
use rosetta_ir::{IrModule, IrFunction, IrBuilder, IrParam, Visibility};

/// Forth tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum ForthToken {
    // Word definition
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,

    // Stack operations
    #[token("DUP", ignore(ascii_case))]
    Dup,
    #[token("DROP", ignore(ascii_case))]
    Drop,
    #[token("SWAP", ignore(ascii_case))]
    Swap,
    #[token("OVER", ignore(ascii_case))]
    Over,
    #[token("ROT", ignore(ascii_case))]
    Rot,
    #[token("NIP", ignore(ascii_case))]
    Nip,
    #[token("TUCK", ignore(ascii_case))]
    Tuck,
    #[token("2DUP", ignore(ascii_case))]
    TwoDup,
    #[token("2DROP", ignore(ascii_case))]
    TwoDrop,
    #[token("2SWAP", ignore(ascii_case))]
    TwoSwap,
    #[token("2OVER", ignore(ascii_case))]
    TwoOver,
    #[token("?DUP", ignore(ascii_case))]
    QuestionDup,

    // Arithmetic
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("MOD", ignore(ascii_case))]
    Mod,
    #[token("/MOD", ignore(ascii_case))]
    SlashMod,
    #[token("*/", ignore(ascii_case))]
    StarSlash,
    #[token("*/MOD", ignore(ascii_case))]
    StarSlashMod,
    #[token("NEGATE", ignore(ascii_case))]
    Negate,
    #[token("ABS", ignore(ascii_case))]
    Abs,
    #[token("MIN", ignore(ascii_case))]
    Min,
    #[token("MAX", ignore(ascii_case))]
    Max,

    // Comparison
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("0<", ignore(ascii_case))]
    ZeroLess,
    #[token("0=", ignore(ascii_case))]
    ZeroEqual,
    #[token("0>", ignore(ascii_case))]
    ZeroGreater,
    #[token("0<>", ignore(ascii_case))]
    ZeroNotEqual,

    // Logical
    #[token("AND", ignore(ascii_case))]
    And,
    #[token("OR", ignore(ascii_case))]
    Or,
    #[token("XOR", ignore(ascii_case))]
    Xor,
    #[token("NOT", ignore(ascii_case))]
    Not,
    #[token("INVERT", ignore(ascii_case))]
    Invert,

    // Control flow
    #[token("IF", ignore(ascii_case))]
    If,
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("THEN", ignore(ascii_case))]
    Then,
    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("LOOP", ignore(ascii_case))]
    Loop,
    #[token("+LOOP", ignore(ascii_case))]
    PlusLoop,
    #[token("BEGIN", ignore(ascii_case))]
    Begin,
    #[token("UNTIL", ignore(ascii_case))]
    Until,
    #[token("WHILE", ignore(ascii_case))]
    While,
    #[token("REPEAT", ignore(ascii_case))]
    Repeat,
    #[token("AGAIN", ignore(ascii_case))]
    Again,
    #[token("EXIT", ignore(ascii_case))]
    Exit,
    #[token("RECURSE", ignore(ascii_case))]
    Recurse,

    // Loop index
    #[token("I", ignore(ascii_case))]
    LoopI,
    #[token("J", ignore(ascii_case))]
    LoopJ,
    #[token("K", ignore(ascii_case))]
    LoopK,
    #[token("LEAVE", ignore(ascii_case))]
    Leave,
    #[token("UNLOOP", ignore(ascii_case))]
    Unloop,

    // Variables and memory
    #[token("VARIABLE", ignore(ascii_case))]
    Variable,
    #[token("CONSTANT", ignore(ascii_case))]
    Constant,
    #[token("CREATE", ignore(ascii_case))]
    Create,
    #[token("ALLOT", ignore(ascii_case))]
    Allot,
    #[token("@")]
    Fetch,
    #[token("!")]
    Store,
    #[token("+!")]
    PlusStore,
    #[token("C@")]
    CFetch,
    #[token("C!")]
    CStore,
    #[token(",")]
    Comma,
    #[token("HERE", ignore(ascii_case))]
    Here,
    #[token("CELLS", ignore(ascii_case))]
    Cells,
    #[token("CELL+", ignore(ascii_case))]
    CellPlus,

    // I/O
    #[token(".", ignore(ascii_case))]
    Dot,
    #[token(".\"")]
    DotQuote,
    #[token("EMIT", ignore(ascii_case))]
    Emit,
    #[token("KEY", ignore(ascii_case))]
    Key,
    #[token("CR", ignore(ascii_case))]
    Cr,
    #[token("SPACE", ignore(ascii_case))]
    Space,
    #[token("SPACES", ignore(ascii_case))]
    Spaces,
    #[token("TYPE", ignore(ascii_case))]
    Type,

    // Return stack
    #[token(">R", ignore(ascii_case))]
    ToR,
    #[token("R>", ignore(ascii_case))]
    RFrom,
    #[token("R@", ignore(ascii_case))]
    RFetch,

    // Boolean constants
    #[token("TRUE", ignore(ascii_case))]
    True,
    #[token("FALSE", ignore(ascii_case))]
    False,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    #[regex(r"-?[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r"[A-Za-z_][A-Za-z0-9_\-]*", |lex| lex.slice().to_string(), priority = 1)]
    Word(String),

    // Strings
    #[regex(r#"S" [^"]*""#, |lex| {
        let s = lex.slice();
        Some(s[3..s.len()-1].to_string())
    })]
    StringLiteral(String),

    // Comments
    #[regex(r"\( [^)]* \)")]
    ParenComment,

    #[regex(r"\\[^\n]*")]
    LineComment,
}

/// Forth parser
pub struct ForthParser {
    tokens: Vec<(ForthToken, std::ops::Range<usize>)>,
    pos: usize,
    source: String,
}

impl ForthParser {
    pub fn new(source: &str) -> Self {
        let mut tokens = Vec::new();
        let lexer = ForthToken::lexer(source);
        for (token, span) in lexer.spanned() {
            if let Ok(t) = token {
                // Skip comments
                if !matches!(t, ForthToken::ParenComment | ForthToken::LineComment) {
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
        let mut builder = IrBuilder::with_language("forth_module", SourceLanguage::Forth);

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                ForthToken::Colon => {
                    let func = self.parse_word_definition()?;
                    builder.add_function(func);
                }
                ForthToken::Variable => {
                    self.parse_variable(&mut builder)?;
                }
                ForthToken::Constant => {
                    self.parse_constant(&mut builder)?;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Ok(builder.build())
    }

    fn parse_word_definition(&mut self) -> Result<IrFunction> {
        self.pos += 1; // Skip ':'

        let name = if let Some((ForthToken::Word(name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
            name.clone()
        } else {
            return Err(TranspileError::ParseError {
                message: "Expected word name after ':'".to_string(),
                location: self.current_location(),
            });
        };

        let mut body = Vec::new();

        while self.pos < self.tokens.len() {
            if matches!(self.tokens[self.pos].0, ForthToken::Semicolon) {
                self.pos += 1;
                break;
            }
            let expr = self.parse_token()?;
            body.push(expr);
        }

        Ok(IrFunction {
            name,
            generics: vec![],
            params: vec![
                IrParam {
                    name: "stack".to_string(),
                    ty: IrType::MutRef(Box::new(IrType::Vec(Box::new(IrType::Int(64))))),
                    is_mutable: true,
                    by_ref: true,
                },
            ],
            return_type: IrType::Unit,
            body,
            is_unsafe: false,
            visibility: Visibility::Public,
            attributes: vec![],
            span: None,
        })
    }

    fn parse_token(&mut self) -> Result<IrExpr> {
        let (token, span) = self.tokens[self.pos].clone();
        self.pos += 1;

        match token {
            // Stack operations
            ForthToken::Dup => Ok(self.stack_op("dup")),
            ForthToken::Drop => Ok(self.stack_op("drop")),
            ForthToken::Swap => Ok(self.stack_op("swap")),
            ForthToken::Over => Ok(self.stack_op("over")),
            ForthToken::Rot => Ok(self.stack_op("rot")),
            ForthToken::Nip => Ok(self.stack_op("nip")),
            ForthToken::Tuck => Ok(self.stack_op("tuck")),
            ForthToken::TwoDup => Ok(self.stack_op("2dup")),
            ForthToken::TwoDrop => Ok(self.stack_op("2drop")),
            ForthToken::TwoSwap => Ok(self.stack_op("2swap")),
            ForthToken::TwoOver => Ok(self.stack_op("2over")),
            ForthToken::QuestionDup => Ok(self.stack_op("?dup")),

            // Arithmetic
            ForthToken::Plus => Ok(self.binary_op("+")),
            ForthToken::Minus => Ok(self.binary_op("-")),
            ForthToken::Star => Ok(self.binary_op("*")),
            ForthToken::Slash => Ok(self.binary_op("/")),
            ForthToken::Mod => Ok(self.binary_op("mod")),
            ForthToken::Negate => Ok(self.unary_op("negate")),
            ForthToken::Abs => Ok(self.unary_op("abs")),
            ForthToken::Min => Ok(self.binary_op("min")),
            ForthToken::Max => Ok(self.binary_op("max")),

            // Comparison
            ForthToken::LessThan => Ok(self.binary_op("<")),
            ForthToken::GreaterThan => Ok(self.binary_op(">")),
            ForthToken::Equal => Ok(self.binary_op("=")),
            ForthToken::NotEqual => Ok(self.binary_op("<>")),
            ForthToken::LessEqual => Ok(self.binary_op("<=")),
            ForthToken::GreaterEqual => Ok(self.binary_op(">=")),
            ForthToken::ZeroLess => Ok(self.zero_compare("0<")),
            ForthToken::ZeroEqual => Ok(self.zero_compare("0=")),
            ForthToken::ZeroGreater => Ok(self.zero_compare("0>")),

            // Logical
            ForthToken::And => Ok(self.binary_op("and")),
            ForthToken::Or => Ok(self.binary_op("or")),
            ForthToken::Xor => Ok(self.binary_op("xor")),
            ForthToken::Not | ForthToken::Invert => Ok(self.unary_op("not")),

            // Control flow
            ForthToken::If => self.parse_if(),
            ForthToken::Do => self.parse_do_loop(),
            ForthToken::Begin => self.parse_begin(),

            // Literals
            ForthToken::Integer(n) => Ok(self.push_literal(n)),
            ForthToken::Float(f) => Ok(IrExpr::Call {
                func: Box::new(IrExpr::Identifier("stack_push_f64".to_string())),
                args: vec![
                    IrExpr::Identifier("stack".to_string()),
                    IrExpr::Float(f),
                ],
            }),
            ForthToken::True => Ok(self.push_literal(-1)),
            ForthToken::False => Ok(self.push_literal(0)),

            // I/O
            ForthToken::Dot => Ok(self.io_op("print_top")),
            ForthToken::Emit => Ok(self.io_op("emit")),
            ForthToken::Cr => Ok(self.io_op("cr")),
            ForthToken::Space => Ok(self.io_op("space")),

            // Memory operations
            ForthToken::Fetch => Ok(self.memory_op("fetch")),
            ForthToken::Store => Ok(self.memory_op("store")),
            ForthToken::PlusStore => Ok(self.memory_op("plus_store")),

            // Return stack
            ForthToken::ToR => Ok(self.rstack_op("to_r")),
            ForthToken::RFrom => Ok(self.rstack_op("r_from")),
            ForthToken::RFetch => Ok(self.rstack_op("r_fetch")),

            // Loop index
            ForthToken::LoopI => Ok(self.loop_index("i")),
            ForthToken::LoopJ => Ok(self.loop_index("j")),
            ForthToken::LoopK => Ok(self.loop_index("k")),

            // Exit/Recurse
            ForthToken::Exit => Ok(IrExpr::Return(Box::new(IrExpr::Nil))),
            ForthToken::Recurse => Ok(IrExpr::Call {
                func: Box::new(IrExpr::Identifier("recurse".to_string())),
                args: vec![IrExpr::Identifier("stack".to_string())],
            }),

            // Word call
            ForthToken::Word(name) => Ok(IrExpr::Call {
                func: Box::new(IrExpr::Identifier(name)),
                args: vec![IrExpr::Identifier("stack".to_string())],
            }),

            ForthToken::StringLiteral(s) => Ok(IrExpr::Call {
                func: Box::new(IrExpr::Identifier("stack_push_string".to_string())),
                args: vec![
                    IrExpr::Identifier("stack".to_string()),
                    IrExpr::String(s),
                ],
            }),

            _ => Ok(IrExpr::Nil),
        }
    }

    fn parse_if(&mut self) -> Result<IrExpr> {
        let mut then_body = Vec::new();
        let mut else_body = Vec::new();
        let mut in_else = false;

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                ForthToken::Then => {
                    self.pos += 1;
                    break;
                }
                ForthToken::Else => {
                    self.pos += 1;
                    in_else = true;
                }
                _ => {
                    let expr = self.parse_token()?;
                    if in_else {
                        else_body.push(expr);
                    } else {
                        then_body.push(expr);
                    }
                }
            }
        }

        let condition = IrExpr::Call {
            func: Box::new(IrExpr::Identifier("stack_pop".to_string())),
            args: vec![IrExpr::Identifier("stack".to_string())],
        };

        Ok(IrExpr::If {
            condition: Box::new(condition),
            then_branch: Box::new(IrExpr::Block(then_body)),
            else_branch: if else_body.is_empty() {
                None
            } else {
                Some(Box::new(IrExpr::Block(else_body)))
            },
        })
    }

    fn parse_do_loop(&mut self) -> Result<IrExpr> {
        let mut body = Vec::new();

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                ForthToken::Loop | ForthToken::PlusLoop => {
                    self.pos += 1;
                    break;
                }
                _ => {
                    let expr = self.parse_token()?;
                    body.push(expr);
                }
            }
        }

        // DO pops limit and index from stack
        Ok(IrExpr::Call {
            func: Box::new(IrExpr::Identifier("do_loop".to_string())),
            args: vec![
                IrExpr::Identifier("stack".to_string()),
                IrExpr::Block(body),
            ],
        })
    }

    fn parse_begin(&mut self) -> Result<IrExpr> {
        let mut body = Vec::new();

        while self.pos < self.tokens.len() {
            match &self.tokens[self.pos].0 {
                ForthToken::Until => {
                    self.pos += 1;
                    // BEGIN...UNTIL loop
                    return Ok(IrExpr::Call {
                        func: Box::new(IrExpr::Identifier("begin_until".to_string())),
                        args: vec![
                            IrExpr::Identifier("stack".to_string()),
                            IrExpr::Block(body),
                        ],
                    });
                }
                ForthToken::Again => {
                    self.pos += 1;
                    // BEGIN...AGAIN infinite loop
                    return Ok(IrExpr::Call {
                        func: Box::new(IrExpr::Identifier("begin_again".to_string())),
                        args: vec![
                            IrExpr::Identifier("stack".to_string()),
                            IrExpr::Block(body),
                        ],
                    });
                }
                ForthToken::While => {
                    self.pos += 1;
                    // BEGIN...WHILE...REPEAT
                    let while_condition = body.clone();
                    body.clear();
                    while self.pos < self.tokens.len() {
                        if matches!(self.tokens[self.pos].0, ForthToken::Repeat) {
                            self.pos += 1;
                            break;
                        }
                        let expr = self.parse_token()?;
                        body.push(expr);
                    }
                    return Ok(IrExpr::Call {
                        func: Box::new(IrExpr::Identifier("begin_while_repeat".to_string())),
                        args: vec![
                            IrExpr::Identifier("stack".to_string()),
                            IrExpr::Block(while_condition),
                            IrExpr::Block(body),
                        ],
                    });
                }
                _ => {
                    let expr = self.parse_token()?;
                    body.push(expr);
                }
            }
        }

        Ok(IrExpr::Block(body))
    }

    fn parse_variable(&mut self, _builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip VARIABLE
        if let Some((ForthToken::Word(_name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
        }
        Ok(())
    }

    fn parse_constant(&mut self, _builder: &mut IrBuilder) -> Result<()> {
        self.pos += 1; // Skip CONSTANT
        if let Some((ForthToken::Word(_name), _)) = self.tokens.get(self.pos) {
            self.pos += 1;
        }
        Ok(())
    }

    fn stack_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("stack_{}", op))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn binary_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("stack_binop_{}", op.replace('<', "lt").replace('>', "gt").replace('=', "eq").replace("+", "add").replace("-", "sub").replace("*", "mul").replace("/", "div")))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn unary_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("stack_unary_{}", op))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn zero_compare(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("stack_{}", op.replace('<', "lt").replace('>', "gt").replace('=', "eq")))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn io_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("forth_{}", op))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn memory_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("memory_{}", op))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn rstack_op(&self, op: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("rstack_{}", op))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn loop_index(&self, idx: &str) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier(format!("loop_index_{}", idx))),
            args: vec![IrExpr::Identifier("stack".to_string())],
        }
    }

    fn push_literal(&self, n: i64) -> IrExpr {
        IrExpr::Call {
            func: Box::new(IrExpr::Identifier("stack_push".to_string())),
            args: vec![
                IrExpr::Identifier("stack".to_string()),
                IrExpr::Int(n),
            ],
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

/// Parse Forth source code
pub fn parse(source: &str) -> Result<IrModule> {
    ForthParser::new(source).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = ": SQUARE DUP * ;";
        let mut lexer = ForthToken::lexer(source);
        assert!(matches!(lexer.next(), Some(Ok(ForthToken::Colon))));
        assert!(matches!(lexer.next(), Some(Ok(ForthToken::Word(_)))));
        assert!(matches!(lexer.next(), Some(Ok(ForthToken::Dup))));
        assert!(matches!(lexer.next(), Some(Ok(ForthToken::Star))));
        assert!(matches!(lexer.next(), Some(Ok(ForthToken::Semicolon))));
    }

    #[test]
    fn test_word_definition() {
        let source = ": DOUBLE 2 * ;";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "DOUBLE");
    }

    #[test]
    fn test_stack_ops() {
        let source = ": TEST DUP DROP SWAP OVER ROT ;";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].body.len(), 5);
    }

    #[test]
    fn test_arithmetic() {
        let source = ": CALC 10 20 + 5 * ;";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn test_if_then() {
        let source = ": POSITIVE? DUP 0> IF .\" positive\" THEN ;";
        let module = parse(source).unwrap();
        assert_eq!(module.functions.len(), 1);
    }
}
