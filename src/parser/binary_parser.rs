use super::unary_parser::UnaryParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct BinaryParser<T: BinaryParserConfig> {
    config: T,

    state: BinaryParserState,

    a_expr: Option<Expr>,
    op_token: Option<Token>,
}

pub trait BinaryParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind>;
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>>;
}

enum BinaryParserState {
    A,
    B,
}

impl<T: BinaryParserConfig> BinaryParser<T> {
    fn new(config: T) -> Self {
        Self {
            config,

            state: BinaryParserState::A,

            a_expr: None,
            op_token: None,
        }
    }

    fn parse_logical_and_a<L: Lexer>(&mut self, ctx: &mut Context<L>, data: AST) -> Result<L> {
        if let AST::Expr(expr) = data {
            for op in self.config.get_op_kinds().iter() {
                if let Some(op) = self.check(ctx, op)? {
                    self.a_expr = Some(expr);
                    self.op_token = Some(op);
                    self.state = BinaryParserState::B;
                    return Ok(ParseResult::Push(self.config.get_next_parser()));
                }
            }
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }
        return Ok(ParseResult::Push(self.config.get_next_parser()));
    }

    fn parse_logical_and_b<L: Lexer>(&mut self, _: &mut Context<L>, data: AST) -> Result<L> {
        let expr = data.as_expr();
        Ok(ParseResult::AST(AST::Expr(Expr::Binary {
            op: self.op_token.take().unwrap(),
            a: Box::new(self.a_expr.take().unwrap()),
            b: Box::new(expr),
        })))
    }
}

impl<T: BinaryParserConfig, L: Lexer> Parser<L> for BinaryParser<T> {
    fn parse(&mut self, ctx: &mut Context<L>, data: AST) -> Result<L> {
        match self.state {
            BinaryParserState::A => self.parse_logical_and_a(ctx, data),
            BinaryParserState::B => self.parse_logical_and_b(ctx, data),
        }
    }
}

// Logical Or

struct LogicalOrParserConfig();

impl BinaryParserConfig for LogicalOrParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::Or]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        LogicalAndParser::new()
    }
}

pub struct LogicalOrParser(BinaryParser<LogicalOrParserConfig>);

impl LogicalOrParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(LogicalOrParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for LogicalOrParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Logical And

struct LogicalAndParserConfig();

impl BinaryParserConfig for LogicalAndParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::And]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        BitOrParser::new()
    }
}

pub struct LogicalAndParser(BinaryParser<LogicalAndParserConfig>);

impl LogicalAndParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(LogicalAndParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for LogicalAndParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Bit Or

struct BitOrParserConfig();

impl BinaryParserConfig for BitOrParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::BitOr]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        BitXorParser::new()
    }
}

pub struct BitOrParser(BinaryParser<BitOrParserConfig>);

impl BitOrParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(BitOrParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for BitOrParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Bit Xor

struct BitXorParserConfig();

impl BinaryParserConfig for BitXorParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::BitXor]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        BitAndParser::new()
    }
}

pub struct BitXorParser(BinaryParser<BitXorParserConfig>);

impl BitXorParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(BitXorParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for BitXorParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Bit And

struct BitAndParserConfig();

impl BinaryParserConfig for BitAndParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::BitAnd]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        EqualityParser::new()
    }
}

pub struct BitAndParser(BinaryParser<BitAndParserConfig>);

impl BitAndParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(BitAndParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for BitAndParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Equality

struct EqualityParserConfig();

impl BinaryParserConfig for EqualityParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::Eq, TokenKind::NotEq]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        RelationalParser::new()
    }
}

pub struct EqualityParser(BinaryParser<EqualityParserConfig>);

impl EqualityParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(EqualityParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for EqualityParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Relational

struct RelationalParserConfig();

impl BinaryParserConfig for RelationalParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::LT, TokenKind::LTEq, TokenKind::GT, TokenKind::GTEq]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        ShiftParser::new()
    }
}

pub struct RelationalParser(BinaryParser<RelationalParserConfig>);

impl RelationalParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(RelationalParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for RelationalParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Shift Parser

struct ShiftParserConfig();

impl BinaryParserConfig for ShiftParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::SHL, TokenKind::SHR]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        AdditiveParser::new()
    }
}

pub struct ShiftParser(BinaryParser<ShiftParserConfig>);

impl ShiftParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(ShiftParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for ShiftParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Additive

struct AdditiveParserConfig();

impl BinaryParserConfig for AdditiveParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::Plus, TokenKind::Minus]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        MultiplicativeParser::new()
    }
}

pub struct AdditiveParser(BinaryParser<AdditiveParserConfig>);

impl AdditiveParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(AdditiveParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for AdditiveParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

// Multiplicative

struct MultiplicativeParserConfig();

impl BinaryParserConfig for MultiplicativeParserConfig {
    fn get_op_kinds(&self) -> Vec<TokenKind> {
        vec![TokenKind::Mul, TokenKind::Div, TokenKind::Mod]
    }
    fn get_next_parser<T: Lexer>(&self) -> Box<dyn Parser<T>> {
        UnaryParser::new()
    }
}

pub struct MultiplicativeParser(BinaryParser<MultiplicativeParserConfig>);

impl MultiplicativeParser {
    pub fn new() -> Box<Self> {
        Box::new(Self(BinaryParser::new(MultiplicativeParserConfig())))
    }
}

impl<T: Lexer> Parser<T> for MultiplicativeParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.0.parse(ctx, data)
    }
}

