use super::block_parser::BlockStatementParser;
use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct IfParser {
    state: IfParserState,
    cond: Option<Expr>,
}

enum IfParserState {
    Cond,
    Body,
}

impl IfParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: IfParserState::Cond,
            cond: None,
        })
    }

    fn parse_cond<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            self.cond = Some(expr);
            self.state = IfParserState::Body;
            return Ok(ParseResult::Push(BlockStatementParser::new()));
        }

        self.expect(ctx, TokenKind::If)?;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_body<T: Lexer>(&mut self, data: AST) -> Result<T> {
        Ok(ParseResult::AST(AST::If(If {
            cond: self.cond.take().unwrap(),
            body: BlockStatement::from(data),
        })))
    }
}

impl<T: Lexer> Parser<T> for IfParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            IfParserState::Cond => self.parse_cond(ctx, data),
            IfParserState::Body => self.parse_body(data),
        }
    }
}
