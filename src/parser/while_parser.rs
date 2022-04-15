use super::block_parser::BlockStatementParser;
use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct WhileParser {
    state: WhileParserState,
    cond: Option<Expr>,
}

enum WhileParserState {
    Cond,
    Body,
}

impl WhileParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: WhileParserState::Cond,
            cond: None,
        })
    }

    fn parse_cond<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            self.cond = Some(expr);
            self.state = WhileParserState::Body;
            return Ok(ParseResult::Push(BlockStatementParser::new()));
        }

        self.expect(ctx, TokenKind::While)?;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_body<T: Lexer>(&mut self, data: AST) -> Result<T> {
        return Ok(ParseResult::AST(AST::Statement(Statement::While {
            cond: self.cond.take().unwrap(),
            body: data.as_block_statement(),
        })));
    }
}

impl<T: Lexer> Parser<T> for WhileParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            WhileParserState::Cond => self.parse_cond(ctx, data),
            WhileParserState::Body => self.parse_body(data),
        }
    }
}
