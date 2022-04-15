use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct ReturnParser {}

impl ReturnParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for ReturnParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            return Ok(ParseResult::AST(AST::Statement(Statement::Return(expr))));
        }

        self.expect(ctx, TokenKind::Return)?;
        Ok(ParseResult::Push(ExprParser::new()))
    }
}
