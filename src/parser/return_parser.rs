use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct ReturnParser {}

impl ReturnParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for ReturnParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Expr(expr) = data {
            return Ok(ParseResult::Ast(Ast::Return(Return { value: expr })));
        }

        self.expect(ctx, TokenKind::Return)?;
        Ok(ParseResult::Push(ExprParser::new()))
    }
}
