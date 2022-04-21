use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
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
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            return Ok(ParseResult::AST(AST::Return(Return { value: expr })));
        }

        self.expect(ctx, TokenKind::Return)?;
        Ok(ParseResult::Push(ExprParser::new()))
    }
}
