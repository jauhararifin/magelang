use super::binary_parser::LogicalOrParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::lexer::Lexer;

pub struct ExprParser {}

impl ExprParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for ExprParser {
    fn parse(&mut self, _: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Expr(expr) = data {
            return Ok(ParseResult::Ast(Ast::Expr(expr)));
        }
        Ok(ParseResult::Push(LogicalOrParser::new()))
    }
}
