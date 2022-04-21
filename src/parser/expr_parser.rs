use super::binary_parser::LogicalOrParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::lexer::Lexer;

pub struct ExprParser {}

impl ExprParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for ExprParser {
    fn parse(&mut self, _: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }
        Ok(ParseResult::Push(LogicalOrParser::new()))
    }
}
