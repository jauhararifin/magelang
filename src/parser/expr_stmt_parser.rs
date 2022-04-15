use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::Statement;
use crate::token::Lexer;

pub struct ExprStmtParser {}

impl ExprStmtParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for ExprStmtParser {
    fn parse(&mut self, _: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            return Ok(ParseResult::AST(AST::Statement(Statement::Expr(expr))));
        }
        Ok(ParseResult::Push(ExprParser::new()))
    }
}
