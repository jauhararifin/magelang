use super::unary_parser::UnaryParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct SelectorParser {}

impl SelectorParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for SelectorParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            if self.check(ctx, &TokenKind::Dot)?.is_some() {
                let selection = self.expect(ctx, TokenKind::Ident("".to_string()))?;
                return Ok(ParseResult::AST(AST::Expr(Expr::Selector(Selector {
                    source: Box::new(expr),
                    selection,
                }))));
            }
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }
        return Ok(ParseResult::Push(UnaryParser::new()));
    }
}