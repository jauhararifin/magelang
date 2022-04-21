use super::unary_parser::UnaryParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

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
                let selection = self.expect(ctx, TokenKind::Ident)?;
                return Ok(ParseResult::AST(AST::Expr(Expr {
                    pos: expr.pos,
                    kind: ExprKind::Selector(Selector {
                        source: Box::new(expr),
                        selection,
                    }),
                })));
            }
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }
        return Ok(ParseResult::Push(UnaryParser::new()));
    }
}
