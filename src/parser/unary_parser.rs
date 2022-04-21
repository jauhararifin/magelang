use super::call_parser::CallParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct UnaryParser {
    op: Option<Token>,
}

impl UnaryParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { op: None })
    }
}

impl<T: Lexer> Parser<T> for UnaryParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            if let Some(op) = self.op.take() {
                return Ok(ParseResult::AST(AST::Expr(Expr {
                    pos: op.pos,
                    kind: ExprKind::Unary(Unary {
                        op,
                        val: Box::new(expr),
                    }),
                })));
            }
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }

        let token = ctx.lexer.peek()?;
        if matches!(
            token.kind,
            TokenKind::Not
                | TokenKind::Minus
                | TokenKind::Plus
                | TokenKind::BitNot
                | TokenKind::BitAnd
                | TokenKind::Mul
        ) {
            self.op = Some(ctx.lexer.next()?);
            return Ok(ParseResult::Push(UnaryParser::new()));
        }

        return Ok(ParseResult::Push(CallParser::new()));
    }
}
