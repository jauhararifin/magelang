use super::call_parser::CallParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct UnaryParser {
    op: Option<Token>,
}

impl UnaryParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { op: None })
    }
}

impl<T: Lexer> Parser<T> for UnaryParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Expr(expr) = data {
            if let Some(op) = self.op.take() {
                return Ok(ParseResult::Ast(Ast::Expr(Expr {
                    pos: op.pos,
                    kind: ExprKind::Unary(Unary {
                        op,
                        val: Box::new(expr),
                    }),
                })));
            }
            return Ok(ParseResult::Ast(Ast::Expr(expr)));
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

        Ok(ParseResult::Push(CallParser::new()))
    }
}
