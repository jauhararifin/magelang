use super::expr_parser::ExprParser;
use super::{Context, Error, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct PrimaryParser {}

impl PrimaryParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for PrimaryParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Expr(expr) = data {
            self.expect(ctx, TokenKind::CloseBrace)?;
            return Ok(ParseResult::Ast(Ast::Expr(expr)));
        }

        if let Some(token) = self.check(ctx, &TokenKind::IntegerLit)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::IntegerLit(token),
            })));
        }

        if let Some(token) = self.check(ctx, &TokenKind::FloatLit)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::FloatLit(token),
            })));
        }

        if let Some(token) = self.check(ctx, &TokenKind::StringLit)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::StringLit(token),
            })));
        }

        if let Some(token) = self.check(ctx, &TokenKind::True)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::BoolLit(token),
            })));
        }

        if let Some(token) = self.check(ctx, &TokenKind::False)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::BoolLit(token),
            })));
        }

        if let Some(token) = self.check(ctx, &TokenKind::Ident)? {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::Ident(token),
            })));
        }

        if (self.check(ctx, &TokenKind::OpenBrace)?).is_some() {
            return Ok(ParseResult::Push(ExprParser::new()));
        }

        Err(Error::UnexpectedToken {
            expected: vec![
                TokenKind::IntegerLit,
                TokenKind::FloatLit,
                TokenKind::StringLit,
                TokenKind::True,
                TokenKind::False,
                TokenKind::OpenBrace,
                TokenKind::Ident,
            ],
            found: ctx.lexer.next()?,
        })
    }
}
