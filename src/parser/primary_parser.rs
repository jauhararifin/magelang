use super::expr_parser::ExprParser;
use super::{Context, Error, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct PrimaryParser {}

impl PrimaryParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for PrimaryParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            self.expect(ctx, TokenKind::CloseBrace)?;
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }

        if let Some(token) = self.check(ctx, &TokenKind::IntegerLit)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::IntegerLit(token))));
        }

        if let Some(token) = self.check(ctx, &TokenKind::FloatLit)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::FloatLit(token))));
        }

        if let Some(token) = self.check(ctx, &TokenKind::StringLit)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::StringLit(token))));
        }

        if let Some(token) = self.check(ctx, &TokenKind::True)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::BoolLit(token))));
        }

        if let Some(token) = self.check(ctx, &TokenKind::False)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::BoolLit(token))));
        }

        if let Some(token) = self.check(ctx, &TokenKind::Ident)? {
            return Ok(ParseResult::AST(AST::Expr(Expr::Ident(token))));
        }

        if let Some(_) = self.check(ctx, &TokenKind::OpenBrace)? {
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
