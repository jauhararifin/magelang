use super::param_parser::ParamParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct StructParser {
    params: Vec<Param>,
}

impl StructParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { params: vec![] })
    }
}

impl<T: Lexer> Parser<T> for StructParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Param(param) = data {
            self.params.push(param);

            self.consume_endl(ctx)?;

            let is_end = if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
                true
            } else {
                self.expect(ctx, TokenKind::Comma)?;
                self.consume_endl(ctx)?;
                self.check(ctx, &TokenKind::CloseBlock)?.is_some()
            };

            if is_end {
                return Ok(ParseResult::Ast(Ast::Struct(Struct {
                    fields: std::mem::take(&mut self.params),
                })));
            } else {
                return Ok(ParseResult::Push(ParamParser::new()));
            }
        }

        self.expect(ctx, TokenKind::Struct)?;
        self.consume_endl(ctx)?;

        self.expect(ctx, TokenKind::OpenBlock)?;
        self.consume_endl(ctx)?;

        if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
            Ok(ParseResult::Ast(Ast::Struct(Struct {
                fields: std::mem::take(&mut self.params),
            })))
        } else {
            Ok(ParseResult::Push(ParamParser::new()))
        }
    }
}
