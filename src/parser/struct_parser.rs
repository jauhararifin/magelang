use super::param_parser::ParamParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct StructParser {
    params: Vec<Param>,
}

impl StructParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { params: vec![] })
    }
}

impl<T: Lexer> Parser<T> for StructParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Param(param) = data {
            self.params.push(param);

            self.consume_endl(ctx)?;

            let is_end = if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
                true
            } else {
                self.expect(ctx, TokenKind::Comma)?;
                self.consume_endl(ctx)?;
                if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
                    true
                } else {
                    false
                }
            };

            if is_end {
                return Ok(ParseResult::AST(AST::Type(Type::Struct(Struct {
                    fields: std::mem::replace(&mut self.params, vec![]),
                }))));
            } else {
                return Ok(ParseResult::Push(ParamParser::new()));
            }
        }

        self.expect(ctx, TokenKind::Struct)?;
        self.consume_endl(ctx)?;
        self.expect(ctx, TokenKind::OpenBlock)?;
        self.consume_endl(ctx)?;

        if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
            return Ok(ParseResult::AST(AST::Type(Type::Struct(Struct {
                fields: std::mem::replace(&mut self.params, vec![]),
            }))));
        } else {
            return Ok(ParseResult::Push(ParamParser::new()));
        }
    }
}