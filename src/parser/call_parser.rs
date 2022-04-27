use super::expr_parser::ExprParser;
use super::primary_parser::PrimaryParser;
use super::{Ast, Context, ParseResult, Parser, Result};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct CallParser {
    state: CallParserState,
    func: Option<Expr>,
    params: Vec<Expr>,
}

enum CallParserState {
    Ptr,
    Params,
}

impl CallParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: CallParserState::Ptr,
            func: None,
            params: vec![],
        })
    }

    fn parse_fn_ptr<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Expr(expr) = data {
            if self.check(ctx, &TokenKind::OpenBrace)?.is_none() {
                return Ok(ParseResult::Ast(Ast::Expr(expr)));
            }

            self.func = Some(expr);
            self.state = CallParserState::Params;

            if self.check(ctx, &TokenKind::CloseBrace)?.is_some() {
                return Ok(ParseResult::Ast(Ast::Expr(Expr {
                    pos: self.func.as_ref().unwrap().pos,
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(self.func.take().unwrap()),
                        args: std::mem::take(&mut self.params),
                    }),
                })));
            }
            return Ok(ParseResult::Push(ExprParser::new()));
        }
        Ok(ParseResult::Push(PrimaryParser::new()))
    }

    fn parse_fn_params<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        let expr = Expr::from(data);
        self.params.push(expr);

        let token = self.expect_one_of(ctx, vec![TokenKind::Comma, TokenKind::CloseBrace])?;
        if let TokenKind::Comma = token.kind {
            Ok(ParseResult::Push(ExprParser::new()))
        } else {
            return Ok(ParseResult::Ast(Ast::Expr(Expr {
                pos: self.func.as_ref().unwrap().pos,
                kind: ExprKind::FunctionCall(FunctionCall {
                    func: Box::new(self.func.take().unwrap()),
                    args: std::mem::take(&mut self.params),
                }),
            })));
        }
    }
}

impl<T: Lexer> Parser<T> for CallParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match self.state {
            CallParserState::Ptr => self.parse_fn_ptr(ctx, data),
            CallParserState::Params => self.parse_fn_params(ctx, data),
        }
    }
}
