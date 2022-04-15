use super::expr_parser::ExprParser;
use super::primary_parser::PrimaryParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};

pub struct CallParser {
    state: CallParserState,
    ptr: Option<Expr>,
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
            ptr: None,
            params: vec![],
        })
    }

    fn parse_fn_ptr<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            if self.check(ctx, &TokenKind::OpenBrace)?.is_none() {
                return Ok(ParseResult::AST(AST::Expr(expr)));
            }

            self.ptr = Some(expr);
            self.state = CallParserState::Params;

            if self.check(ctx, &TokenKind::CloseBrace)?.is_some() {
                return Ok(ParseResult::AST(AST::Expr(Expr::FunctionCall {
                    ptr: Box::new(self.ptr.take().unwrap()),
                    args: std::mem::replace(&mut self.params, vec![]),
                })));
            }
            return Ok(ParseResult::Push(ExprParser::new()));
        }
        Ok(ParseResult::Push(PrimaryParser::new()))
    }

    fn parse_fn_params<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        let expr = data.as_expr();
        self.params.push(expr);

        let token = self.expect_one_of(ctx, vec![TokenKind::Comma, TokenKind::CloseBrace])?;
        if let TokenKind::Comma = token.kind {
            return Ok(ParseResult::Push(ExprParser::new()));
        } else {
            return Ok(ParseResult::AST(AST::Expr(Expr::FunctionCall {
                ptr: Box::new(self.ptr.take().unwrap()),
                args: std::mem::replace(&mut self.params, vec![]),
            })));
        }
    }
}

impl<T: Lexer> Parser<T> for CallParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            CallParserState::Ptr => self.parse_fn_ptr(ctx, data),
            CallParserState::Params => self.parse_fn_params(ctx, data),
        }
    }
}
