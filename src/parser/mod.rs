use crate::ast::{*, self};
use crate::token::{Lexer, Token, TokenKind};
use std::mem::discriminant;

mod assign_parser;
mod binary_parser;
mod block_parser;
mod call_parser;
mod cast_parser;
mod expr_parser;
mod fn_parser;
mod if_parser;
mod param_parser;
mod primary_parser;
mod return_parser;
mod root_parser;
mod selector_parser;
mod statement_parser;
mod struct_parser;
mod type_parser;
mod unary_parser;
mod var_parser;
mod while_parser;

use root_parser::RootParser;

type Result<T> = std::result::Result<ParseResult<T>, Error>;

pub trait Parser<T: Lexer> {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T>;

    fn expect(&self, ctx: &mut Context<T>, kind: TokenKind) -> std::result::Result<Token, Error> {
        let token = ctx.lexer.next()?;
        if discriminant(&token.kind) != discriminant(&kind) {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }

        Ok(token)
    }

    fn consume_endl(&self, ctx: &mut Context<T>) -> std::result::Result<(), Error> {
        while let TokenKind::Endl = ctx.lexer.peek()?.kind {
            ctx.lexer.next()?;
        }
        Ok(())
    }

    fn check(
        &self,
        ctx: &mut Context<T>,
        kind: &TokenKind,
    ) -> std::result::Result<Option<Token>, Error> {
        let token = ctx.lexer.peek()?;
        if discriminant(&token.kind) != discriminant(kind) {
            return Ok(None);
        }

        let token = ctx.lexer.next()?;
        Ok(Some(token))
    }

    fn expect_one_of(
        &self,
        ctx: &mut Context<T>,
        kind: Vec<TokenKind>,
    ) -> std::result::Result<Token, Error> {
        let token = ctx.lexer.next()?;

        for k in kind.iter() {
            if discriminant(&token.kind) == discriminant(k) {
                return Ok(token);
            }
        }

        Err(Error::UnexpectedToken {
            expected: kind,
            found: token,
        })
    }

    fn check_one_of(
        &self,
        ctx: &mut Context<T>,
        kind: Vec<TokenKind>,
    ) -> std::result::Result<Option<Token>, Error> {
        let token = ctx.lexer.peek()?;

        for k in kind.iter() {
            if discriminant(&token.kind) != discriminant(k) {
                return Ok(Some(ctx.lexer.next()?));
            }
        }

        Ok(None)
    }
}

pub struct Context<T: Lexer> {
    lexer: T,
}

pub enum ParseResult<T: Lexer> {
    Push(Box<dyn Parser<T>>),
    AST(ast::AST),
}

pub struct SimpleParser<T: Lexer> {
    context: Context<T>,
}

impl<T: Lexer> SimpleParser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            context: Context { lexer },
        }
    }
}

impl<T:Lexer> ast::Parser for SimpleParser<T> {
    fn parse(&mut self) -> std::result::Result<Root, ast::Error> {
        let mut stack: Vec<Box<dyn Parser<T>>> = vec![RootParser::new()];

        let mut data = AST::Empty;
        while let Some(mut parser) = stack.pop() {
            let result = parser.parse(&mut self.context, data)?;
            match result {
                ParseResult::Push(new_parser) => {
                    stack.push(parser);
                    stack.push(new_parser);
                    data = AST::Empty;
                }
                ParseResult::AST(ast) => {
                    data = ast;
                }
            }
        }

        if let AST::Root(result) = data {
            return Ok(result);
        }

        panic!("invalid parsing result");
    }
}
