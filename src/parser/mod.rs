use crate::ast::*;
use crate::token::{Error as LexerError, Lexer, Token, TokenKind};
use std::mem::discriminant;

mod binary_parser;
mod block_parser;
mod call_parser;
mod expr_parser;
mod fn_parser;
mod param_parser;
mod primary_parser;
mod root_parser;
mod statement_parser;
mod type_parser;
mod unary_parser;
mod var_parser;
mod while_parser;
mod if_parser;

use root_parser::RootParser;

#[derive(Debug)]
pub enum Error {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
    Lexer(LexerError),
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Self {
        Error::Lexer(err)
    }
}

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
}

pub struct Context<T: Lexer> {
    lexer: T,
}

pub enum ParseResult<T: Lexer> {
    Push(Box<dyn Parser<T>>),
    AST(AST),
}

#[derive(Debug)]
pub enum AST {
    Root(Root),
    Declaration(Declaration),
    Param(Param),
    Type(Type),
    Statement(Statement),
    Expr(Expr),
    Empty,
}

impl AST {
    fn as_param(self) -> Param {
        if let AST::Param(param) = self {
            return param;
        }
        panic!("as_param called but the underlying variant is not AST::Param");
    }

    fn as_type(self) -> Type {
        if let AST::Type(typ) = self {
            return typ;
        }
        panic!("as_type called but the underlying variant is not AST::Type");
    }

    fn as_statement(self) -> Statement {
        if let AST::Statement(stmt) = self {
            return stmt;
        }
        panic!("as_statement called but the underlying variant is not AST::Statement");
    }

    fn as_block_statement(self) -> BlockStatement {
        if let Statement::Block(block) = self.as_statement() {
            return block;
        }
        panic!(
            "as_block called but the underlying variant is not AST::Statement(Statement::Block)"
        );
    }

    fn as_expr(self) -> Expr {
        if let AST::Expr(expr) = self {
            return expr;
        }
        panic!("as_expr called but the underlying variant is not AST::Expr");
    }
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

    pub fn parse(&mut self) -> std::result::Result<Root, Error> {
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
