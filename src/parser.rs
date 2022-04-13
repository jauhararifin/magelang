use crate::ast::*;
use crate::token::{Error as LexerError, Lexer, Token, TokenKind};
use std::mem::discriminant;

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

pub trait Parser {
    fn parse(&mut self) -> Result<Root, Error>;
}

pub struct SimpleParser<T: Lexer> {
    lexer: T,
}

impl<T: Lexer> SimpleParser<T> {
    pub fn new(lexer: T) -> Self {
        Self { lexer }
    }

    fn parse(&mut self) -> Result<Root, Error> {
        let mut declarations = Vec::new();

        'outer: loop {
            let token = self.lexer.peek()?;

            match token.kind {
                TokenKind::EOI => break 'outer,
                TokenKind::Endl | TokenKind::Comment(_) => {
                    self.lexer.next()?;
                }
                TokenKind::Var => {
                    declarations.push(self.parse_var()?);
                }
                TokenKind::Fn => {
                    declarations.push(self.parse_fn()?);
                }
                _ => {
                    let token = self.lexer.next()?;
                    return Err(Error::UnexpectedToken {
                        expected: vec![TokenKind::Var, TokenKind::Fn],
                        found: token,
                    });
                }
            }
        }

        Ok(Root { declarations })
    }

    fn parse_var(&mut self) -> Result<Declaration, Error> {
        self.expect(TokenKind::Var)?;
        let name = self.expect(TokenKind::Ident("".to_string()))?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type()?;

        let mut value = None;
        if let Ok(_) = self.expect(TokenKind::Assign) {
            value = Some(self.parse_expr()?);
        }

        Ok(Declaration::Var { name, typ, value })
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        unimplemented!();
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        unimplemented!();
    }

    fn parse_fn(&mut self) -> Result<Declaration, Error> {
        unimplemented!();
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        let token = self.lexer.next()?;

        if discriminant(&token.kind) != discriminant(&kind) {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }

        Ok(token)
    }
}

impl<T: Lexer> Parser for SimpleParser<T> {
    fn parse(&mut self) -> Result<Root, Error> {
        SimpleParser::parse(self)
    }
}
