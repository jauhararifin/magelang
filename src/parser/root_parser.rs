use super::{Context, Error, ParseResult, Parser, AST};
use crate::ast::*;
use crate::token::{Lexer, TokenKind};
use super::fn_parser::FnParser;

pub struct RootParser {
    declarations: Vec<Declaration>,
}

impl RootParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            declarations: Vec::new(),
        })
    }
}

impl<T: Lexer> Parser<T> for RootParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        match data {
            AST::Declaration(decl) => self.declarations.push(decl),
            AST::Empty => (),
            _ => panic!("got {:?} instead of declaration", data),
        }

        loop {
            let token = ctx.lexer.peek()?;
            match token.kind {
                TokenKind::EOI => {
                    return Ok(ParseResult::AST(AST::Root(Root {
                        declarations: std::mem::replace(&mut self.declarations, vec![]),
                    })));
                }
                TokenKind::Endl => {
                    ctx.lexer.next()?;
                }
                TokenKind::Fn => {
                    return Ok(ParseResult::Push(FnParser::new()));
                }
                TokenKind::Var => {
                    unimplemented!("global variable is not implemented yet");
                }
                _ => {
                    let token = ctx.lexer.next()?;
                    return Err(Error::UnexpectedToken {
                        expected: vec![TokenKind::Fn, TokenKind::Var],
                        found: token,
                    });
                }
            }
        }
    }
}
