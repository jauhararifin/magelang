use super::{Context, Error, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | function_call ;
// function_call  → primary ( "(" params ")" )?;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

pub struct CallParser {}

impl CallParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for CallParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        unimplemented!();
    }
}
