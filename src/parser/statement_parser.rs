use super::if_parser::IfParser;
use super::return_parser::ReturnParser;
use super::while_parser::WhileParser;
use super::{Context, ParseResult, Parser, Result, AST};
use super::var_parser::VarStatementParser;
use crate::token::{Lexer, TokenKind};

pub struct StatementParser {}

impl StatementParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for StatementParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Statement(s) = data {
            return Ok(ParseResult::AST(AST::Statement(s)));
        }

        while self.check(ctx, &TokenKind::Endl)?.is_some() {}

        let token = ctx.lexer.peek_n(2)?;
        match (&token[0].kind, &token[1].kind) {
            (TokenKind::Var, _) => {
                Ok(ParseResult::Push(VarStatementParser::new()))
            },
            (TokenKind::Ident(_), TokenKind::Assign) => {
                unimplemented!("assignment is not implemented yet")
            }
            (TokenKind::While, _) => {
                Ok(ParseResult::Push(WhileParser::new()))
            }
            (TokenKind::If, _) => {
                Ok(ParseResult::Push(IfParser::new()))
            }
            (TokenKind::Return, _) => {
                Ok(ParseResult::Push(ReturnParser::new()))
            }
            _ => unimplemented!("expression is not implemented yet"),
        }
    }
}
