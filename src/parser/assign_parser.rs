use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct AssignParser {
    state: AssignParserState,
    name: Option<Token>,
}

enum AssignParserState {
    Name,
    Value,
}

impl AssignParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: AssignParserState::Name,
            name: None,
        })
    }

    fn parse_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        let name_token = self.expect(ctx, TokenKind::Ident("".to_string()))?;
        self.name = Some(name_token);

        self.expect(ctx, TokenKind::Assign)?;
        self.state = AssignParserState::Value;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_value<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        let value = data.as_expr();
        self.expect(ctx, TokenKind::Endl)?;
        return Ok(ParseResult::AST(AST::Statement(Statement::Assign(
            Assign {
                name: self.name.take().unwrap(),
                value,
            },
        ))));
    }
}

impl<T: Lexer> Parser<T> for AssignParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            AssignParserState::Name => self.parse_name(ctx),
            AssignParserState::Value => self.parse_value(ctx, data),
        }
    }
}
