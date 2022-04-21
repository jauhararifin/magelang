use super::type_parser::TypeParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct ParamParser {
    state: ParamParserState,
    name_token: Option<Token>,
}

enum ParamParserState {
    Name,
    Type,
}

impl ParamParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: ParamParserState::Name,
            name_token: None,
        })
    }

    fn parse_param_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        let name_token = self.expect(ctx, TokenKind::Ident)?;
        self.name_token = Some(name_token);

        self.expect(ctx, TokenKind::Colon)?;
        self.state = ParamParserState::Type;

        Ok(ParseResult::Push(TypeParser::new()))
    }

    fn parse_param_type<T: Lexer>(&mut self, data: AST) -> Result<T> {
        let typ = Type::from(data);
        Ok(ParseResult::AST(AST::Param(Param {
            name: self.name_token.take().unwrap(),
            typ,
        })))
    }
}

impl<T: Lexer> Parser<T> for ParamParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            ParamParserState::Name => self.parse_param_name(ctx),
            ParamParserState::Type => self.parse_param_type(data),
        }
    }
}
