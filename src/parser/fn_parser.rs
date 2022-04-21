use super::block_parser::BlockStatementParser;
use super::param_parser::ParamParser;
use super::type_parser::TypeParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct FnParser {
    state: FnParserState,

    name: Option<Token>,
    params: Vec<Param>,
    return_type: Option<Type>,
    body: Option<BlockStatement>,
}

enum FnParserState {
    Name,
    Param,
    Return,
    Body,
}

impl FnParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: FnParserState::Name,
            name: None,
            params: vec![],
            return_type: None,
            body: None,
        })
    }

    fn parse_fn_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        self.expect(ctx, TokenKind::Fn)?;

        let name = self.expect(ctx, TokenKind::Ident)?;

        self.expect(ctx, TokenKind::OpenBrace)?;
        self.name = Some(name);

        if self.check(ctx, &TokenKind::CloseBrace)?.is_some() {
            return self.parse_after_close_brace(ctx);
        }

        self.state = FnParserState::Param;
        Ok(ParseResult::Push(ParamParser::new()))
    }

    fn parse_after_close_brace<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        if self.check(ctx, &TokenKind::Colon)?.is_none() {
            self.state = FnParserState::Body;
            return Ok(ParseResult::Push(BlockStatementParser::new()));
        }

        self.state = FnParserState::Return;
        Ok(ParseResult::Push(TypeParser::new()))
    }

    fn parse_fn_param<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        let param = Param::from(data);
        self.params.push(param);

        let t = self.expect_one_of(ctx, vec![TokenKind::Comma, TokenKind::CloseBrace])?;
        if t.kind == TokenKind::Comma {
            return Ok(ParseResult::Push(ParamParser::new()));
        }

        self.parse_after_close_brace(ctx)
    }

    fn parse_fn_return<T: Lexer>(&mut self, data: AST) -> Result<T> {
        let typ = Type::from(data);
        self.return_type = Some(typ);

        self.state = FnParserState::Body;
        Ok(ParseResult::Push(BlockStatementParser::new()))
    }

    fn parse_fn_body<T: Lexer>(&mut self, data: AST) -> Result<T> {
        let body = BlockStatement::from(data);
        self.body = Some(body);

        Ok(ParseResult::AST(AST::FnDecl(
            FnDecl {
                name: self.name.take().unwrap(),
                param: std::mem::take(&mut self.params),
                ret_type: self.return_type.take(),
                body: self.body.take().unwrap(),
            },
        )))
    }
}

impl<T: Lexer> Parser<T> for FnParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match &self.state {
            FnParserState::Name => self.parse_fn_name(ctx),
            FnParserState::Param => self.parse_fn_param(ctx, data),
            FnParserState::Return => self.parse_fn_return(data),
            FnParserState::Body => self.parse_fn_body(data),
        }
    }
}
