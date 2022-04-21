use super::statement_parser::StatementParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct BlockStatementParser {
    state: BlockStatementParserState,
    body: Vec<Statement>,
}

enum BlockStatementParserState {
    Open,
    Body,
}

impl BlockStatementParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: BlockStatementParserState::Open,
            body: vec![],
        })
    }

    fn parse_block_open<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        self.expect(ctx, TokenKind::OpenBlock)?;

        self.state = BlockStatementParserState::Body;
        self.parse_block_body(ctx, Ast::Empty)
    }

    fn parse_block_body<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if !matches!(data, Ast::Empty) {
            self.body.push(Statement::from(data))
        }

        while self.check(ctx, &TokenKind::Endl)?.is_some() {}

        if self.check(ctx, &TokenKind::CloseBlock)?.is_some() {
            return Ok(ParseResult::Ast(Ast::BlockStatement(BlockStatement {
                body: std::mem::take(&mut self.body),
            })));
        }

        Ok(ParseResult::Push(StatementParser::new()))
    }
}

impl<T: Lexer> Parser<T> for BlockStatementParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match self.state {
            BlockStatementParserState::Open => self.parse_block_open(ctx),
            BlockStatementParserState::Body => self.parse_block_body(ctx, data),
        }
    }
}
