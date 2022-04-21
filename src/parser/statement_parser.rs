use super::assign_parser::AssignParser;
use super::if_parser::IfParser;
use super::return_parser::ReturnParser;
use super::var_parser::VarParser;
use super::while_parser::WhileParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::Statement;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct StatementParser {}

impl StatementParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for StatementParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match data {
            Ast::Var(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::Var(stmt)))),
            Ast::Assign(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::Assign(stmt)))),
            Ast::Return(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::Return(stmt)))),
            Ast::If(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::If(stmt)))),
            Ast::While(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::While(stmt)))),
            Ast::BlockStatement(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::Block(stmt)))),
            Ast::Expr(stmt) => return Ok(ParseResult::Ast(Ast::Statement(Statement::Expr(stmt)))),
            Ast::Statement(stmt) => return Ok(ParseResult::Ast(Ast::Statement(stmt))),
            _ => {},
        }

        self.consume_endl(ctx)?;

        let token = ctx.lexer.peek()?;
        let next_parser: Box<dyn Parser<T>> = match &token.kind {
            TokenKind::Var => VarParser::new(),
            TokenKind::While => WhileParser::new(),
            TokenKind::If => IfParser::new(),
            TokenKind::Return => ReturnParser::new(),
            _ => AssignParser::new(),
        };

        Ok(ParseResult::Push(next_parser))
    }
}
