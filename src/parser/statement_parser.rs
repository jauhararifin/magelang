use super::assign_parser::AssignParser;
use super::if_parser::IfParser;
use super::return_parser::ReturnParser;
use super::var_parser::VarParser;
use super::while_parser::WhileParser;
use super::{Context, ParseResult, Parser, Result, AST};
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
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match data {
            AST::Var(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::Var(stmt)))),
            AST::Assign(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::Assign(stmt)))),
            AST::Return(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::Return(stmt)))),
            AST::If(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::If(stmt)))),
            AST::While(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::While(stmt)))),
            AST::BlockStatement(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::Block(stmt)))),
            AST::Expr(stmt) => return Ok(ParseResult::AST(AST::Statement(Statement::Expr(stmt)))),
            AST::Statement(stmt) => return Ok(ParseResult::AST(AST::Statement(stmt))),
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
