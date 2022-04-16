use super::assign_parser::AssignParser;
use super::expr_stmt_parser::ExprStmtParser;
use super::if_parser::IfParser;
use super::return_parser::ReturnParser;
use super::var_parser::VarParser;
use super::while_parser::WhileParser;
use super::{Context, ParseResult, Parser, Result, AST};
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
        let next_parser: Box<dyn Parser<T>> = match (&token[0].kind, &token[1].kind) {
            (TokenKind::Var, _) => VarParser::new_statement_parser(),
            (TokenKind::Ident(_), TokenKind::Assign) => AssignParser::new(),
            (TokenKind::While, _) => WhileParser::new(),
            (TokenKind::If, _) => IfParser::new(),
            (TokenKind::Return, _) => ReturnParser::new(),
            _ => ExprStmtParser::new(),
        };

        Ok(ParseResult::Push(next_parser))
    }
}
