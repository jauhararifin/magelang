use super::expr_parser::ExprParser;
use super::type_parser::TypeParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct VarStatementParser {
    state: VarStatementParserState,
    name: Option<Token>,
    typ: Option<Type>,
}

enum VarStatementParserState {
    Name,
    Type,
    Value,
}

impl VarStatementParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: VarStatementParserState::Name,
            name: None,
            typ: None,
        })
    }

    fn parse_var_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        self.expect(ctx, TokenKind::Var)?;

        let name_token = self.expect(ctx, TokenKind::Ident("".to_string()))?;
        self.name = Some(name_token);

        self.expect(ctx, TokenKind::Colon)?;
        self.state = VarStatementParserState::Type;
        Ok(ParseResult::Push(TypeParser::new()))
    }

    fn parse_var_type<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.typ = Some(data.as_type());

        if self.check(ctx, &TokenKind::Assign)?.is_none() {
            self.expect(ctx, TokenKind::Endl)?;
            return Ok(ParseResult::AST(AST::Statement(Statement::VarDecl {
                name: self.name.take().unwrap(),
                typ: self.typ.take().unwrap(),
                value: None,
            })));
        }

        self.state = VarStatementParserState::Value;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_var_value<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.expect(ctx, TokenKind::Endl)?;
        return Ok(ParseResult::AST(AST::Statement(Statement::VarDecl {
            name: self.name.take().unwrap(),
            typ: self.typ.take().unwrap(),
            value: Some(data.as_expr()),
        })));
    }
}

impl<T: Lexer> Parser<T> for VarStatementParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            VarStatementParserState::Name => self.parse_var_name(ctx),
            VarStatementParserState::Type => self.parse_var_type(ctx, data),
            VarStatementParserState::Value => self.parse_var_value(ctx, data),
        }
    }
}
