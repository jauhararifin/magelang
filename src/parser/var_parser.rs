use super::expr_parser::ExprParser;
use super::type_parser::TypeParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct VarParser {
    state: VarStatementParserState,
    name: Option<Token>,
    typ: Option<Type>,
    value: Option<Expr>,
}

enum VarStatementParserState {
    Name,
    Type,
    Value,
}

impl VarParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: VarStatementParserState::Name,
            name: None,
            typ: None,
            value: None,
        })
    }

    fn parse_var_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        self.expect(ctx, TokenKind::Var)?;

        let name_token = self.expect(ctx, TokenKind::Ident)?;
        self.name = Some(name_token);

        self.expect(ctx, TokenKind::Colon)?;
        self.state = VarStatementParserState::Type;
        Ok(ParseResult::Push(TypeParser::new()))
    }

    fn parse_var_type<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        self.typ = Some(Type::from(data));

        if self.check(ctx, &TokenKind::Assign)?.is_none() {
            self.expect(ctx, TokenKind::Endl)?;
            return self.build_result();
        }

        self.state = VarStatementParserState::Value;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_var_value<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        self.expect(ctx, TokenKind::Endl)?;
        self.value = Some(Expr::from(data));
        self.build_result()
    }

    fn build_result<T: Lexer>(&mut self) -> Result<T> {
        Ok(ParseResult::Ast(Ast::Var(Var {
            name: self.name.take().unwrap(),
            typ: self.typ.take().unwrap(),
            value: self.value.take(),
        })))
    }
}

impl<T: Lexer> Parser<T> for VarParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match self.state {
            VarStatementParserState::Name => self.parse_var_name(ctx),
            VarStatementParserState::Type => self.parse_var_type(ctx, data),
            VarStatementParserState::Value => self.parse_var_value(ctx, data),
        }
    }
}
