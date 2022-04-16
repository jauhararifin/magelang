use super::expr_parser::ExprParser;
use super::type_parser::TypeParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct VarParser {
    as_statement: bool,
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
    fn new(as_statement: bool) -> Box<Self> {
        Box::new(Self {
            as_statement,
            state: VarStatementParserState::Name,
            name: None,
            typ: None,
            value: None,
        })
    }

    pub fn new_statement_parser() -> Box<Self> {
        Self::new(true)
    }

    pub fn new_decl_parser() -> Box<Self> {
        Self::new(false)
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
            return self.build_result();
        }

        self.state = VarStatementParserState::Value;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_var_value<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        self.expect(ctx, TokenKind::Endl)?;
        self.value = Some(data.as_expr());
        self.build_result()
    }

    fn build_result<T: Lexer>(&mut self) -> Result<T> {
        Ok(ParseResult::AST(if self.as_statement {
            AST::Statement(Statement::Var(Var {
                name: self.name.take().unwrap(),
                typ: self.typ.take().unwrap(),
                value: self.value.take(),
            }))
        } else {
            AST::Declaration(Declaration::Var(Var {
                name: self.name.take().unwrap(),
                typ: self.typ.take().unwrap(),
                value: self.value.take(),
            }))
        }))
    }
}

impl<T: Lexer> Parser<T> for VarParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            VarStatementParserState::Name => self.parse_var_name(ctx),
            VarStatementParserState::Type => self.parse_var_type(ctx, data),
            VarStatementParserState::Value => self.parse_var_value(ctx, data),
        }
    }
}
