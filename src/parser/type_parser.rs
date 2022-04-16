use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct TypeParser {}

impl TypeParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {})
    }
}

impl<T: Lexer> Parser<T> for TypeParser {
    fn parse(&mut self, ctx: &mut Context<T>, _: AST) -> Result<T> {
        let token = self.expect_one_of(
            ctx,
            vec![
                TokenKind::Ident("".to_string()),
                TokenKind::I8,
                TokenKind::I16,
                TokenKind::I32,
                TokenKind::I64,
                TokenKind::U8,
                TokenKind::U16,
                TokenKind::U32,
                TokenKind::U64,
                TokenKind::Bool,
            ],
        )?;

        if let TokenKind::Ident(_) = token.kind {
            return Ok(ParseResult::AST(AST::Type(Type::Ident(token))));
        }

        Ok(ParseResult::AST(AST::Type(Type::Primitive(token))))
    }
}

pub struct TypeDeclParser {
    name: Option<Token>,
}

impl TypeDeclParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { name: None })
    }
}

impl<T: Lexer> Parser<T> for TypeDeclParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Type(typ) = data {
            return Ok(ParseResult::AST(AST::Declaration(Declaration::Type {
                name: self.name.take().unwrap(),
                typ,
            })));
        }

        self.expect(ctx, TokenKind::Type)?;
        self.name = Some(self.expect(ctx, TokenKind::Ident("".to_string()))?);
        Ok(ParseResult::Push(TypeParser::new()))
    }
}
