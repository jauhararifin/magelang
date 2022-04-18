use super::struct_parser::StructParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::token::{Lexer, Token, TokenKind};

pub struct TypeParser {
    is_pointer: bool,
}

impl TypeParser {
    pub fn new() -> Box<Self> {
        Box::new(Self { is_pointer: false })
    }
}

impl<T: Lexer> Parser<T> for TypeParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Type(typ) = data {
            let mut result = typ;
            if self.is_pointer {
                result = Type::Pointer(Pointer {
                    elem: Box::new(result),
                });
            }
            return Ok(ParseResult::AST(AST::Type(result)));
        }

        if ctx.lexer.peek()?.kind == TokenKind::Struct {
            return Ok(ParseResult::Push(StructParser::new()));
        }

        if ctx.lexer.peek()?.kind == TokenKind::Mul {
            return Ok(ParseResult::Push(TypeParser::new()));
        }

        let token = self.expect_one_of(
            ctx,
            vec![
                TokenKind::Ident,
                TokenKind::I8,
                TokenKind::I16,
                TokenKind::I32,
                TokenKind::I64,
                TokenKind::U8,
                TokenKind::U16,
                TokenKind::U32,
                TokenKind::U64,
                TokenKind::Bool,
                TokenKind::Struct,
            ],
        )?;

        if TokenKind::Ident == token.kind {
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
            return Ok(ParseResult::AST(AST::TypeDecl(TypeDecl {
                name: self.name.take().unwrap(),
                typ,
            })));
        }

        self.expect(ctx, TokenKind::Type)?;
        self.name = Some(self.expect(ctx, TokenKind::Ident)?);
        Ok(ParseResult::Push(TypeParser::new()))
    }
}
