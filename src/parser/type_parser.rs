use super::struct_parser::StructParser;
use super::{Ast, Context, ParseResult, Parser, Result};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct TypeParser {
    state: TypeParserState,
}

enum TypeParserState {
    Init,
    Struct,
}

impl TypeParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: TypeParserState::Init,
        })
    }

    fn parse_init<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<T> {
        let token = ctx.lexer.peek()?;

        match token.kind {
            TokenKind::Struct => {
                self.state = TypeParserState::Struct;
                Ok(ParseResult::Push(StructParser::new()))
            }
            TokenKind::Ident => {
                let token = ctx.lexer.next()?;
                Ok(ParseResult::Ast(Ast::Type(Type::Ident(token))))
            }
            TokenKind::I8
            | TokenKind::I16
            | TokenKind::I32
            | TokenKind::I64
            | TokenKind::U8
            | TokenKind::U16
            | TokenKind::U32
            | TokenKind::U64
            | TokenKind::Bool => {
                let token = ctx.lexer.next()?;
                Ok(ParseResult::Ast(Ast::Type(Type::Primitive(token))))
            }
            _ => {
                let token = ctx.lexer.next()?;
                Err(Error::UnexpectedToken {
                    expected: vec![
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
                        TokenKind::Mul,
                    ],
                    found: token,
                })
            }
        }
    }

    fn parse_struct<T: Lexer>(&mut self, data: Ast) -> Result<T> {
        let t = Struct::from(data);
        Ok(ParseResult::Ast(Ast::Type(Type::Struct(t))))
    }
}

impl<T: Lexer> Parser<T> for TypeParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match self.state {
            TypeParserState::Init => self.parse_init(ctx),
            TypeParserState::Struct => self.parse_struct(data),
        }
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
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        if let Ast::Type(typ) = data {
            return Ok(ParseResult::Ast(Ast::TypeDecl(TypeDecl {
                name: self.name.take().unwrap(),
                typ,
            })));
        }

        self.expect(ctx, TokenKind::Type)?;
        self.name = Some(self.expect(ctx, TokenKind::Ident)?);
        Ok(ParseResult::Push(TypeParser::new()))
    }
}
