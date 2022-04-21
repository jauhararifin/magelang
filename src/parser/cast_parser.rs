use super::selector_parser::SelectorParser;
use super::{Context, ParseResult, Parser, Result, AST};
use crate::ast::*;
use crate::parser::type_parser::TypeParser;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct CastParser {
    state: CastParserState,
    val: Option<Expr>,
}

enum CastParserState {
    Expr,
    Type,
}

impl CastParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: CastParserState::Expr,
            val: None,
        })
    }

    fn parse_expr<T: Lexer>(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        if let AST::Expr(expr) = data {
            if self.check(ctx, &TokenKind::As)?.is_some() {
                self.val = Some(expr);
                self.state = CastParserState::Type;
                return Ok(ParseResult::Push(TypeParser::new()));
            }
            return Ok(ParseResult::AST(AST::Expr(expr)));
        }
        return Ok(ParseResult::Push(SelectorParser::new()));
    }

    fn parse_type<T: Lexer>(&mut self, data: AST) -> Result<T> {
        Ok(ParseResult::AST(AST::Expr(Expr {
            pos: self.val.as_ref().unwrap().pos,
            kind: ExprKind::Cast(Cast {
                target: Type::from(data),
                val: Box::new(self.val.take().unwrap()),
            }),
        })))
    }
}

impl<T: Lexer> Parser<T> for CastParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<T> {
        match self.state {
            CastParserState::Expr => self.parse_expr(ctx, data),
            CastParserState::Type => self.parse_type(data),
        }
    }
}
