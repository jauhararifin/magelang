use super::expr_parser::ExprParser;
use super::{Context, ParseResult, Parser, Result, Ast};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct AssignParser {
    state: AssignParserState,
    receiver: Option<Expr>,
    assign_token: Option<Token>,
}

enum AssignParserState {
    Receiver,
    AssignOp,
    Value,
}

impl AssignParser {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            state: AssignParserState::Receiver,
            receiver: None,
            assign_token: None,
        })
    }

    fn parse_receiver<T: Lexer>(&mut self) -> Result<T> {
        self.state = AssignParserState::AssignOp;
        Ok(ParseResult::Push(ExprParser::new()))
    }

    fn parse_assign_op<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        let expr: Expr = data.into();

        if let Some(assign_token) = self.check_one_of(
            ctx,
            vec![
                TokenKind::Assign,
                TokenKind::BitAndAssign,
                TokenKind::BitOrAssign,
                TokenKind::BitXorAssign,
                TokenKind::PlusAssign,
                TokenKind::MinusAssign,
                TokenKind::MulAssign,
                TokenKind::ModAssign,
                TokenKind::ShrAssign,
                TokenKind::ShlAssign,
            ],
        )? {
            self.receiver = Some(expr);
            self.assign_token = Some(assign_token);
            self.state = AssignParserState::Value;
            Ok(ParseResult::Push(ExprParser::new()))
        } else {
            Ok(ParseResult::Ast(Ast::Statement(Statement::Expr(expr))))
        }
    }

    fn parse_value<T: Lexer>(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        let value = Expr::from(data);
        self.consume_endl(ctx)?;
        Ok(ParseResult::Ast(Ast::Assign(Assign {
            receiver: self.receiver.take().unwrap(),
            op: self.assign_token.take().unwrap(),
            value,
        })))
    }
}

impl<T: Lexer> Parser<T> for AssignParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: Ast) -> Result<T> {
        match self.state {
            AssignParserState::Receiver => self.parse_receiver(),
            AssignParserState::AssignOp => self.parse_assign_op(ctx, data),
            AssignParserState::Value => self.parse_value(ctx, data),
        }
    }
}
