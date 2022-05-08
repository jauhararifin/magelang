use crate::{
    ast::{ArrayTypeNode, BinaryNode, CastNode, ExprNode, ExprNodeKind, FunctionCallNode, IndexNode, UnaryNode},
    errors::Error,
    lexer::LexerHelper,
    token::{Token, TokenKind},
};

pub trait IExprParserHelper {
    fn parse(&mut self) -> Result<ExprNode, Error>;
}

pub struct ExprParserHelper<'a> {
    lexer: &'a mut dyn LexerHelper,
    allow_empty: bool,
    stack: Vec<State>,
}

impl<'a> ExprParserHelper<'a> {
    pub fn new(lexer: &'a mut dyn LexerHelper, allow_empty: bool) -> Self {
        Self {
            lexer,
            allow_empty,
            stack: vec![],
        }
    }
}

impl<'a> IExprParserHelper for ExprParserHelper<'a> {
    fn parse(&mut self) -> Result<ExprNode, Error> {
        self.stack.push(State::Expr);

        let mut data = None;
        while let Some(state) = self.stack.pop() {
            data = self.parse_state(state, data)?;
        }

        Ok(data.unwrap())
    }
}

// priority (from lowest to highest):
// logical_or
// logical_and
// bit_or
// bit_xor
// bit_and
// eq, neq
// lt, lteq, gt, gteq
// shl, shr
// plus, minus
// mul, div, mod
// index
// cast
// unary not, minus, plus, bit_not
// call
// array
// primary: braces, ident, literals

enum State {
    Expr,
    Binary {
        kind: BinaryKind,
    },
    BinaryA {
        kind: BinaryKind,
    },
    BinaryB {
        kind: BinaryKind,
        a: Box<ExprNode>,
        op: Token,
    },
    Index,
    IndexIndex {
        array: Box<ExprNode>,
        index: Vec<ExprNode>,
    },
    Cast,
    CastType {
        value: Box<ExprNode>,
        as_token: Token,
    },
    Unary,
    UnaryVal {
        op: Token,
    },
    Call,
    CallParam {
        func: Box<ExprNode>,
        args: Vec<ExprNode>,
    },
    Array,
    ArrayType {
        open_brack: Token,
        dimension: usize,
    },
    Primary,
    Brace,
    BraceBody,
}

#[derive(Copy, Clone)]
enum BinaryKind {
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Equality,
    Comparison,
    Shifts,
    Addition,
    Multiplication,
}

type ParseResult = Result<Option<ExprNode>, Error>;
type ParseData = Option<ExprNode>;

impl<'a> ExprParserHelper<'a> {
    fn parse_state(&mut self, state: State, data: ParseData) -> ParseResult {
        match state {
            State::Expr => self.parse_expr(data),
            State::Binary { kind } => self.parse_binary(kind),
            State::BinaryA { kind } => self.parse_binary_a(kind, data),
            State::BinaryB { kind, a, op } => self.parse_binary_b(kind, a, op, data),
            State::Index => self.parse_index(data),
            State::IndexIndex { array, index } => self.parse_index_index(array, index, data),
            State::Cast => self.parse_cast(data),
            State::CastType { value, as_token } => self.parse_cast_type(value, as_token, data),
            State::Unary => self.parse_unary(),
            State::UnaryVal { op } => self.parse_unary_val(op, data),
            State::Call => self.parse_call(data),
            State::CallParam { func, args } => self.parse_call_params(func, args, data),
            State::Array => self.parse_array(),
            State::ArrayType { open_brack, dimension } => self.parse_array_type(open_brack, dimension, data),
            State::Primary => self.parse_primary(),
            State::Brace => self.parse_brace(),
            State::BraceBody => self.parse_brace_body(data),
        }
    }

    fn parse_expr(&mut self, data: ParseData) -> ParseResult {
        if let Some(expr) = data {
            Ok(Some(expr))
        } else {
            self.stack.push(State::Expr);
            self.stack.push(State::Binary { kind: BinaryKind::Or });
            Ok(None)
        }
    }

    fn parse_binary(&mut self, kind: BinaryKind) -> ParseResult {
        self.stack.push(State::BinaryA { kind });
        if let Some(next_kind) = self.binary_next_kind(kind) {
            self.stack.push(State::Binary { kind: next_kind });
        } else {
            self.stack.push(State::Index);
        }
        Ok(None)
    }

    fn parse_binary_a(&mut self, kind: BinaryKind, data: ParseData) -> ParseResult {
        let expr = data.unwrap();
        self.parse_binary_op(kind, expr)
    }

    fn parse_binary_b(&mut self, kind: BinaryKind, a: Box<ExprNode>, op: Token, data: ParseData) -> ParseResult {
        let b = Box::new(data.unwrap());
        let expr = ExprNode {
            pos: a.pos.clone(),
            kind: ExprNodeKind::Binary(BinaryNode { a, op, b }),
        };

        self.parse_binary_op(kind, expr)
    }

    fn parse_binary_op(&mut self, kind: BinaryKind, expr: ExprNode) -> ParseResult {
        let ops = self.binary_kind_op(kind);
        let op = self.lexer.next_in(&ops)?;
        if op.is_none() {
            return Ok(Some(expr));
        }

        let op = op.unwrap();
        self.stack.push(State::BinaryB {
            kind,
            a: Box::new(expr),
            op,
        });
        if let Some(next_kind) = self.binary_next_kind(kind) {
            self.stack.push(State::Binary { kind: next_kind });
        } else {
            self.stack.push(State::Index);
        }
        Ok(None)
    }

    fn binary_next_kind(&self, kind: BinaryKind) -> Option<BinaryKind> {
        match kind {
            BinaryKind::Or => Some(BinaryKind::And),
            BinaryKind::And => Some(BinaryKind::BitOr),
            BinaryKind::BitOr => Some(BinaryKind::BitXor),
            BinaryKind::BitXor => Some(BinaryKind::BitAnd),
            BinaryKind::BitAnd => Some(BinaryKind::Equality),
            BinaryKind::Equality => Some(BinaryKind::Comparison),
            BinaryKind::Comparison => Some(BinaryKind::Shifts),
            BinaryKind::Shifts => Some(BinaryKind::Addition),
            BinaryKind::Addition => Some(BinaryKind::Multiplication),
            BinaryKind::Multiplication => None,
        }
    }

    fn binary_kind_op(&self, kind: BinaryKind) -> Vec<TokenKind> {
        match kind {
            BinaryKind::Or => vec![TokenKind::Or],
            BinaryKind::And => vec![TokenKind::And],
            BinaryKind::BitOr => vec![TokenKind::BitOr],
            BinaryKind::BitXor => vec![TokenKind::BitXor],
            BinaryKind::BitAnd => vec![TokenKind::BitAnd],
            BinaryKind::Equality => vec![TokenKind::Eq, TokenKind::NotEq],
            BinaryKind::Comparison => vec![TokenKind::GT, TokenKind::LT, TokenKind::GTEq, TokenKind::LTEq],
            BinaryKind::Shifts => vec![TokenKind::Shl, TokenKind::Shr],
            BinaryKind::Addition => vec![TokenKind::Plus, TokenKind::Minus],
            BinaryKind::Multiplication => vec![TokenKind::Mul, TokenKind::Div, TokenKind::Mod],
        }
    }

    fn parse_index(&mut self, data: ParseData) -> ParseResult {
        if data.is_none() {
            self.stack.push(State::Index);
            self.stack.push(State::Cast);
            return Ok(None);
        }

        let expr = data.unwrap();
        if self.lexer.next_is(&TokenKind::OpenBrack)?.is_some() {
            self.stack.push(State::IndexIndex {
                array: Box::new(expr),
                index: vec![],
            });
            Ok(None)
        } else {
            Ok(Some(expr))
        }
    }

    fn parse_index_index(&mut self, array: Box<ExprNode>, mut index: Vec<ExprNode>, data: ParseData) -> ParseResult {
        if let Some(item) = data {
            index.push(item);

            self.lexer.expect(TokenKind::CloseBrack)?;
            if self.lexer.next_is(&TokenKind::OpenBrack)?.is_some() {
                self.stack.push(State::IndexIndex { array, index });
                return Ok(None);
            }

            Ok(Some(ExprNode {
                pos: array.pos.clone(),
                kind: ExprNodeKind::Index(IndexNode { array, index }),
            }))
        } else {
            self.stack.push(State::IndexIndex { array, index });
            self.stack.push(State::Expr);
            Ok(None)
        }
    }

    fn parse_cast(&mut self, data: ParseData) -> ParseResult {
        if data.is_none() {
            self.stack.push(State::Cast);
            self.stack.push(State::Unary);
            return Ok(None);
        }

        let expr = data.unwrap();
        if let Some(as_token) = self.lexer.next_is(&TokenKind::As)? {
            self.stack.push(State::CastType {
                value: Box::new(expr),
                as_token,
            });
            Ok(None)
        } else {
            Ok(Some(expr))
        }
    }

    fn parse_cast_type(&mut self, value: Box<ExprNode>, as_token: Token, data: ParseData) -> ParseResult {
        if let Some(target) = data {
            let target = Box::new(target);
            Ok(Some(ExprNode {
                pos: value.pos.clone(),
                kind: ExprNodeKind::Cast(CastNode {
                    value,
                    as_token,
                    target,
                }),
            }))
        } else {
            self.stack.push(State::CastType { value, as_token });
            self.stack.push(State::Expr);
            Ok(None)
        }
    }

    fn parse_unary(&mut self) -> ParseResult {
        if let Some(op) = self
            .lexer
            .next_in(&[TokenKind::Plus, TokenKind::Minus, TokenKind::Not, TokenKind::BitNot])?
        {
            self.stack.push(State::UnaryVal { op });
        } else {
            self.stack.push(State::Call);
        }
        Ok(None)
    }

    fn parse_unary_val(&mut self, op: Token, data: ParseData) -> ParseResult {
        if let Some(expr) = data {
            Ok(Some(ExprNode {
                pos: op.pos.clone(),
                kind: ExprNodeKind::Unary(UnaryNode {
                    op,
                    val: Box::new(expr),
                }),
            }))
        } else {
            self.stack.push(State::UnaryVal { op });
            self.stack.push(State::Call);
            Ok(None)
        }
    }

    fn parse_call(&mut self, data: ParseData) -> ParseResult {
        if data.is_none() {
            self.stack.push(State::Call);
            self.stack.push(State::Array);
            return Ok(None);
        }

        let expr = data.unwrap();
        if self.lexer.next_is(&TokenKind::OpenBrace)?.is_some() {
            self.stack.push(State::CallParam {
                func: Box::new(expr),
                args: vec![],
            });
            Ok(None)
        } else {
            Ok(Some(expr))
        }
    }

    fn parse_call_params(&mut self, func: Box<ExprNode>, mut args: Vec<ExprNode>, data: ParseData) -> ParseResult {
        let mut first_param = true;
        if let Some(expr) = data {
            args.push(expr);
            first_param = false;
        }

        if self.lexer.next_is(&TokenKind::CloseBrace)?.is_some() {
            return Ok(Some(ExprNode {
                pos: func.pos.clone(),
                kind: ExprNodeKind::FunctionCall(FunctionCallNode { func, args }),
            }));
        }

        if first_param || self.lexer.next_is(&TokenKind::Comma)?.is_some() {
            self.stack.push(State::CallParam { func, args });
            self.stack.push(State::Expr);
            return Ok(None);
        }

        Err(Error::UnexpectedToken {
            expected: vec![TokenKind::Comma, TokenKind::CloseBrace],
            found: self.lexer.next()?,
        })
    }

    fn parse_array(&mut self) -> ParseResult {
        if let Some(open_brack) = self.lexer.next_is(&TokenKind::OpenBrack)? {
            self.lexer.expect(TokenKind::CloseBrack)?;

            let mut dimension = 1usize;
            while self.lexer.next_is(&TokenKind::OpenBrace)?.is_some() {
                self.lexer.expect(TokenKind::CloseBrack)?;
                dimension += 1;
            }

            self.stack.push(State::ArrayType { open_brack, dimension });
        } else {
            self.stack.push(State::Primary);
        }
        Ok(None)
    }

    fn parse_array_type(&mut self, open_brack: Token, dimension: usize, data: ParseData) -> ParseResult {
        if let Some(elem) = data {
            Ok(Some(ExprNode {
                pos: open_brack.pos.clone(),
                kind: ExprNodeKind::ArrayType(ArrayTypeNode {
                    open_brack,
                    dimension,
                    elem: Box::new(elem),
                }),
            }))
        } else {
            self.stack.push(State::ArrayType { open_brack, dimension });
            self.stack.push(State::Primary);
            Ok(None)
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        let token = self.lexer.peek()?;
        match &token.kind {
            TokenKind::OpenBrace => {
                self.stack.push(State::Brace);
                Ok(None)
            }
            TokenKind::IntegerLit => Ok(Some(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::IntegerLit(self.lexer.next()?),
            })),
            TokenKind::FloatLit => Ok(Some(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::FloatLit(self.lexer.next()?),
            })),
            TokenKind::True | TokenKind::False => Ok(Some(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::BoolLit(self.lexer.next()?),
            })),
            TokenKind::Ident => Ok(Some(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::Ident(self.lexer.next()?),
            })),
            TokenKind::Bool
            | TokenKind::I8
            | TokenKind::I16
            | TokenKind::I32
            | TokenKind::I64
            | TokenKind::U8
            | TokenKind::U16
            | TokenKind::U32
            | TokenKind::U64
            | TokenKind::F32
            | TokenKind::F64 => Ok(Some(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::PrimitiveType(self.lexer.next()?),
            })),
            _ => Err(Error::UnexpectedToken {
                expected: vec![
                    TokenKind::IntegerLit,
                    TokenKind::FloatLit,
                    TokenKind::StringLit,
                    TokenKind::True,
                    TokenKind::False,
                    TokenKind::OpenBrace,
                    TokenKind::Ident,
                ],
                found: self.lexer.next()?,
            }),
        }
    }

    fn parse_brace(&mut self) -> ParseResult {
        self.lexer.expect(TokenKind::OpenBrace)?;
        self.stack.push(State::BraceBody);
        Ok(None)
    }

    fn parse_brace_body(&mut self, data: ParseData) -> ParseResult {
        if let Some(expr) = data {
            self.lexer.expect(TokenKind::CloseBrace)?;
            Ok(Some(expr))
        } else {
            self.stack.push(State::BraceBody);
            self.stack.push(State::Expr);
            Ok(None)
        }
    }
}
