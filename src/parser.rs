use crate::ast::{
    AssignNode, BinaryNode, BlockStatementNode, CastNode, DeclNode, ExprNode, ExprNodeKind, FnDeclNode, FnHeaderNode,
    FunctionCallNode, IfNode, ParamNode, ReturnNode, RootNode, StatementNode, TypeNode, UnaryNode, VarNode, WhileNode,
};
use crate::errors::Error;
use crate::lexer::ILexer;
use crate::token::{Token, TokenKind};

pub trait Parser {
    fn parse(&mut self) -> std::result::Result<RootNode, Error>;
}

pub struct SimpleParser<T: ILexer> {
    lexer: T,
    stack: Vec<ParsingState>,
}

#[derive(Debug)]
enum ParsingState {
    Root,
    RootDecl {
        declarations: Vec<DeclNode>,
    },
    Var,
    VarType {
        name: Token,
    },
    VarValue {
        name: Token,
        typ: TypeNode,
    },
    FnName,
    FnParam {
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
    },
    FnReturn {
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
    },
    FnBody {
        fn_token: Token,
        name: Token,
        params: Vec<ParamNode>,
        ret_type: Option<TypeNode>,
    },
    ParamName,
    ParamType {
        name: Token,
    },
    Type,
    Statement,
    BlockStatement {
        body: Vec<StatementNode>,
    },
    AssignStatement,
    AssignStatementValue {
        receiver: ExprNode,
        op: Token,
    },
    IfStatement,
    IfStatementCond {
        if_token: Token,
    },
    IfStatementBody {
        if_token: Token,
        cond: ExprNode,
    },
    WhileStatement,
    WhileStatementCond {
        while_token: Token,
    },
    WhileStatementBody {
        while_token: Token,
        cond: ExprNode,
    },
    ReturnStatement,
    ReturnStatementValue {
        return_token: Token,
    },
    Expr {
        allow_empty: bool,
    },
    UnaryExpr {
        allow_empty: bool,
    },
    UnaryExprVal {
        op: Token,
    },
    BinaryExpr {
        allow_empty: bool,
    },
    BinaryExprOperand {
        allow_empty: bool,
        a: ExprNode,
        op: Token,
    },
    CastExpr {
        allow_empty: bool,
    },
    CastExprType {
        allow_empty: bool,
        val: ExprNode,
        as_token: Token,
    },
    CallExpr {
        allow_empty: bool,
    },
    CallExprParams {
        allow_empty: bool,
        func: ExprNode,
        args: Vec<ExprNode>,
    },
    PrimaryExpr {
        allow_empty: bool,
    },
}

#[derive(Debug)]
enum Ast {
    Root(RootNode),
    FnDecl(FnDeclNode),
    Var(VarNode),
    Param(ParamNode),
    Type(TypeNode),
    Statement(StatementNode),
    Assign(AssignNode),
    If(IfNode),
    While(WhileNode),
    Return(ReturnNode),
    Expr(ExprNode),
    BlockStatement(BlockStatementNode),
    Empty,
}

impl<T: ILexer> SimpleParser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            lexer,
            stack: Vec::new(),
        }
    }

    fn parse_state(&mut self, state: ParsingState, data: Ast) -> Result<Ast, Error> {
        match state {
            ParsingState::Root => self.parse_root(),
            ParsingState::RootDecl { declarations } => self.parse_root_decl(declarations, data),
            ParsingState::Var => self.parse_var(),
            ParsingState::VarType { name } => self.parse_var_type(name, data),
            ParsingState::VarValue { name, typ } => self.parse_var_value(name, typ, data),
            ParsingState::FnName => self.parse_fn_name(),
            ParsingState::FnParam {
                fn_token,
                name,
                native_token,
                params,
            } => self.parse_fn_param(fn_token, name, native_token, params, data),
            ParsingState::FnReturn {
                fn_token,
                name,
                native_token,
                params,
            } => self.parse_fn_return(fn_token, name, native_token, params, data),
            ParsingState::FnBody {
                fn_token,
                name,
                params,
                ret_type,
            } => self.parse_fn_body(fn_token, name, params, ret_type, data),
            ParsingState::ParamName => self.parse_param_name(),
            ParsingState::ParamType { name } => self.parse_param_type(name, data),
            ParsingState::Type => self.parse_type(),
            ParsingState::Statement => self.parse_statement(data),
            ParsingState::BlockStatement { body } => self.parse_block_statement(body, data),
            ParsingState::AssignStatement => self.parse_assign_statement(data),
            ParsingState::AssignStatementValue { receiver, op } => {
                self.parse_assign_value_statement(receiver, op, data)
            }
            ParsingState::IfStatement => self.parse_if_statement(),
            ParsingState::IfStatementCond { if_token } => self.parse_if_statement_cond(if_token, data),
            ParsingState::IfStatementBody { if_token, cond } => self.parse_if_statement_body(if_token, cond, data),
            ParsingState::WhileStatement => self.parse_while_statement(),
            ParsingState::WhileStatementCond { while_token } => self.parse_while_statement_cond(while_token, data),
            ParsingState::WhileStatementBody { while_token, cond } => {
                self.parse_while_statement_body(while_token, cond, data)
            }
            ParsingState::ReturnStatement => self.parse_return_statement(),
            ParsingState::ReturnStatementValue { return_token } => {
                self.parse_return_value_statement(return_token, data)
            }
            ParsingState::Expr { allow_empty } => self.parse_expr(allow_empty, data),
            ParsingState::UnaryExpr { allow_empty } => self.parse_unary_expr(allow_empty),
            ParsingState::UnaryExprVal { op } => self.parse_unary_expr_val(op, data),
            ParsingState::BinaryExpr { allow_empty } => self.parse_binary_expr(allow_empty, data),
            ParsingState::BinaryExprOperand { allow_empty, a, op } => {
                self.parse_binary_expr_operand(allow_empty, a, op, data)
            }
            ParsingState::CastExpr { allow_empty } => self.parse_cast_expr(allow_empty, data),
            ParsingState::CastExprType {
                allow_empty,
                val,
                as_token,
            } => self.parse_cast_expr_type(allow_empty, val, as_token, data),
            ParsingState::CallExpr { allow_empty } => self.parse_call_expr(allow_empty, data),
            ParsingState::CallExprParams {
                allow_empty,
                func,
                args,
            } => self.parse_call_expr_params(allow_empty, func, args, data),
            ParsingState::PrimaryExpr { allow_empty } => self.parse_primary_expr(allow_empty, data),
        }
    }

    fn parse_root(&mut self) -> Result<Ast, Error> {
        self.consume_endl()?;
        self.stack.push(ParsingState::RootDecl { declarations: vec![] });
        Ok(Ast::Empty)
    }

    fn parse_root_decl(&mut self, mut declarations: Vec<DeclNode>, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::FnDecl(decl) => declarations.push(DeclNode::Fn(decl)),
            Ast::Empty => (),
            _ => panic!("got {:?} instead of declaration", data),
        }

        self.consume_endl()?;

        let token = self.lexer.peek()?;
        match token.kind {
            TokenKind::Eoi => Ok(Ast::Root(RootNode { declarations })),
            TokenKind::Fn => {
                self.stack.push(ParsingState::RootDecl { declarations });
                self.stack.push(ParsingState::FnName);
                Ok(Ast::Empty)
            }
            _ => {
                let token = self.lexer.next()?;
                Err(Error::UnexpectedToken {
                    expected: vec![TokenKind::Fn, TokenKind::Var],
                    found: token,
                })
            }
        }
    }

    fn parse_var(&mut self) -> Result<Ast, Error> {
        self.expect(TokenKind::Var)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        self.stack.push(ParsingState::VarType { name });
        Ok(Ast::Empty)
    }

    fn parse_var_type(&mut self, name: Token, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::VarType { name });
                self.stack.push(ParsingState::Type);
                Ast::Empty
            }
            Ast::Type(typ) => {
                if self.check(&TokenKind::Assign)?.is_some() {
                    self.stack.push(ParsingState::VarValue { name, typ });
                    Ast::Empty
                } else {
                    Ast::Var(VarNode { name, typ, value: None })
                }
            }
            _ => panic!("invalid data when parsing var type statement: {:?}", data),
        })
    }

    fn parse_var_value(&mut self, name: Token, typ: TypeNode, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::VarValue { name, typ });
                self.stack.push(ParsingState::Expr { allow_empty: false });
                Ast::Empty
            }
            Ast::Expr(expr) => Ast::Var(VarNode {
                name,
                typ,
                value: Some(expr),
            }),
            _ => panic!("invalid data when parsing var value statement: {:?}", data),
        })
    }

    fn parse_fn_name(&mut self) -> Result<Ast, Error> {
        let fn_token = self.expect(TokenKind::Fn)?;
        let native_token = self.check(&TokenKind::Native)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenBrace)?;

        self.stack.push(ParsingState::FnParam {
            fn_token,
            name,
            native_token,
            params: Vec::new(),
        });
        Ok(Ast::Empty)
    }

    fn parse_fn_param(
        &mut self,
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        mut params: Vec<ParamNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        let mut first_param = true;
        if let Ast::Param(param) = data {
            params.push(param);
            first_param = false;
        }

        if self.check(&TokenKind::CloseBrace)?.is_some() {
            if self.check(&TokenKind::Colon)?.is_none() {
                if native_token.is_none() {
                    self.stack.push(ParsingState::FnBody {
                        fn_token,
                        name,
                        params,
                        ret_type: None,
                    });
                    return Ok(Ast::Empty);
                }
                return Ok(Ast::FnDecl(FnDeclNode {
                    fn_token,
                    name,
                    header: FnHeaderNode {
                        native_token,
                        params,
                        ret_type: None,
                    },
                    body: None,
                }));
            }

            self.stack.push(ParsingState::FnReturn {
                fn_token,
                name,
                native_token,
                params,
            });
            return Ok(Ast::Empty);
        }

        if !first_param {
            self.expect(TokenKind::Comma)?;
        }

        self.stack.push(ParsingState::FnParam {
            fn_token,
            name,
            native_token,
            params,
        });
        self.stack.push(ParsingState::ParamName);

        Ok(Ast::Empty)
    }

    fn parse_fn_return(
        &mut self,
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::Type(typ) = data {
            if native_token.is_none() {
                self.stack.push(ParsingState::FnBody {
                    fn_token,
                    name,
                    params,
                    ret_type: Some(typ),
                });
                return Ok(Ast::Empty);
            }
            return Ok(Ast::FnDecl(FnDeclNode {
                fn_token,
                name,
                header: FnHeaderNode {
                    native_token,
                    params,
                    ret_type: Some(typ),
                },
                body: None,
            }));
        }

        self.stack.push(ParsingState::FnReturn {
            fn_token,
            name,
            native_token,
            params,
        });
        self.stack.push(ParsingState::Type);

        Ok(Ast::Empty)
    }

    fn parse_fn_body(
        &mut self,
        fn_token: Token,
        name: Token,
        params: Vec<ParamNode>,
        ret_type: Option<TypeNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::BlockStatement(body) = data {
            return Ok(Ast::FnDecl(FnDeclNode {
                fn_token,
                name,
                header: FnHeaderNode {
                    native_token: None,
                    params,
                    ret_type,
                },
                body: Some(body),
            }));
        }

        self.stack.push(ParsingState::FnBody {
            fn_token,
            name,
            params,
            ret_type,
        });
        self.stack.push(ParsingState::BlockStatement { body: vec![] });

        Ok(Ast::Empty)
    }

    fn parse_param_name(&mut self) -> Result<Ast, Error> {
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        self.stack.push(ParsingState::ParamType { name });
        Ok(Ast::Empty)
    }

    fn parse_param_type(&mut self, name: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Type(typ) = data {
            return Ok(Ast::Param(ParamNode { name, typ }));
        }
        self.stack.push(ParsingState::ParamType { name });
        self.stack.push(ParsingState::Type);
        Ok(Ast::Empty)
    }

    fn parse_type(&mut self) -> Result<Ast, Error> {
        let token = self.lexer.peek()?;
        return match &token.kind {
            TokenKind::I8
            | TokenKind::I16
            | TokenKind::I32
            | TokenKind::I64
            | TokenKind::U8
            | TokenKind::U16
            | TokenKind::U32
            | TokenKind::U64
            | TokenKind::F32
            | TokenKind::F64
            | TokenKind::Bool => {
                let token = self.lexer.next()?;
                Ok(Ast::Type(TypeNode::Primitive(token)))
            }
            _ => {
                let token = self.lexer.next()?;
                Err(Error::UnexpectedToken {
                    expected: vec![
                        TokenKind::I8,
                        TokenKind::I16,
                        TokenKind::I32,
                        TokenKind::I64,
                        TokenKind::U8,
                        TokenKind::U16,
                        TokenKind::U32,
                        TokenKind::U64,
                        TokenKind::F32,
                        TokenKind::F64,
                        TokenKind::Bool,
                    ],
                    found: token,
                })
            }
        };
    }

    fn parse_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Var(stmt) => return Ok(Ast::Statement(StatementNode::Var(stmt))),
            Ast::Assign(stmt) => return Ok(Ast::Statement(StatementNode::Assign(stmt))),
            Ast::Return(stmt) => return Ok(Ast::Statement(StatementNode::Return(stmt))),
            Ast::If(stmt) => return Ok(Ast::Statement(StatementNode::If(stmt))),
            Ast::While(stmt) => return Ok(Ast::Statement(StatementNode::While(stmt))),
            Ast::BlockStatement(stmt) => return Ok(Ast::Statement(StatementNode::Block(stmt))),
            Ast::Expr(stmt) => return Ok(Ast::Statement(StatementNode::Expr(stmt))),
            Ast::Statement(stmt) => return Ok(Ast::Statement(stmt)),
            Ast::Empty => (),
            _ => panic!("invalid data when parsing statement: {:?}", data),
        }

        self.consume_endl()?;

        let next_parser = match &self.lexer.peek()?.kind {
            TokenKind::Var => ParsingState::Var,
            TokenKind::While => ParsingState::WhileStatement,
            TokenKind::If => ParsingState::IfStatement,
            TokenKind::Return => ParsingState::ReturnStatement,
            TokenKind::OpenBlock => ParsingState::BlockStatement { body: vec![] },
            _ => ParsingState::AssignStatement,
        };

        self.stack.push(ParsingState::Statement);
        self.stack.push(next_parser);

        Ok(Ast::Empty)
    }

    fn parse_block_statement(&mut self, mut body: Vec<StatementNode>, data: Ast) -> Result<Ast, Error> {
        self.consume_endl()?;

        match data {
            Ast::Empty => {
                self.expect(TokenKind::OpenBlock)?;
            }
            Ast::Statement(statement) => {
                body.push(statement);
            }
            _ => panic!("invalid data when parsing block statement: {:?}", data),
        }

        self.consume_endl()?;

        let result = if self.check(&TokenKind::CloseBlock)?.is_some() {
            Ast::BlockStatement(BlockStatementNode { body })
        } else {
            self.stack.push(ParsingState::BlockStatement { body });
            self.stack.push(ParsingState::Statement);
            Ast::Empty
        };

        Ok(result)
    }

    fn parse_assign_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Expr(receiver) => {
                let op = self.check_one_of(vec![
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
                ])?;
                if let Some(op) = op {
                    self.stack.push(ParsingState::AssignStatementValue { receiver, op });
                    Ok(Ast::Empty)
                } else {
                    Ok(Ast::Expr(receiver))
                }
            }
            Ast::Empty => {
                self.stack.push(ParsingState::AssignStatement);
                self.stack.push(ParsingState::Expr { allow_empty: false });
                Ok(Ast::Empty)
            }
            _ => panic!("invalid data when parsing assign statement: {:?}", data),
        }
    }

    fn parse_assign_value_statement(&mut self, receiver: ExprNode, op: Token, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Expr(value) => Ok(Ast::Assign(AssignNode { receiver, op, value })),
            Ast::Empty => {
                self.stack.push(ParsingState::AssignStatementValue { receiver, op });
                self.stack.push(ParsingState::Expr { allow_empty: false });
                Ok(Ast::Empty)
            }
            _ => panic!("invalid data when parsing assign value statement: {:?}", data),
        }
    }

    fn parse_if_statement(&mut self) -> Result<Ast, Error> {
        let if_token = self.expect(TokenKind::If)?;
        self.stack.push(ParsingState::IfStatementCond { if_token });
        self.stack.push(ParsingState::Expr { allow_empty: false });
        Ok(Ast::Empty)
    }

    fn parse_if_statement_cond(&mut self, if_token: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(cond) = data {
            self.stack.push(ParsingState::IfStatementBody { if_token, cond });
            Ok(Ast::Empty)
        } else {
            panic!("invalid data when parsing if statement cond: {:?}", data);
        }
    }

    fn parse_if_statement_body(&mut self, if_token: Token, cond: ExprNode, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::IfStatementBody { if_token, cond });
                self.stack.push(ParsingState::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::If(IfNode { if_token, cond, body }),
            _ => panic!("invalid data when parsing if statement body: {:?}", data),
        })
    }

    fn parse_while_statement(&mut self) -> Result<Ast, Error> {
        let while_token = self.expect(TokenKind::While)?;
        self.stack.push(ParsingState::WhileStatementCond { while_token });
        self.stack.push(ParsingState::Expr { allow_empty: false });
        Ok(Ast::Empty)
    }

    fn parse_while_statement_cond(&mut self, while_token: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(cond) = data {
            self.stack.push(ParsingState::WhileStatementBody { while_token, cond });
            Ok(Ast::Empty)
        } else {
            panic!("invalid data when parsing while statement cond: {:?}", data)
        }
    }

    fn parse_while_statement_body(&mut self, while_token: Token, cond: ExprNode, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::WhileStatementBody { while_token, cond });
                self.stack.push(ParsingState::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::While(WhileNode {
                while_token,
                cond,
                body,
            }),
            _ => panic!("invalid data when parsing while statement body: {:?}", data),
        })
    }

    fn parse_return_statement(&mut self) -> Result<Ast, Error> {
        let return_token = self.expect(TokenKind::Return)?;
        self.stack.push(ParsingState::ReturnStatementValue { return_token });
        self.stack.push(ParsingState::Expr { allow_empty: true });
        Ok(Ast::Empty)
    }

    fn parse_return_value_statement(&mut self, return_token: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(value) = data {
            let value = if let ExprNodeKind::Empty = value.kind {
                None
            } else {
                Some(value)
            };
            return Ok(Ast::Return(ReturnNode { return_token, value }));
        } else {
            panic!("got invalid data while parsing return value statement: {:?}", data);
        }
    }

    fn parse_expr(&mut self, allow_empty: bool, data: Ast) -> Result<Ast, Error> {
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
        // cast
        // unary not, minus, plus, bit_not
        // call
        // struct_lit
        // selector
        // primary: braces, ident, literals
        if let Ast::Expr(expr) = data {
            return Ok(Ast::Expr(expr));
        }

        self.stack.push(ParsingState::Expr { allow_empty });
        self.stack.push(ParsingState::BinaryExpr { allow_empty });
        self.stack.push(ParsingState::CastExpr { allow_empty });
        self.stack.push(ParsingState::UnaryExpr { allow_empty });

        Ok(Ast::Empty)
    }

    fn parse_binary_expr(&mut self, allow_empty: bool, data: Ast) -> Result<Ast, Error> {
        let binary_precedence = vec![
            vec![TokenKind::Or],
            vec![TokenKind::And],
            vec![TokenKind::BitOr],
            vec![TokenKind::BitAnd],
            vec![TokenKind::Eq, TokenKind::NotEq],
            vec![TokenKind::LT, TokenKind::LTEq, TokenKind::GT, TokenKind::GTEq],
            vec![TokenKind::Shl, TokenKind::Shr],
            vec![TokenKind::Plus, TokenKind::Minus],
            vec![TokenKind::Mul, TokenKind::Div, TokenKind::Mod],
        ];

        if let Ast::Expr(expr) = data {
            for target_op in binary_precedence.into_iter().rev() {
                if let Some(op) = self.check_one_of(target_op)? {
                    self.stack.push(ParsingState::BinaryExprOperand {
                        allow_empty,
                        a: expr,
                        op,
                    });
                    return Ok(Ast::Empty);
                }
            }
            Ok(Ast::Expr(expr))
        } else {
            panic!("invalid data when parsing binary expr: {:?}", data)
        }
    }

    fn parse_binary_expr_operand(
        &mut self,
        allow_empty: bool,
        a: ExprNode,
        op: Token,
        data: Ast,
    ) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Expr(expr) => Ast::Expr(ExprNode {
                pos: a.pos.clone(),
                kind: ExprNodeKind::Binary(BinaryNode {
                    a: Box::new(a),
                    op,
                    b: Box::new(expr),
                }),
            }),
            Ast::Empty => {
                self.stack.push(ParsingState::BinaryExprOperand { allow_empty, a, op });
                self.stack.push(ParsingState::Expr { allow_empty });
                Ast::Empty
            }
            _ => panic!("invalid data when parsing binary expr operand: {:?}", data),
        })
    }

    fn parse_cast_expr(&mut self, allow_empty: bool, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(val) = data {
            if let Some(as_token) = self.check(&TokenKind::As)? {
                self.stack.push(ParsingState::CastExprType {
                    allow_empty,
                    val,
                    as_token,
                });
                Ok(Ast::Empty)
            } else {
                Ok(Ast::Expr(val))
            }
        } else {
            panic!("invalid data when parsing cast expr: {:?}", data)
        }
    }

    fn parse_cast_expr_type(
        &mut self,
        allow_empty: bool,
        val: ExprNode,
        as_token: Token,
        data: Ast,
    ) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Type(target) => Ast::Expr(ExprNode {
                pos: val.pos.clone(),
                kind: ExprNodeKind::Cast(CastNode {
                    val: Box::new(val),
                    as_token,
                    target,
                }),
            }),
            Ast::Empty => {
                self.stack.push(ParsingState::CastExprType {
                    allow_empty,
                    val,
                    as_token,
                });
                self.stack.push(ParsingState::Type);
                Ast::Empty
            }
            _ => panic!("invalid data when parsing cast expr type: {:?}", data),
        })
    }

    fn parse_unary_expr(&mut self, allow_empty: bool) -> Result<Ast, Error> {
        if let Some(op) = self.check_one_of(vec![
            TokenKind::Not,
            TokenKind::Minus,
            TokenKind::Plus,
            TokenKind::BitNot,
        ])? {
            self.stack.push(ParsingState::UnaryExprVal { op });
        } else {
            self.stack.push(ParsingState::CallExpr { allow_empty });
            self.stack.push(ParsingState::PrimaryExpr { allow_empty });
        }
        Ok(Ast::Empty)
    }

    fn parse_unary_expr_val(&mut self, op: Token, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::UnaryExprVal { op });
                self.stack.push(ParsingState::Expr { allow_empty: false });
                Ast::Empty
            }
            Ast::Expr(val) => Ast::Expr(ExprNode {
                pos: op.pos.clone(),
                kind: ExprNodeKind::Unary(UnaryNode { op, val: Box::new(val) }),
            }),
            _ => panic!("invalid data when parsing unary expr val: {:?}", data),
        })
    }

    fn parse_call_expr(&mut self, allow_empty: bool, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(func) = data {
            if self.check(&TokenKind::OpenBrace)?.is_some() {
                self.stack.push(ParsingState::CallExprParams {
                    allow_empty,
                    func,
                    args: vec![],
                });
                Ok(Ast::Empty)
            } else {
                Ok(Ast::Expr(func))
            }
        } else {
            panic!("invalid data when parsing call expr: {:?}", data)
        }
    }

    fn parse_call_expr_params(
        &mut self,
        allow_empty: bool,
        func: ExprNode,
        mut args: Vec<ExprNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::Expr(arg) = data {
            args.push(arg);
        }

        if let Some(token) = self.check_one_of(vec![TokenKind::Comma, TokenKind::CloseBrace])? {
            if token.kind == TokenKind::Comma {
                self.stack.push(ParsingState::CallExprParams {
                    allow_empty,
                    func,
                    args,
                });
                self.stack.push(ParsingState::Expr { allow_empty });
                return Ok(Ast::Empty);
            }

            return Ok(Ast::Expr(ExprNode {
                pos: func.pos.clone(),
                kind: ExprNodeKind::FunctionCall(FunctionCallNode {
                    func: Box::new(func),
                    args,
                }),
            }));
        }

        self.stack.push(ParsingState::CallExprParams {
            allow_empty,
            func,
            args,
        });
        self.stack.push(ParsingState::Expr { allow_empty });
        Ok(Ast::Empty)
    }

    fn parse_primary_expr(&mut self, allow_empty: bool, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(expr) = data {
            self.expect(TokenKind::CloseBrace)?;
            return Ok(Ast::Expr(expr));
        }

        let token = self.lexer.peek()?;

        match &token.kind {
            TokenKind::IntegerLit => Ok(Ast::Expr(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::IntegerLit(self.lexer.next()?),
            })),
            TokenKind::FloatLit => Ok(Ast::Expr(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::FloatLit(self.lexer.next()?),
            })),
            TokenKind::True | TokenKind::False => Ok(Ast::Expr(ExprNode {
                pos: token.pos.clone(),
                kind: ExprNodeKind::BoolLit(self.lexer.next()?),
            })),
            TokenKind::Ident => {
                Ok(Ast::Expr(ExprNode {
                    pos: token.pos.clone(),
                    kind: ExprNodeKind::Ident(self.lexer.next()?),
                }))
            }
            TokenKind::OpenBrace => {
                self.lexer.next()?;
                self.stack.push(ParsingState::PrimaryExpr { allow_empty: false });
                self.stack.push(ParsingState::Expr { allow_empty: false });
                Ok(Ast::Empty)
            }
            _ => {
                if allow_empty {
                    Ok(Ast::Expr(ExprNode {
                        kind: ExprNodeKind::Empty,
                        pos: token.pos.clone(),
                    }))
                } else {
                    Err(Error::UnexpectedToken {
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
                    })
                }
            }
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        let token = self.lexer.next()?;
        if &token.kind != &kind {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }
        Ok(token)
    }

    fn check(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error> {
        let token = self.lexer.peek()?;
        if &token.kind != kind {
            return Ok(None);
        }

        let token = self.lexer.next()?;
        Ok(Some(token))
    }

    fn check_one_of(&mut self, kind: Vec<TokenKind>) -> Result<Option<Token>, Error> {
        let token = self.lexer.peek()?;

        for k in kind.iter() {
            if &token.kind == k {
                return Ok(Some(self.lexer.next()?));
            }
        }

        Ok(None)
    }

    fn consume_endl(&mut self) -> Result<(), Error> {
        while let TokenKind::Endl = self.lexer.peek()?.kind {
            self.lexer.next()?;
        }
        Ok(())
    }
}

impl<T: ILexer> Parser for SimpleParser<T> {
    fn parse(&mut self) -> std::result::Result<RootNode, Error> {
        self.stack.push(ParsingState::Root);

        let mut data = Ast::Empty;
        while let Some(state) = self.stack.pop() {
            data = self.parse_state(state, data)?;
        }

        if let Ast::Root(result) = data {
            return Ok(result);
        }

        panic!("invalid parsing result");
    }
}
