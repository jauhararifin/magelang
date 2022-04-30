use crate::ast::{
    Assign, BlockStatement, Declaration, Expr, FnDecl, If, Param, Return, Root, Statement, Struct, Type, TypeDecl, Var,
    While,
};
use crate::lexer::{Error as LexerError, Lexer};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub enum Error {
    UnexpectedToken { expected: Vec<TokenKind>, found: Token },
    Lexer(LexerError),
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Self {
        Error::Lexer(err)
    }
}

pub trait Parser {
    fn parse(&mut self) -> std::result::Result<Root, Error>;
}

pub struct SimpleParser<T: Lexer> {
    lexer: T,
    stack: Vec<ParsingState>,
}

#[derive(Debug)]
enum ParsingState {
    Root {
        declarations: Vec<Declaration>,
    },
    Var,
    VarType {
        name: Token,
    },
    VarValue {
        name: Token,
        typ: Type,
    },
    FnName,
    FnParam {
        name: Token,
        params: Vec<Param>,
    },
    FnReturn {
        name: Token,
        params: Vec<Param>,
    },
    FnBody {
        name: Token,
        params: Vec<Param>,
        ret_type: Option<Type>,
    },
    ParamName,
    ParamType {
        name: Token,
    },
    Type,
    StructType {
        fields: Vec<Param>,
    },
    Statement,
    BlockStatement {
        body: Vec<Statement>,
    },
    AssignStatement,
    AssignStatementValue {
        receiver: Expr,
        op: Token,
    },
    IfStatement,
    IfStatementBody {
        cond: Expr,
    },
    WhileStatement,
    WhileStatementBody {
        cond: Expr,
    },
    ExprStatement,
    ReturnStatement,
    Expr,
    // LogicalOrExpr,
    // LogicalAndExpr,
    UnaryExpr,
    BinaryExpr,
    CastExpr,
    CallExpr,
    SelectorExpr,
}

#[derive(Debug)]
enum Ast {
    Root(Root),
    FnDecl(FnDecl),
    Var(Var),
    TypeDecl(TypeDecl),
    Param(Param),
    Type(Type),
    Struct(Struct),
    Statement(Statement),
    Assign(Assign),
    If(If),
    While(While),
    Return(Return),
    Expr(Expr),
    BlockStatement(BlockStatement),
    Empty,
}

impl<T: Lexer> SimpleParser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            lexer,
            stack: Vec::new(),
        }
    }

    fn parse_state(&mut self, state: ParsingState, data: Ast) -> Result<Ast, Error> {
        match state {
            ParsingState::Root { declarations } => self.parse_root(declarations, data),
            ParsingState::Var => self.parse_var(),
            ParsingState::VarType { name } => self.parse_var_type(name, data),
            ParsingState::VarValue { name, typ } => self.parse_var_value(name, typ, data),
            ParsingState::FnName => self.parse_fn_name(),
            ParsingState::FnParam { name, params } => self.parse_fn_param(name, params, data),
            ParsingState::FnReturn { name, params } => self.parse_fn_return(name, params, data),
            ParsingState::FnBody { name, params, ret_type } => self.parse_fn_body(name, params, ret_type, data),
            ParsingState::ParamName => self.parse_param_name(),
            ParsingState::ParamType { name } => self.parse_param_type(name, data),
            ParsingState::Type => self.parse_type(data),
            ParsingState::StructType { fields } => self.parse_struct_type(fields, data),
            ParsingState::Statement => self.parse_statement(data),
            ParsingState::BlockStatement { body } => self.parse_block_statement(body, data),
            ParsingState::AssignStatement => self.parse_assign_statement(data),
            ParsingState::AssignStatementValue { receiver, op } => {
                self.parse_assign_value_statement(receiver, op, data)
            }
            ParsingState::IfStatement => self.parse_if_statement(data),
            ParsingState::IfStatementBody { cond } => self.parse_if_statement_body(cond, data),
            ParsingState::WhileStatement => self.parse_while_statement(data),
            ParsingState::WhileStatementBody { cond } => self.parse_while_statement_body(cond, data),
            _ => unimplemented!(),
        }
    }

    fn parse_root(&mut self, mut declarations: Vec<Declaration>, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Var(decl) => declarations.push(Declaration::Var(decl)),
            Ast::FnDecl(decl) => declarations.push(Declaration::Fn(decl)),
            Ast::TypeDecl(decl) => declarations.push(Declaration::Type(decl)),
            Ast::Empty => (),
            _ => panic!("got {:?} instead of declaration", data),
        }

        loop {
            let token = self.lexer.peek()?;
            match token.kind {
                TokenKind::Eoi => {
                    return Ok(Ast::Root(Root { declarations }));
                }
                TokenKind::Endl => {
                    self.lexer.next()?;
                }
                TokenKind::Fn => {
                    self.stack.push(ParsingState::Root { declarations });
                    self.stack.push(ParsingState::FnName);
                    return Ok(Ast::Empty);
                }
                TokenKind::Var => {
                    self.stack.push(ParsingState::Root { declarations });
                    self.stack.push(ParsingState::Var);
                    return Ok(Ast::Empty);
                }
                TokenKind::Type => {
                    self.stack.push(ParsingState::Root { declarations });
                    self.stack.push(ParsingState::Type);
                    return Ok(Ast::Empty);
                }
                _ => {
                    let token = self.lexer.next()?;
                    return Err(Error::UnexpectedToken {
                        expected: vec![TokenKind::Fn, TokenKind::Var],
                        found: token,
                    });
                }
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
                    Ast::Var(Var { name, typ, value: None })
                }
            }
            _ => panic!("invalid data when parsing var type statement: {:?}", data),
        })
    }

    fn parse_var_value(&mut self, name: Token, typ: Type, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::VarValue { name, typ });
                self.stack.push(ParsingState::Expr);
                Ast::Empty
            }
            Ast::Expr(expr) => Ast::Var(Var {
                name,
                typ,
                value: Some(expr),
            }),
            _ => panic!("invalid data when parsing var value statement: {:?}", data),
        })
    }

    fn parse_fn_name(&mut self) -> Result<Ast, Error> {
        self.expect(TokenKind::Fn)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenBrace)?;

        self.stack.push(ParsingState::FnParam {
            name,
            params: Vec::new(),
        });
        Ok(Ast::Empty)
    }

    fn parse_fn_param(&mut self, name: Token, mut params: Vec<Param>, data: Ast) -> Result<Ast, Error> {
        let mut first_param = true;
        if let Ast::Param(param) = data {
            params.push(param);
            first_param = false;
        }

        if self.check(&TokenKind::CloseBrace)?.is_some() {
            if self.check(&TokenKind::Colon)?.is_none() {
                self.stack.push(ParsingState::FnBody {
                    name,
                    params,
                    ret_type: None,
                });
                return Ok(Ast::Empty);
            }

            self.stack.push(ParsingState::FnReturn { name, params });
            return Ok(Ast::Empty);
        }

        if !first_param {
            self.expect(TokenKind::Comma)?;
        }

        self.stack.push(ParsingState::FnParam { name, params });
        self.stack.push(ParsingState::ParamName);

        Ok(Ast::Empty)
    }

    fn parse_fn_return(&mut self, name: Token, params: Vec<Param>, data: Ast) -> Result<Ast, Error> {
        if let Ast::Type(typ) = data {
            self.stack.push(ParsingState::FnBody {
                name,
                params,
                ret_type: Some(typ),
            });
            return Ok(Ast::Empty);
        }

        self.stack.push(ParsingState::FnReturn { name, params });
        self.stack.push(ParsingState::Type);

        Ok(Ast::Empty)
    }

    fn parse_fn_body(
        &mut self,
        name: Token,
        params: Vec<Param>,
        ret_type: Option<Type>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::BlockStatement(body) = data {
            return Ok(Ast::FnDecl(FnDecl {
                name,
                params,
                ret_type,
                body,
            }));
        }

        self.stack.push(ParsingState::FnBody { name, params, ret_type });
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
            return Ok(Ast::Param(Param { name, typ }));
        }
        self.stack.push(ParsingState::ParamType { name });
        self.stack.push(ParsingState::Type);
        Ok(Ast::Empty)
    }

    fn parse_type(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Struct(typ) = data {
            return Ok(Ast::Struct(typ));
        }

        let token = self.lexer.peek()?;
        return match &token.kind {
            TokenKind::Struct => {
                self.stack.push(ParsingState::Type);
                self.stack.push(ParsingState::StructType { fields: Vec::new() });
                Ok(Ast::Empty)
            }
            TokenKind::Ident => {
                let token = self.lexer.next()?;
                Ok(Ast::Type(Type::Ident(token)))
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
                let token = self.lexer.next()?;
                Ok(Ast::Type(Type::Primitive(token)))
            }
            _ => {
                let token = self.lexer.next()?;
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
                    ],
                    found: token,
                })
            }
        };
    }

    fn parse_struct_type(&mut self, mut fields: Vec<Param>, data: Ast) -> Result<Ast, Error> {
        if let Ast::Param(field) = data {
            fields.push(field);
            self.consume_endl()?;

            let is_end = if self.check(&TokenKind::CloseBlock)?.is_some() {
                true
            } else {
                self.expect(TokenKind::Comma)?;
                self.consume_endl()?;
                self.check(&TokenKind::CloseBlock)?.is_some()
            };

            if is_end {
                return Ok(Ast::Struct(Struct { fields }));
            } else {
                self.stack.push(ParsingState::StructType { fields });
                self.stack.push(ParsingState::ParamName);
                return Ok(Ast::Empty);
            }
        }

        self.expect(TokenKind::Struct)?;
        self.consume_endl()?;

        self.expect(TokenKind::OpenBlock)?;
        self.consume_endl()?;

        if self.check(&TokenKind::CloseBlock)?.is_some() {
            return Ok(Ast::Struct(Struct { fields }));
        } else {
            self.stack.push(ParsingState::StructType { fields });
            self.stack.push(ParsingState::ParamName);
            return Ok(Ast::Empty);
        }
    }

    fn parse_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Var(stmt) => return Ok(Ast::Statement(Statement::Var(stmt))),
            Ast::Assign(stmt) => return Ok(Ast::Statement(Statement::Assign(stmt))),
            Ast::Return(stmt) => return Ok(Ast::Statement(Statement::Return(stmt))),
            Ast::If(stmt) => return Ok(Ast::Statement(Statement::If(stmt))),
            Ast::While(stmt) => return Ok(Ast::Statement(Statement::While(stmt))),
            Ast::BlockStatement(stmt) => return Ok(Ast::Statement(Statement::Block(stmt))),
            Ast::Expr(stmt) => return Ok(Ast::Statement(Statement::Expr(stmt))),
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

    fn parse_block_statement(&mut self, mut body: Vec<Statement>, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Empty => {
                self.expect(TokenKind::OpenBlock)?;
            }
            Ast::Statement(statement) => {
                body.push(statement);
            }
            _ => panic!("invalid data when parsing block statement: {:?}", data),
        }

        let result = if self.check(&TokenKind::CloseBlock)?.is_some() {
            Ast::BlockStatement(BlockStatement { body })
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
                let op = self.expect_one_of(vec![
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
                self.stack.push(ParsingState::AssignStatementValue { receiver, op });
                Ok(Ast::Empty)
            }
            Ast::Empty => {
                self.stack.push(ParsingState::AssignStatement);
                self.stack.push(ParsingState::Expr);
                Ok(Ast::Empty)
            }
            _ => panic!("invalid data when parsing assign statement: {:?}", data),
        }
    }

    fn parse_assign_value_statement(&mut self, receiver: Expr, op: Token, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Expr(value) => Ok(Ast::Assign(Assign { receiver, op, value })),
            Ast::Empty => {
                self.stack.push(ParsingState::AssignStatementValue { receiver, op });
                self.stack.push(ParsingState::Expr);
                Ok(Ast::Empty)
            }
            _ => panic!("invalid data when parsing assign value statement: {:?}", data),
        }
    }

    fn parse_if_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.expect(TokenKind::If)?;
                self.stack.push(ParsingState::IfStatement);
                self.stack.push(ParsingState::Expr);
                Ast::Empty
            }
            Ast::Expr(cond) => {
                self.stack.push(ParsingState::IfStatementBody { cond });
                Ast::Empty
            }
            _ => panic!("invalid data when parsing if statement: {:?}", data),
        })
    }

    fn parse_if_statement_body(&mut self, cond: Expr, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::IfStatementBody { cond });
                self.stack.push(ParsingState::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::If(If { cond, body }),
            _ => panic!("invalid data when parsing if statement body: {:?}", data),
        })
    }

    fn parse_while_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.expect(TokenKind::While)?;
                self.stack.push(ParsingState::WhileStatement);
                self.stack.push(ParsingState::Expr);
                Ast::Empty
            }
            Ast::Expr(cond) => {
                self.stack.push(ParsingState::WhileStatementBody { cond });
                Ast::Empty
            }
            _ => panic!("invalid data when parsing while statement: {:?}", data),
        })
    }

    fn parse_while_statement_body(&mut self, cond: Expr, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::WhileStatementBody { cond });
                self.stack.push(ParsingState::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::While(While { cond, body }),
            _ => panic!("invalid data when parsing while statement body: {:?}", data),
        })
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

    fn expect_one_of(&mut self, kind: Vec<TokenKind>) -> Result<Token, Error> {
        let token = self.lexer.next()?;

        for k in kind.iter() {
            if &token.kind != k {
                return Ok(token);
            }
        }

        Err(Error::UnexpectedToken {
            expected: kind,
            found: token,
        })
    }

    fn check(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error> {
        let token = self.lexer.peek()?;
        if &token.kind != kind {
            return Ok(None);
        }

        let token = self.lexer.next()?;
        Ok(Some(token))
    }

    fn consume_endl(&mut self) -> Result<(), Error> {
        while let TokenKind::Endl = self.lexer.peek()?.kind {
            self.lexer.next()?;
        }
        Ok(())
    }
}

impl<T: Lexer> Parser for SimpleParser<T> {
    fn parse(&mut self) -> std::result::Result<Root, Error> {
        let initial_state = ParsingState::Root {
            declarations: Vec::new(),
        };
        self.stack.push(initial_state);

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
