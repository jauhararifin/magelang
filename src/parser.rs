use crate::ast::{
    Assign, Binary, BlockStatement, Cast, Declaration, Expr, ExprKind, Field, FnDecl, FnHeader, FunctionCall, If,
    Import, Param, Return, Root, Selector, Statement, Struct, StructLit, Type, TypeDecl, Unary, Var, While,
};
use crate::lexer::{Error as LexerError, Lexer};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub enum Error {
    UnexpectedToken { expected: Vec<TokenKind>, found: Token },
    UnexpectedStructType { expr: Expr },
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
    Root,
    RootDecl {
        package_token: Token,
        package_name: Token,
        imports: Vec<Import>,
        declarations: Vec<Declaration>,
    },
    Import,
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
        fn_token: Token,
        name: Token,
        native: bool,
        params: Vec<Param>,
    },
    FnReturn {
        fn_token: Token,
        name: Token,
        native: bool,
        params: Vec<Param>,
    },
    FnBody {
        fn_token: Token,
        name: Token,
        params: Vec<Param>,
        ret_type: Option<Type>,
    },
    ParamName,
    ParamType {
        name: Token,
    },
    TypeDecl,
    TypeDeclType {
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
    ReturnStatement,
    ReturnStatementValue {
        ret: Token,
    },
    Expr,
    UnaryExpr,
    UnaryExprVal {
        op: Token,
    },
    BinaryExpr,
    BinaryExprOperand {
        a: Expr,
        op: Token,
    },
    CastExpr,
    CastExprType {
        val: Expr,
    },
    CallExpr,
    CallExprParams {
        func: Expr,
        args: Vec<Expr>,
    },
    StructLitExpr,
    StructLitField {
        type_expr: Expr,
        fields: Vec<Field>,
    },
    SelectorExpr,
    PrimaryExpr,
    FieldName,
    FieldValue {
        name: Token,
    },
}

#[derive(Debug)]
enum Ast {
    Root(Root),
    Import(Import),
    FnDecl(FnDecl),
    Var(Var),
    TypeDecl(TypeDecl),
    Param(Param),
    Field(Field),
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
            ParsingState::Root => self.parse_root(),
            ParsingState::RootDecl {
                package_token,
                package_name,
                imports,
                declarations,
            } => self.parse_root_decl(package_token, package_name, imports, declarations, data),
            ParsingState::Import => self.parse_import(),
            ParsingState::Var => self.parse_var(),
            ParsingState::VarType { name } => self.parse_var_type(name, data),
            ParsingState::VarValue { name, typ } => self.parse_var_value(name, typ, data),
            ParsingState::FnName => self.parse_fn_name(),
            ParsingState::FnParam {
                fn_token,
                name,
                native,
                params,
            } => self.parse_fn_param(fn_token, name, native, params, data),
            ParsingState::FnReturn {
                fn_token,
                name,
                native,
                params,
            } => self.parse_fn_return(fn_token, name, native, params, data),
            ParsingState::FnBody {
                fn_token,
                name,
                params,
                ret_type,
            } => self.parse_fn_body(fn_token, name, params, ret_type, data),
            ParsingState::ParamName => self.parse_param_name(),
            ParsingState::ParamType { name } => self.parse_param_type(name, data),
            ParsingState::TypeDecl => self.parse_type_decl(),
            ParsingState::TypeDeclType { name } => self.parse_type_decl_type(name, data),
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
            ParsingState::ReturnStatement => self.parse_return_statement(),
            ParsingState::ReturnStatementValue { ret } => self.parse_return_value_statement(ret, data),
            ParsingState::Expr => self.parse_expr(data),
            ParsingState::UnaryExpr => self.parse_unary_expr(),
            ParsingState::UnaryExprVal { op } => self.parse_unary_expr_val(op, data),
            ParsingState::BinaryExpr => self.parse_binary_expr(data),
            ParsingState::BinaryExprOperand { a, op } => self.parse_binary_expr_operand(a, op, data),
            ParsingState::CastExpr => self.parse_cast_expr(data),
            ParsingState::CastExprType { val } => self.parse_cast_expr_type(val, data),
            ParsingState::SelectorExpr => self.parse_selector_expr(data),
            ParsingState::CallExpr => self.parse_call_expr(data),
            ParsingState::CallExprParams { func, args } => self.parse_call_expr_params(func, args, data),
            ParsingState::PrimaryExpr => self.parse_primary_expr(data),
            ParsingState::StructLitExpr => self.parse_struct_lit_expr(data),
            ParsingState::StructLitField { type_expr, fields } => self.parse_struct_lit_field(type_expr, fields, data),
            ParsingState::FieldName => self.parse_field_name(),
            ParsingState::FieldValue { name } => self.parse_field_value(name, data),
        }
    }

    fn parse_root(&mut self) -> Result<Ast, Error> {
        self.consume_endl()?;
        let package_token = self.expect(TokenKind::Package)?;
        let package = self.expect(TokenKind::StringLit)?;
        self.stack.push(ParsingState::RootDecl {
            package_token,
            package_name: package,
            imports: vec![],
            declarations: vec![],
        });
        Ok(Ast::Empty)
    }

    fn parse_root_decl(
        &mut self,
        package_token: Token,
        package_name: Token,
        mut imports: Vec<Import>,
        mut declarations: Vec<Declaration>,
        data: Ast,
    ) -> Result<Ast, Error> {
        match data {
            Ast::Var(decl) => declarations.push(Declaration::Var(decl)),
            Ast::Import(import) => imports.push(import),
            Ast::FnDecl(decl) => declarations.push(Declaration::Fn(decl)),
            Ast::TypeDecl(decl) => declarations.push(Declaration::Type(decl)),
            Ast::Empty => (),
            _ => panic!("got {:?} instead of declaration", data),
        }

        self.consume_endl()?;

        let token = self.lexer.peek()?;
        match token.kind {
            TokenKind::Eoi => Ok(Ast::Root(Root {
                package_token,
                package_name,
                imports,
                declarations,
            })),
            TokenKind::Import => {
                self.stack.push(ParsingState::RootDecl {
                    package_token,
                    package_name,
                    imports,
                    declarations,
                });
                self.stack.push(ParsingState::Import);
                Ok(Ast::Empty)
            }
            TokenKind::Fn => {
                self.stack.push(ParsingState::RootDecl {
                    package_token,
                    package_name,
                    imports,
                    declarations,
                });
                self.stack.push(ParsingState::FnName);
                Ok(Ast::Empty)
            }
            TokenKind::Var => {
                self.stack.push(ParsingState::RootDecl {
                    package_token,
                    package_name,
                    imports,
                    declarations,
                });
                self.stack.push(ParsingState::Var);
                Ok(Ast::Empty)
            }
            TokenKind::Type => {
                self.stack.push(ParsingState::RootDecl {
                    package_token,
                    package_name,
                    imports,
                    declarations,
                });
                self.stack.push(ParsingState::TypeDecl);
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

    fn parse_import(&mut self) -> Result<Ast, Error> {
        let import_token = self.expect(TokenKind::Import)?;
        let name = self.expect(TokenKind::Ident)?;
        let package_name = self.expect(TokenKind::StringLit)?;
        Ok(Ast::Import(Import {
            import_token,
            name,
            package_name,
        }))
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
        let fn_token = self.expect(TokenKind::Fn)?;
        let native = self.check(&TokenKind::Native)?.is_some();
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenBrace)?;

        self.stack.push(ParsingState::FnParam {
            fn_token,
            name,
            native,
            params: Vec::new(),
        });
        Ok(Ast::Empty)
    }

    fn parse_fn_param(
        &mut self,
        fn_token: Token,
        name: Token,
        native: bool,
        mut params: Vec<Param>,
        data: Ast,
    ) -> Result<Ast, Error> {
        let mut first_param = true;
        if let Ast::Param(param) = data {
            params.push(param);
            first_param = false;
        }

        if self.check(&TokenKind::CloseBrace)?.is_some() {
            if self.check(&TokenKind::Colon)?.is_none() {
                if !native {
                    self.stack.push(ParsingState::FnBody {
                        fn_token,
                        name,
                        params,
                        ret_type: None,
                    });
                    return Ok(Ast::Empty);
                }
                return Ok(Ast::FnDecl(FnDecl {
                    fn_token,
                    name,
                    header: FnHeader {
                        native,
                        params,
                        ret_type: None,
                    },
                    body: None,
                }));
            }

            self.stack.push(ParsingState::FnReturn {
                fn_token,
                name,
                native,
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
            native,
            params,
        });
        self.stack.push(ParsingState::ParamName);

        Ok(Ast::Empty)
    }

    fn parse_fn_return(
        &mut self,
        fn_token: Token,
        name: Token,
        native: bool,
        params: Vec<Param>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::Type(typ) = data {
            if !native {
                self.stack.push(ParsingState::FnBody {
                    fn_token,
                    name,
                    params,
                    ret_type: Some(typ),
                });
                return Ok(Ast::Empty);
            }
            return Ok(Ast::FnDecl(FnDecl {
                fn_token,
                name,
                header: FnHeader {
                    native,
                    params,
                    ret_type: Some(typ),
                },
                body: None,
            }));
        }

        self.stack.push(ParsingState::FnReturn {
            fn_token,
            name,
            native,
            params,
        });
        self.stack.push(ParsingState::Type);

        Ok(Ast::Empty)
    }

    fn parse_fn_body(
        &mut self,
        fn_token: Token,
        name: Token,
        params: Vec<Param>,
        ret_type: Option<Type>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::BlockStatement(body) = data {
            return Ok(Ast::FnDecl(FnDecl {
                fn_token,
                name,
                header: FnHeader {
                    native: false,
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
            return Ok(Ast::Param(Param { name, typ }));
        }
        self.stack.push(ParsingState::ParamType { name });
        self.stack.push(ParsingState::Type);
        Ok(Ast::Empty)
    }

    fn parse_type_decl(&mut self) -> Result<Ast, Error> {
        self.expect(TokenKind::Type)?;
        let name = self.expect(TokenKind::Ident)?;
        self.stack.push(ParsingState::TypeDeclType { name });
        Ok(Ast::Empty)
    }

    fn parse_type_decl_type(&mut self, name: Token, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Type(typ) => Ok(Ast::TypeDecl(TypeDecl { name, typ })),
            Ast::Empty => {
                self.stack.push(ParsingState::TypeDeclType { name });
                self.stack.push(ParsingState::Type);
                Ok(Ast::Empty)
            }
            _ => panic!("invalid data when parsing type decl type: {:?}", data),
        }
    }

    fn parse_type(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Struct(typ) = data {
            return Ok(Ast::Type(Type::Struct(typ)));
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

                if self.check(&TokenKind::Dot)?.is_some() {
                    let selection = self.expect(TokenKind::Ident)?;
                    let source = Box::new(Expr {
                        pos: token.pos.clone(),
                        kind: ExprKind::Ident(token),
                    });
                    Ok(Ast::Type(Type::Selector(Selector { source, selection })))
                } else {
                    Ok(Ast::Type(Type::Ident(token)))
                }
            }
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
                        TokenKind::F32,
                        TokenKind::F64,
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
            TokenKind::Continue => return Ok(Ast::Statement(Statement::Continue)),
            TokenKind::Break => return Ok(Ast::Statement(Statement::Break)),
            _ => ParsingState::AssignStatement,
        };

        self.stack.push(ParsingState::Statement);
        self.stack.push(next_parser);

        Ok(Ast::Empty)
    }

    fn parse_block_statement(&mut self, mut body: Vec<Statement>, data: Ast) -> Result<Ast, Error> {
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

    fn parse_return_statement(&mut self) -> Result<Ast, Error> {
        let ret = self.expect(TokenKind::Return)?;
        if self.check(&TokenKind::Endl)?.is_some() {
            // TODO (jauhararifin): endl is not the only token indicating end of statement.
            return Ok(Ast::Return(Return { ret, value: None }));
        }

        self.stack.push(ParsingState::ReturnStatementValue { ret });
        self.stack.push(ParsingState::Expr);
        Ok(Ast::Empty)
    }

    fn parse_return_value_statement(&mut self, ret: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(value) = data {
            return Ok(Ast::Return(Return {
                ret,
                value: Some(value),
            }));
        } else {
            panic!("got invalid data while parsing return value statement: {:?}", data);
        }
    }

    fn parse_expr(&mut self, data: Ast) -> Result<Ast, Error> {
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

        self.stack.push(ParsingState::Expr);
        self.stack.push(ParsingState::BinaryExpr);
        self.stack.push(ParsingState::CastExpr);
        self.stack.push(ParsingState::UnaryExpr);

        Ok(Ast::Empty)
    }

    fn parse_binary_expr(&mut self, data: Ast) -> Result<Ast, Error> {
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
                    self.stack.push(ParsingState::BinaryExprOperand { a: expr, op });
                    return Ok(Ast::Empty);
                }
            }
            Ok(Ast::Expr(expr))
        } else {
            panic!("invalid data when parsing binary expr: {:?}", data)
        }
    }

    fn parse_binary_expr_operand(&mut self, a: Expr, op: Token, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Expr(expr) => Ast::Expr(Expr {
                pos: a.pos,
                kind: ExprKind::Binary(Binary {
                    a: Box::new(a),
                    op,
                    b: Box::new(expr),
                }),
            }),
            Ast::Empty => {
                self.stack.push(ParsingState::BinaryExprOperand { a, op });
                self.stack.push(ParsingState::Expr);
                Ast::Empty
            }
            _ => panic!("invalid data when parsing binary expr operand: {:?}", data),
        })
    }

    fn parse_cast_expr(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(val) = data {
            if self.check(&TokenKind::As)?.is_some() {
                self.stack.push(ParsingState::CastExprType { val });
                Ok(Ast::Empty)
            } else {
                Ok(Ast::Expr(val))
            }
        } else {
            panic!("invalid data when parsing cast expr: {:?}", data)
        }
    }

    fn parse_cast_expr_type(&mut self, val: Expr, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Type(target) => Ast::Expr(Expr {
                pos: val.pos,
                kind: ExprKind::Cast(Cast {
                    target,
                    val: Box::new(val),
                }),
            }),
            Ast::Empty => {
                self.stack.push(ParsingState::CastExprType { val });
                self.stack.push(ParsingState::Type);
                Ast::Empty
            }
            _ => panic!("invalid data when parsing cast expr type: {:?}", data),
        })
    }

    fn parse_selector_expr(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(source) = data {
            if self.check(&TokenKind::Dot)?.is_some() {
                let selection = self.expect(TokenKind::Ident)?;
                Ok(Ast::Expr(Expr {
                    pos: source.pos,
                    kind: ExprKind::Selector(Selector {
                        source: Box::new(source),
                        selection,
                    }),
                }))
            } else {
                Ok(Ast::Expr(source))
            }
        } else {
            panic!("invalid data when parsing selector expr: {:?}", data)
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Ast, Error> {
        if let Some(op) = self.check_one_of(vec![
            TokenKind::Not,
            TokenKind::Minus,
            TokenKind::Plus,
            TokenKind::BitNot,
        ])? {
            self.stack.push(ParsingState::UnaryExprVal { op });
        } else {
            self.stack.push(ParsingState::CallExpr);
            self.stack.push(ParsingState::StructLitExpr);
            self.stack.push(ParsingState::SelectorExpr);
            self.stack.push(ParsingState::PrimaryExpr);
        }
        Ok(Ast::Empty)
    }

    fn parse_unary_expr_val(&mut self, op: Token, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(ParsingState::UnaryExprVal { op });
                self.stack.push(ParsingState::Expr);
                Ast::Empty
            }
            Ast::Expr(val) => Ast::Expr(Expr {
                pos: op.pos,
                kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
            }),
            _ => panic!("invalid data when parsing unary expr val: {:?}", data),
        })
    }

    fn parse_call_expr(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(func) = data {
            if self.check(&TokenKind::OpenBrace)?.is_some() {
                self.stack.push(ParsingState::CallExprParams { func, args: vec![] });
                Ok(Ast::Empty)
            } else {
                Ok(Ast::Expr(func))
            }
        } else {
            panic!("invalid data when parsing call expr: {:?}", data)
        }
    }

    fn parse_call_expr_params(&mut self, func: Expr, mut args: Vec<Expr>, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(arg) = data {
            args.push(arg);
        }

        if let Some(token) = self.check_one_of(vec![TokenKind::Comma, TokenKind::CloseBrace])? {
            if token.kind == TokenKind::Comma {
                self.stack.push(ParsingState::CallExprParams { func, args });
                self.stack.push(ParsingState::Expr);
                return Ok(Ast::Empty);
            }

            return Ok(Ast::Expr(Expr {
                pos: func.pos,
                kind: ExprKind::FunctionCall(FunctionCall {
                    func: Box::new(func),
                    args,
                }),
            }));
        }

        self.stack.push(ParsingState::CallExprParams { func, args });
        self.stack.push(ParsingState::Expr);
        Ok(Ast::Empty)
    }

    fn parse_struct_lit_expr(&mut self, data: Ast) -> Result<Ast, Error> {
        let expr = if let Ast::Expr(expr) = data {
            expr
        } else {
            panic!("invalid data when parsing struct lit expr: {:?}", data)
        };

        if self.check(&TokenKind::OpenBlock)?.is_some() {
            self.stack.push(ParsingState::StructLitField {
                type_expr: expr,
                fields: vec![],
            });
            Ok(Ast::Empty)
        } else {
            Ok(Ast::Expr(expr))
        }
    }

    fn parse_struct_lit_field(&mut self, type_expr: Expr, mut fields: Vec<Field>, data: Ast) -> Result<Ast, Error> {
        if let Ast::Field(field) = data {
            fields.push(field);
        }

        if self.check(&TokenKind::CloseBlock)?.is_some() {
            return Ok(Ast::Expr(Expr {
                pos: type_expr.pos,
                kind: ExprKind::StructLit(StructLit {
                    typ: self.expr_to_type(type_expr)?,
                    fields,
                }),
            }));
        }

        self.stack.push(ParsingState::StructLitField { type_expr, fields });
        self.stack.push(ParsingState::FieldName);

        Ok(Ast::Empty)
    }

    fn expr_to_type(&self, type_expr: Expr) -> Result<Type, Error> {
        match type_expr.kind {
            ExprKind::Ident(ident) => Ok(Type::Ident(ident)),
            ExprKind::Selector(selector) => Ok(Type::Selector(selector)),
            _ => Err(Error::UnexpectedStructType { expr: type_expr }),
        }
    }

    fn parse_field_name(&mut self) -> Result<Ast, Error> {
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        self.stack.push(ParsingState::ParamType { name });
        Ok(Ast::Empty)
    }

    fn parse_field_value(&mut self, name: Token, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(value) = data {
            self.consume_endl()?;
            self.check(&TokenKind::Comma)?;
            return Ok(Ast::Field(Field { name, value }));
        }
        self.stack.push(ParsingState::FieldValue { name });
        self.stack.push(ParsingState::Type);
        Ok(Ast::Empty)
    }

    fn parse_primary_expr(&mut self, data: Ast) -> Result<Ast, Error> {
        if let Ast::Expr(expr) = data {
            self.expect(TokenKind::CloseBrace)?;
            return Ok(Ast::Expr(expr));
        }

        let token = self.lexer.next()?;

        match token.kind {
            TokenKind::IntegerLit => Ok(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::IntegerLit(token),
            })),
            TokenKind::FloatLit => Ok(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::FloatLit(token),
            })),
            TokenKind::StringLit => Ok(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::StringLit(token),
            })),
            TokenKind::True | TokenKind::False => Ok(Ast::Expr(Expr {
                pos: token.pos,
                kind: ExprKind::BoolLit(token),
            })),
            TokenKind::Ident => {
                self.consume_endl()?;
                Ok(Ast::Expr(Expr {
                    pos: token.pos,
                    kind: ExprKind::Ident(token),
                }))
            }
            TokenKind::OpenBrace => {
                self.stack.push(ParsingState::PrimaryExpr);
                self.stack.push(ParsingState::Expr);
                Ok(Ast::Empty)
            }
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
                found: token,
            }),
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

impl<T: Lexer> Parser for SimpleParser<T> {
    fn parse(&mut self) -> std::result::Result<Root, Error> {
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
