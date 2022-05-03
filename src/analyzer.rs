use crate::{
    ast,
    errors::Error,
    expr_helper::{ExprHelper, Symbol},
    semantic::{
        Assign, AssignOp, BlockStatement, FnDecl, FnHeader, FnType, Header, If, Name, Return, Statement, Unit, Var,
        VarHeader, While,
    },
    token::TokenKind,
    type_helper::{ITypeHelper, TypeHelper},
};

pub trait Analyzer {
    fn analyze(&self, root: &ast::Root) -> Result<Unit, Error>;
}

pub struct SimpleAnalyzer<'a> {
    headers: &'a [Header],
}

impl<'a> SimpleAnalyzer<'a> {
    pub fn new(headers: &'a [Header]) -> Self {
        Self { headers }
    }
}

impl<'a> Analyzer for SimpleAnalyzer<'a> {
    fn analyze(&self, root: &ast::Root) -> Result<Unit, Error> {
        let type_helper = TypeHelper::from_headers(root.package_name.unwrap_str(), self.headers);
        let mut expr_helper = ExprHelper::from_headers(root.package_name.unwrap_str(), &type_helper, self.headers);

        let mut func_processor = FuncProcessor::new(&type_helper, &mut expr_helper);
        let fn_declarations = func_processor.analyze(root)?;

        Ok(Unit {
            package_name: root.package_name.unwrap_value().clone(),
            var_declarations: Vec::new(),
            fn_declarations,
        })
    }
}

struct FuncProcessor<'a, 'b> {
    type_helper: &'b dyn ITypeHelper<'a>,
    expr_helper: &'b mut ExprHelper<'a, 'b>,
}

impl<'a, 'b> FuncProcessor<'a, 'b> {
    fn new(type_helper: &'b dyn ITypeHelper<'a>, expr_helper: &'b mut ExprHelper<'a, 'b>) -> Self {
        Self {
            type_helper,
            expr_helper,
        }
    }

    fn analyze(&mut self, root_ast: &'a ast::Root) -> Result<Vec<FnDecl>, Error> {
        let package_name = root_ast.package_name.unwrap_str();
        let type_decls: Vec<&ast::FnDecl> = root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Fn(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        let mut fn_declarations: Vec<FnDecl> = Vec::new();
        for fn_decl in type_decls.iter() {
            let name = Name {
                package: String::from(package_name),
                name: String::from(fn_decl.name.unwrap_str()),
            };

            let func_symbol = self.expr_helper.find_symbol(&name).unwrap();
            let func_type = func_symbol.typ.clone();
            let ftype = func_type.borrow();
            let ftype = ftype.unwrap_func();

            for arg in ftype.arguments.iter() {
                self.expr_helper.add_symbol(Symbol {
                    name: Name {
                        package: String::new(),
                        name: String::from(&arg.name),
                    },
                    typ: arg.typ.upgrade().unwrap().clone(),
                });
            }

            let native = fn_decl.header.native;

            let statement = if let Some(body) = &fn_decl.body {
                Some(self.analyze_block_stmt(package_name, body, ftype)?)
            } else {
                None
            };

            fn_declarations.push(FnDecl {
                header: FnHeader {
                    name,
                    native,
                    typ: func_type.clone(),
                },
                body: statement,
            });

            self.expr_helper.pop_block();
        }

        Ok(fn_declarations)
    }

    fn analyze_stmt(
        &mut self,
        package_name: &str,
        stmt: &'a ast::Statement,
        ftype: &FnType,
    ) -> Result<Statement, Error> {
        match stmt {
            ast::Statement::Var(stmt) => {
                // TODO jauhararifin: this implementation is similar to the one in the value
                // processor. Maybe can consider make it dryer.

                let typ = self.type_helper.get(&stmt.typ).ok_or(Error::UndeclaredSymbol)?;

                let name = Name {
                    package: String::from(package_name),
                    name: String::from(stmt.name.unwrap_str()),
                };

                if self.expr_helper.find_symbol(&name).is_some() {
                    return Err(Error::RedeclaredSymbol);
                }

                let value = if let Some(val) = &stmt.value {
                    let value = self.expr_helper.analyze_expr(val, &typ, false)?;
                    if &value.typ != &typ {
                        return Err(Error::MismatchType);
                    }
                    Some(value)
                } else {
                    None
                };

                self.expr_helper.add_symbol(Symbol {
                    name: name.clone(),
                    typ: typ.clone(),
                });

                Ok(Statement::Var(Var {
                    header: VarHeader {
                        name: name.clone(),
                        typ: typ.clone(),
                    },
                    value,
                }))
            }
            ast::Statement::Assign(stmt) => {
                let receiver = self
                    .expr_helper
                    .analyze_expr(&stmt.receiver, &self.type_helper.get_void(), false)?;
                if !receiver.assignable {
                    return Err(Error::CannotAssignTo);
                }

                let value = self.expr_helper.analyze_expr(&stmt.value, &receiver.typ, false)?;

                // TODO: check based on the op instead of just using !=.
                if value.typ != receiver.typ {
                    return Err(Error::MismatchType);
                }

                let op = match &stmt.op.kind {
                    TokenKind::Assign => AssignOp::Assign,
                    TokenKind::PlusAssign => AssignOp::PlusAssign,
                    TokenKind::MinusAssign => AssignOp::MinusAssign,
                    TokenKind::MulAssign => AssignOp::MulAssign,
                    TokenKind::DivAssign => AssignOp::DivAssign,
                    TokenKind::ModAssign => AssignOp::ModAssign,
                    TokenKind::BitAndAssign => AssignOp::BitAndAssign,
                    TokenKind::BitOrAssign => AssignOp::BitOrAssign,
                    TokenKind::BitXorAssign => AssignOp::BitXorAssign,
                    TokenKind::ShlAssign => AssignOp::ShlAssign,
                    TokenKind::ShrAssign => AssignOp::ShrAssign,
                    _ => unreachable!(),
                };

                Ok(Statement::Assign(Assign { receiver, op, value }))
            }
            ast::Statement::Return(stmt) => {
                if let Some(expected_ret_type) = &ftype.return_type {
                    let t = expected_ret_type.upgrade().unwrap();
                    if let Some(ret_val) = &stmt.value {
                        let val = self
                            .expr_helper
                            .analyze_expr(&ret_val, &self.type_helper.get_void(), false)?;
                        if val.typ != t {
                            return Err(Error::MismatchType);
                        }

                        Ok(Statement::Return(Return { value: Some(val) }))
                    } else {
                        Err(Error::MissingReturnValue)
                    }
                } else {
                    if let Some(_) = &stmt.value {
                        Err(Error::MismatchType)
                    } else {
                        Ok(Statement::Return(Return { value: None }))
                    }
                }
            }
            ast::Statement::If(stmt) => {
                let cond = self
                    .expr_helper
                    .analyze_expr(&stmt.cond, &self.type_helper.get_bool(), false)?;
                let body = Box::new(self.analyze_block_stmt(package_name, &stmt.body, ftype)?);
                Ok(Statement::If(If { cond, body }))
            }
            ast::Statement::While(stmt) => {
                let cond = self
                    .expr_helper
                    .analyze_expr(&stmt.cond, &self.type_helper.get_bool(), false)?;
                let body = Box::new(self.analyze_block_stmt(package_name, &stmt.body, ftype)?);
                Ok(Statement::While(While { cond, body }))
            }
            ast::Statement::Block(stmt) => self.analyze_block_stmt(package_name, stmt, ftype),
            ast::Statement::Expr(_stmt) => todo!(),
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
        }
    }

    fn analyze_block_stmt(
        &mut self,
        package_name: &str,
        stmt: &'a ast::BlockStatement,
        ftype: &FnType,
    ) -> Result<Statement, Error> {
        let mut body = Vec::new();
        for s in stmt.body.iter() {
            body.push(self.analyze_stmt(package_name, s, ftype)?);
        }
        Ok(Statement::Block(BlockStatement { body }))
    }
}
