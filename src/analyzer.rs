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

pub trait Analyzer<'a> {
    fn analyze(&mut self, root: &'a ast::Root) -> Result<Unit, Error>;
}

pub fn analyze_asts(roots: &[ast::Root], headers: &[Header]) -> Result<Vec<Unit>, Error> {
    let package_name = roots[0].package_name.unwrap_str();
    let type_helper = TypeHelper::from_headers(package_name, headers);
    let mut expr_helper = ExprHelper::from_headers(package_name, &type_helper, headers);

    let mut analyzer = SimpleAnalyzer::new(&type_helper, &mut expr_helper);

    let mut units = Vec::new();
    for root in roots.iter() {
        units.push(analyzer.analyze(root)?);
    }

    Ok(units)
}

pub struct SimpleAnalyzer<'a, 'b> {
    type_helper: &'a dyn ITypeHelper<'a>,
    expr_helper: &'a mut ExprHelper<'a, 'b>,
}

impl<'a, 'b> SimpleAnalyzer<'a, 'b> {
    pub fn new(type_helper: &'a dyn ITypeHelper<'a>, expr_helper: &'a mut ExprHelper<'a, 'b>) -> Self {
        Self {
            type_helper,
            expr_helper,
        }
    }

    fn analyze_ast(&mut self, root_ast: &'a ast::Root) -> Result<Vec<FnDecl>, Error> {
        let package_name = root_ast.package_name.unwrap_str();
        let type_decls: Vec<&ast::FnDecl> = root_ast
            .declarations
            .iter()
            .filter_map(|decl| decl.try_unwrap_func())
            .collect();

        let mut fn_declarations: Vec<FnDecl> = Vec::new();
        for fn_decl in type_decls.iter() {
            self.expr_helper.add_block();

            let name = Name {
                package: String::from(package_name),
                name: String::from(fn_decl.name.unwrap_str()),
            };

            let func_symbol = self.expr_helper.find_symbol(&name).unwrap();
            let ftype = func_symbol.typ.unwrap_func().clone();

            for arg in ftype.arguments.iter() {
                self.expr_helper.add_symbol(Symbol {
                    name: Name {
                        package: String::from(package_name),
                        name: String::from(&arg.name),
                    },
                    typ: arg.typ.clone(),
                });
            }

            let native = fn_decl.header.native;

            let statement = if let Some(body) = &fn_decl.body {
                Some(self.analyze_block_stmt(package_name, body, &ftype)?)
            } else {
                None
            };

            fn_declarations.push(FnDecl {
                header: FnHeader {
                    name,
                    native,
                    typ: ftype.clone(),
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

                let typ = self.type_helper.get(&stmt.typ);
                if typ.is_invalid() {
                    return Err(Error::UndeclaredSymbol);
                }

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
                    if let Some(ret_val) = &stmt.value {
                        let val = self
                            .expr_helper
                            .analyze_expr(&ret_val, &self.type_helper.get_void(), false)?;
                        if &val.typ != expected_ret_type.as_ref() {
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

impl<'a, 'b> Analyzer<'a> for SimpleAnalyzer<'a, 'b> {
    fn analyze(&mut self, root: &'a ast::Root) -> Result<Unit, Error> {
        let fn_declarations = self.analyze_ast(root)?;

        Ok(Unit {
            package_name: root.package_name.unwrap_value().clone(),
            var_declarations: Vec::new(),
            fn_declarations,
        })
    }
}
