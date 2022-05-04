use crate::{
    ast,
    errors::Error,
    semantic::{
        Assign, AssignOp, BlockStatement, FnDecl, FnHeader, FnType, Header, If, Return, Statement, Type, Unit, Var,
        VarHeader, While,
    },
    token::TokenKind,
};

use super::{
    expr::{ExprHelper, Symbol},
    types::TypeHelper,
};

pub fn analyze_root(root: &ast::RootNode, headers: &[Header]) -> Result<Unit, Error> {
    let type_helper = TypeHelper::new();
    let mut expr_helper = ExprHelper::from_headers(&type_helper, headers);
    let mut analyzer = UnitAnalyzer::new(&type_helper, &mut expr_helper);

    analyzer.analyze(root)
}

pub struct UnitAnalyzer<'a> {
    type_helper: &'a TypeHelper,
    expr_helper: &'a mut ExprHelper<'a>,
}

impl<'a> UnitAnalyzer<'a> {
    pub fn new(type_helper: &'a TypeHelper, expr_helper: &'a mut ExprHelper<'a>) -> Self {
        Self {
            type_helper,
            expr_helper,
        }
    }

    pub fn analyze(&mut self, root: &'a ast::RootNode) -> Result<Unit, Error> {
        let functions = self.analyze_ast(root)?;
        Ok(Unit { functions })
    }

    fn analyze_ast(&mut self, root_ast: &'a ast::RootNode) -> Result<Vec<FnDecl>, Error> {
        let mut results: Vec<FnDecl> = Vec::new();

        let func_decls: Vec<&ast::FnDeclNode> = root_ast
            .declarations
            .iter()
            .filter_map(|decl| decl.try_unwrap_func())
            .collect();

        for fn_decl in func_decls.iter() {
            self.expr_helper.add_block();

            let name = String::from(fn_decl.name.unwrap_str());

            let func_symbol = self.expr_helper.find_symbol(&name).unwrap();
            let ftype = func_symbol.type_kind.unwrap_func().clone();

            for arg in ftype.arguments.iter() {
                self.expr_helper.add_symbol(Symbol {
                    name: String::from(&arg.name),
                    type_kind: arg.type_kind.clone(),
                });
            }

            let native = fn_decl.header.native_token.is_some();

            let statement = if let Some(body) = &fn_decl.body {
                Some(self.analyze_block_stmt(body, &ftype)?)
            } else {
                None
            };

            results.push(FnDecl {
                header: FnHeader {
                    name,
                    native,
                    typ: ftype.clone(),
                },
                body: statement,
            });

            self.expr_helper.pop_block();
        }

        Ok(results)
    }

    fn analyze_stmt(&mut self, stmt: &'a ast::Statement, ftype: &FnType) -> Result<Statement, Error> {
        match stmt {
            ast::Statement::Var(stmt) => {
                let name = String::from(stmt.name.unwrap_str());
                if self.expr_helper.find_symbol(&name).is_some() {
                    return Err(Error::RedeclaredSymbol);
                }

                let type_kind = self.type_helper.get(&stmt.typ);
                let value = if let Some(val) = &stmt.value {
                    let value = self.expr_helper.analyze(val, &type_kind)?;
                    if &value.type_kind != &type_kind {
                        return Err(Error::MismatchType);
                    }
                    Some(value)
                } else {
                    None
                };

                self.expr_helper.add_symbol(Symbol {
                    name: name.clone(),
                    type_kind: type_kind.clone(),
                });

                Ok(Statement::Var(Var {
                    header: VarHeader {
                        name: name.clone(),
                        type_kind,
                    },
                    value,
                }))
            }
            ast::Statement::Assign(stmt) => {
                let receiver = self.expr_helper.analyze(&stmt.receiver, &Type::Void)?;
                if !receiver.assignable {
                    return Err(Error::CannotAssignTo);
                }

                let value = self.expr_helper.analyze(&stmt.value, &receiver.type_kind)?;

                if value.type_kind != receiver.type_kind {
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
                        let val = self.expr_helper.analyze(&ret_val, &Type::Void)?;
                        if &val.type_kind != expected_ret_type.as_ref() {
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
                let cond = self.expr_helper.analyze(&stmt.cond, &Type::Bool)?;
                let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
                Ok(Statement::If(If { cond, body }))
            }
            ast::Statement::While(stmt) => {
                let cond = self.expr_helper.analyze(&stmt.cond, &Type::Bool)?;
                let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
                Ok(Statement::While(While { cond, body }))
            }
            ast::Statement::Block(stmt) => self.analyze_block_stmt(stmt, ftype),
            ast::Statement::Expr(expr) => Ok(Statement::Expr(self.expr_helper.analyze(expr, &Type::Void)?)),
        }
    }

    fn analyze_block_stmt(&mut self, stmt: &'a ast::BlockStatement, ftype: &FnType) -> Result<Statement, Error> {
        let mut body = Vec::new();
        for s in stmt.body.iter() {
            body.push(self.analyze_stmt(s, ftype)?);
        }
        Ok(Statement::Block(BlockStatement { body }))
    }
}
