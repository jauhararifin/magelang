use std::rc::Rc;

use crate::{
    ast::{
        AssignNode, BlockStatementNode, FnDeclNode, IfNode, ReturnNode, RootNode, StatementNode, VarNode, WhileNode,
    },
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

pub struct UnitAnalyzer {}

impl UnitAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze(&self, root: &RootNode, headers: &[Header]) -> Result<Unit, Error> {
        let type_helper = TypeHelper::new();
        let mut expr_helper = ExprHelper::from_headers(&type_helper, headers);
        let mut analyzer = UnitAnalyzerHelper::new(&type_helper, &mut expr_helper);

        analyzer.analyze(root)
    }
}

struct UnitAnalyzerHelper<'a> {
    type_helper: &'a TypeHelper,
    expr_helper: &'a mut ExprHelper<'a>,
}

impl<'a> UnitAnalyzerHelper<'a> {
    fn new(type_helper: &'a TypeHelper, expr_helper: &'a mut ExprHelper<'a>) -> Self {
        Self {
            type_helper,
            expr_helper,
        }
    }

    fn analyze(&mut self, root: &'a RootNode) -> Result<Unit, Error> {
        let functions = self.analyze_ast(root)?;
        Ok(Unit { functions })
    }

    fn analyze_ast(&mut self, root_ast: &'a RootNode) -> Result<Vec<FnDecl>, Error> {
        let mut results: Vec<FnDecl> = Vec::new();

        let func_decls: Vec<&FnDeclNode> = root_ast
            .declarations
            .iter()
            .filter_map(|decl| decl.try_unwrap_func())
            .collect();

        for fn_decl in func_decls.iter() {
            self.expr_helper.add_block();

            let name = fn_decl.name.clone_value();

            let func_symbol = self.expr_helper.find_symbol(&name).unwrap();
            let typ = func_symbol.typ.clone();
            let ftype = typ.unwrap_func();

            for arg in ftype.arguments.iter() {
                self.expr_helper.add_symbol(Rc::new(Symbol {
                    name: arg.name.clone(),
                    typ: arg.typ.clone(),
                }));
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
                    fn_type: ftype.clone(),
                    typ: typ.clone(),
                },
                body: statement,
            });

            self.expr_helper.pop_block();
        }

        Ok(results)
    }

    fn analyze_stmt(&mut self, stmt: &'a StatementNode, ftype: &FnType) -> Result<Statement, Error> {
        match stmt {
            StatementNode::Var(stmt) => self.analyze_var_stmt(stmt),
            StatementNode::Assign(stmt) => self.analyze_assign_stmt(stmt),
            StatementNode::Return(stmt) => self.analyze_return_stmt(stmt, ftype),
            StatementNode::If(stmt) => self.analyze_if_stmt(stmt, ftype),
            StatementNode::While(stmt) => self.analyze_while_stmt(stmt, ftype),
            StatementNode::Block(stmt) => self.analyze_block_stmt(stmt, ftype),
            StatementNode::Expr(expr) => Ok(Statement::Expr(self.expr_helper.analyze(expr, Rc::new(Type::Void))?)),
        }
    }

    fn analyze_var_stmt(&mut self, stmt: &VarNode) -> Result<Statement, Error> {
        let name = stmt.name.clone_value();

        if self.expr_helper.find_symbol(&name).is_some() {
            return Err(Error::RedeclaredSymbol);
        }

        let typ = self.type_helper.get(&stmt.typ)?;

        let value = if let Some(val) = &stmt.value {
            let value = self.expr_helper.analyze(val, typ.clone())?;
            if &value.typ != &typ {
                return Err(Error::MismatchType);
            }
            Some(value)
        } else {
            None
        };

        self.expr_helper.add_symbol(Rc::new(Symbol {
            name: name.clone(),
            typ: typ.clone(),
        }));

        Ok(Statement::Var(Var {
            header: VarHeader { name, typ },
            value,
        }))
    }

    fn analyze_assign_stmt(&self, stmt: &AssignNode) -> Result<Statement, Error> {
        let receiver = self.expr_helper.analyze(&stmt.receiver, Rc::new(Type::Void))?;
        if !receiver.assignable {
            return Err(Error::CannotAssignTo);
        }

        let value = self.expr_helper.analyze(&stmt.value, receiver.typ.clone())?;

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

    fn analyze_return_stmt(&self, stmt: &ReturnNode, ftype: &FnType) -> Result<Statement, Error> {
        match (ftype.return_type.as_ref(), stmt.value.as_ref()) {
            (Some(ret_type), Some(ret_val)) => {
                let val = self.expr_helper.analyze(&ret_val, Rc::new(Type::Void))?;
                if &val.typ != ret_type {
                    return Err(Error::MismatchType);
                }
                Ok(Statement::Return(Return { value: Some(val) }))
            }
            (Some(_), None) | (None, Some(_)) => Err(Error::MissingReturnValue),
            (None, None) => Ok(Statement::Return(Return { value: None })),
        }
    }

    fn analyze_if_stmt(&mut self, stmt: &'a IfNode, ftype: &FnType) -> Result<Statement, Error> {
        let cond = self.expr_helper.analyze(&stmt.cond, Rc::new(Type::Bool))?;
        let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
        Ok(Statement::If(If { cond, body }))
    }

    fn analyze_while_stmt(&mut self, stmt: &'a WhileNode, ftype: &FnType) -> Result<Statement, Error> {
        let cond = self.expr_helper.analyze(&stmt.cond, Rc::new(Type::Bool))?;
        let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
        Ok(Statement::While(While { cond, body }))
    }

    fn analyze_block_stmt(&mut self, stmt: &'a BlockStatementNode, ftype: &FnType) -> Result<Statement, Error> {
        let mut body = Vec::new();
        for s in stmt.body.iter() {
            body.push(self.analyze_stmt(s, ftype)?);
        }
        Ok(Statement::Block(BlockStatement { body }))
    }
}
