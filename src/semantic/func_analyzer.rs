use crate::semantic::semantic::{AssignKind, FnParam, FnType, Statement};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast, token};

use super::cycle;
use super::error::Error;
use super::semantic::{AssignStmt, BlockStmt, Expr, FnDef, Type, TypeKind, Var, VarStmt};
use super::type_analyzer::TypeAnalyzer;

pub struct FuncAnalyzer<'a: 'b, 'b> {
    root: &'a ast::Root,
    type_analyzer: &'b mut TypeAnalyzer<'a>,

    fn_to_ast: HashMap<String, &'a ast::FnDecl>,
    funcs: HashMap<String, FnDef>,
}

impl<'a, 'b> FuncAnalyzer<'a, 'b> {
    pub fn new(root: &'a ast::Root, type_analyzer: &'b mut TypeAnalyzer<'a>) -> Self {
        Self {
            root,
            type_analyzer,
            fn_to_ast: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn analyze(&mut self) -> Result<HashMap<String, FnDef>, Error<'a>> {
        for decl in self.root.declarations.iter() {
            if let ast::Declaration::Fn(fn_decl) = decl {
                let name = &fn_decl.name.value.as_ref().unwrap().clone();
                self.fn_to_ast.insert(name.clone(), &fn_decl);
            }
        }

        let mut result = HashMap::new();
        for (name, typ) in std::mem::replace(&mut self.fn_to_ast, HashMap::new()).iter() {
            let typ = self.analyze_func(typ)?;
            result.insert(name.clone(), typ);
        }

        Ok(result)
    }

    fn analyze_func(&mut self, func: &'a ast::FnDecl) -> Result<FnDef, Error<'a>> {
        let name = func.name.value.as_ref().unwrap().clone();
        let typ = self.type_analyzer.get_type(&name).unwrap();
        let body = self.analyze_block_statement(&func.body)?;
        Ok(FnDef { name, typ, body })
    }

    fn analyze_statement(&mut self, stmt: &'a ast::Statement) -> Result<Statement, Error<'a>> {
        match stmt {
            ast::Statement::Var(stmt) => self.analyze_var_statement(stmt),
            ast::Statement::Assign(stmt) => self.analyze_assign_statement(stmt),
            ast::Statement::Block(stmt) => self.analyze_block_statement(stmt),
            _ => unimplemented!(),
        }
    }

    fn analyze_var_statement(&mut self, stmt: &'a ast::Var) -> Result<Statement, Error<'a>> {
        Ok(Statement::Var(VarStmt {
            receiver: Rc::new(Var {
                name: stmt.name.value.as_ref().unwrap().clone(),
                typ: self.type_analyzer.analyze_type(&stmt.typ),
            }),
            value: None, // TODO: analyze the expr;
        }))
    }

    fn analyze_assign_statement(&mut self, stmt: &'a ast::Assign) -> Result<Statement, Error<'a>> {
        let target = self.analyze_expr(&stmt.receiver)?;
        if !target.assignable {
            return Err(Error::CannotAssignToValue {
                expr: &stmt.receiver,
            });
        }

        let value = self.analyze_expr(&stmt.value)?;
        if !self.is_type_assignable_to(target.typ.borrow(), value.typ.borrow()) {
            return Err(Error::MismatchType {
                expected: Rc::clone(&target.typ),
                got: Rc::clone(&value.typ),
            });
        }

        let kind = match stmt.op.kind {
            token::TokenKind::Assign => AssignKind::Assign,
            token::TokenKind::PlusAssign => AssignKind::Plus,
            token::TokenKind::MinusAssign => AssignKind::Minus,
            token::TokenKind::MulAssign => AssignKind::Mul,
            token::TokenKind::DivAssign => AssignKind::Div,
            token::TokenKind::ModAssign => AssignKind::Mod,
            token::TokenKind::BitAndAssign => AssignKind::BitAnd,
            token::TokenKind::BitOrAssign => AssignKind::BitOr,
            token::TokenKind::BitXorAssign => AssignKind::BitXor,
            token::TokenKind::SHLAssign => AssignKind::SHL,
            token::TokenKind::SHRAssign => AssignKind::SHR,
            _ => panic!("found assign operator {:?}", stmt.op.kind),
        };

        Ok(Statement::Assign(AssignStmt {
            target,
            value,
            kind,
        }))
    }

    fn analyze_block_statement(
        &mut self,
        stmt: &'a ast::BlockStatement,
    ) -> Result<Statement, Error<'a>> {
        let mut statements = Vec::new();
        for body in stmt.body.iter() {
            statements.push(self.analyze_statement(body)?);
        }

        Ok(Statement::Block(BlockStmt { statements }))
    }

    fn analyze_expr(&mut self, expr: &'a ast::Expr) -> Result<Expr, Error<'a>> {
        unimplemented!();
    }

    fn is_type_assignable_to(&self, target: &Type, value: &Type) -> bool {
        target == value
    }
}
