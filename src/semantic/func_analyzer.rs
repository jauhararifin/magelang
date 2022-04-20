use crate::semantic::semantic::{AssignKind, BinaryOpKind, Statement};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast, token};

use super::error::Error;
use super::semantic::{
    AssignStmt, BinaryOp, BlockStmt, Expr, ExprKind, ExprStmt, FnDef, IfStmt, ReturnStmt, Type,
    TypeKind, Var, VarExpr, VarStmt, WhileStmt,
};
use super::type_analyzer::TypeAnalyzer;

pub struct FuncAnalyzer<'a: 'b, 'b> {
    root: &'a ast::Root,
    type_analyzer: &'b mut TypeAnalyzer<'a>,

    fn_to_ast: HashMap<String, &'a ast::FnDecl>,
    funcs: HashMap<String, FnDef>,

    locals: HashMap<String, Rc<Var>>,
    current_ret_type: Option<Rc<Type>>,
}

impl<'a, 'b> FuncAnalyzer<'a, 'b> {
    pub fn new(root: &'a ast::Root, type_analyzer: &'b mut TypeAnalyzer<'a>) -> Self {
        Self {
            root,
            type_analyzer,
            fn_to_ast: HashMap::new(),
            funcs: HashMap::new(),
            locals: HashMap::new(),
            current_ret_type: None,
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
        let typ = self.type_analyzer.get_type_from_token(&func.name)?;

        self.current_ret_type = Some(Rc::clone(&typ));
        if let TypeKind::Fn(fn_type) = &typ.kind {
            for param in fn_type.params.iter() {
                self.locals.insert(param.name.clone(), Rc::clone(param));
            }
        }

        let name = func.name.value.as_ref().unwrap().clone();
        let body = self.analyze_block_statement(&func.body)?;

        if let TypeKind::Fn(fn_type) = &typ.kind {
            for param in fn_type.params.iter() {
                self.locals.remove(&param.name);
            }
        }

        Ok(FnDef { name, typ, body })
    }

    fn analyze_statement(&mut self, stmt: &'a ast::Statement) -> Result<Statement, Error<'a>> {
        match stmt {
            ast::Statement::Var(stmt) => self.analyze_var_statement(stmt),
            ast::Statement::Assign(stmt) => self.analyze_assign_statement(stmt),
            ast::Statement::Return(stmt) => self.analyze_return_statement(stmt),
            ast::Statement::If(stmt) => self.analyze_if_statement(stmt),
            ast::Statement::While(stmt) => self.analyze_while_statement(stmt),
            ast::Statement::Block(stmt) => self.analyze_block_statement(stmt),
            ast::Statement::Expr(stmt) => self.analyze_expr_statement(stmt),
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

    fn analyze_return_statement(&mut self, stmt: &'a ast::Return) -> Result<Statement, Error<'a>> {
        let value = self.analyze_expr(&stmt.value)?;
        let ret_type = self.current_ret_type.as_ref().unwrap();

        if !self.is_type_assignable_to(ret_type.borrow(), value.typ.borrow()) {
            return Err(Error::MismatchType {
                expected: Rc::clone(ret_type),
                got: Rc::clone(&value.typ),
            });
        }

        return Ok(Statement::Return(ReturnStmt { value }));
    }

    fn analyze_if_statement(&mut self, stmt: &'a ast::If) -> Result<Statement, Error<'a>> {
        let cond = self.analyze_expr(&stmt.cond)?;
        if !matches!(cond.typ.kind, TypeKind::Bool) {
            return Err(Error::MismatchType {
                expected: Rc::clone(&self.type_analyzer.bool_type),
                got: Rc::clone(&cond.typ),
            });
        }

        let body = Box::new(self.analyze_block_statement(&stmt.body)?);
        Ok(Statement::If(IfStmt { cond, body }))
    }

    fn analyze_while_statement(&mut self, stmt: &'a ast::While) -> Result<Statement, Error<'a>> {
        let cond = self.analyze_expr(&stmt.cond)?;
        if !matches!(cond.typ.kind, TypeKind::Bool) {
            return Err(Error::MismatchType {
                expected: Rc::clone(&self.type_analyzer.bool_type),
                got: Rc::clone(&cond.typ),
            });
        }

        let body = Box::new(self.analyze_block_statement(&stmt.body)?);
        Ok(Statement::While(WhileStmt { cond, body }))
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

    fn analyze_expr_statement(&mut self, stmt: &'a ast::Expr) -> Result<Statement, Error<'a>> {
        Ok(Statement::Expr(ExprStmt {
            expr: self.analyze_expr(stmt)?,
        }))
    }

    fn is_type_assignable_to(&self, target: &Type, value: &Type) -> bool {
        target == value
    }

    fn analyze_expr(&mut self, expr: &'a ast::Expr) -> Result<Expr, Error<'a>> {
        match expr {
            ast::Expr::Ident(expr) => self.analyze_ident_expr(expr),
            ast::Expr::Binary(expr) => self.analyze_binary_expr(expr),
            ast::Expr::IntegerLit(expr) => self.analyze_int_lit_expr(expr),
            x => unimplemented!("{:?}", x),
        }
    }

    fn analyze_ident_expr(&mut self, ident: &'a token::Token) -> Result<Expr, Error<'a>> {
        let var = Rc::clone(self.get_var(ident)?);
        Ok(Expr {
            typ: Rc::clone(&var.typ),
            kind: ExprKind::VarExpr(VarExpr { var }),
            assignable: true,
        })
    }

    fn analyze_binary_expr(&mut self, binary: &'a ast::Binary) -> Result<Expr, Error<'a>> {
        let a_expr = self.analyze_expr(&binary.a)?;
        let b_expr = self.analyze_expr(&binary.b)?;

        let ret_type = match &binary.op.kind {
            token::TokenKind::Plus
            | token::TokenKind::Minus
            | token::TokenKind::Mul
            | token::TokenKind::Div
            | token::TokenKind::Mod
            | token::TokenKind::BitAnd
            | token::TokenKind::BitOr
            | token::TokenKind::BitXor
            | token::TokenKind::SHL
            | token::TokenKind::SHR
            | token::TokenKind::GT
            | token::TokenKind::LT
            | token::TokenKind::GTEq
            | token::TokenKind::LTEq => {
                if !matches!(a_expr.typ.kind, TypeKind::Int(_) | TypeKind::Float(_)) {
                    return Err(Error::CannotPerformOp {
                        typ: Rc::clone(&a_expr.typ),
                    });
                }
                if a_expr.typ != b_expr.typ {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&a_expr.typ),
                        got: Rc::clone(&b_expr.typ),
                    });
                }

                if matches!(
                    binary.op.kind,
                    token::TokenKind::GT
                        | token::TokenKind::LT
                        | token::TokenKind::GTEq
                        | token::TokenKind::LTEq
                ) {
                    Rc::clone(&self.type_analyzer.bool_type)
                } else {
                    Rc::clone(&a_expr.typ)
                }
            }
            token::TokenKind::And | token::TokenKind::Or => {
                if a_expr.typ != self.type_analyzer.bool_type
                    || b_expr.typ != self.type_analyzer.bool_type
                {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&self.type_analyzer.bool_type),
                        got: Rc::clone(&a_expr.typ),
                    });
                }
                Rc::clone(&self.type_analyzer.bool_type)
            }
            token::TokenKind::Eq | token::TokenKind::NotEq => {
                if a_expr.typ != b_expr.typ {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&a_expr.typ),
                        got: Rc::clone(&b_expr.typ),
                    });
                }
                Rc::clone(&self.type_analyzer.bool_type)
            }
            k => panic!("Unrecognized binary opearator {:?}", k),
        };

        let op = match &binary.op.kind {
            token::TokenKind::Plus => BinaryOpKind::Add,
            token::TokenKind::Minus => BinaryOpKind::Sub,
            token::TokenKind::Mul => BinaryOpKind::Mul,
            token::TokenKind::Div => BinaryOpKind::Div,
            token::TokenKind::Mod => BinaryOpKind::Mod,
            token::TokenKind::BitAnd => BinaryOpKind::BitAnd,
            token::TokenKind::BitOr => BinaryOpKind::BitOr,
            token::TokenKind::BitXor => BinaryOpKind::BitXor,
            token::TokenKind::SHL => BinaryOpKind::SHL,
            token::TokenKind::SHR => BinaryOpKind::SHR,
            token::TokenKind::GT => BinaryOpKind::GT,
            token::TokenKind::LT => BinaryOpKind::LT,
            token::TokenKind::GTEq => BinaryOpKind::GTEq,
            token::TokenKind::LTEq => BinaryOpKind::LTEq,
            token::TokenKind::And => BinaryOpKind::And,
            token::TokenKind::Or => BinaryOpKind::Or,
            token::TokenKind::Eq => BinaryOpKind::Eq,
            token::TokenKind::NotEq => BinaryOpKind::NotEq,
            k => panic!("Unrecognized binary opearator {:?}", k),
        };

        Ok(Expr {
            typ: Rc::clone(&ret_type),
            assignable: false,
            kind: ExprKind::BinaryOp(BinaryOp {
                a: Box::new(a_expr),
                b: Box::new(b_expr),
                op,
                typ: Rc::clone(&ret_type),
            }),
        })
    }

    fn analyze_int_lit_expr(&mut self, ident: &'a token::Token) -> Result<Expr, Error<'a>> {
        let value: i32 = ident.value.as_ref().unwrap().parse().unwrap(); // TODO: don't unwrap this. report error on wrong int.
        let typ = Rc::clone(&self.type_analyzer.i32_type);
        Ok(Expr {
            typ,
            kind: ExprKind::I32Lit(value),
            assignable: false,
        })
    }

    fn get_var(&self, name: &'a token::Token) -> Result<&Rc<Var>, Error<'a>> {
        if let Some(v) = self.locals.get(name.value.as_ref().unwrap()) {
            return Ok(v);
        }
        Err(Error::UndefinedIdent { token: name })
    }
}
