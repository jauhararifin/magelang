use crate::semantic::semantic::{AssignKind, BinaryOpKind, CastExpr, FnCall, Statement};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast, token};

use super::error::Error;
use super::semantic::{
    AssignStmt, BinaryOp, BlockStmt, Expr, ExprKind, ExprStmt, FnDef, FnExpr, FnId, IfStmt,
    ReturnStmt, Type, TypeKind, UnaryOp, UnaryOpKind, Var, VarExpr, VarStmt, WhileStmt,
};
use super::type_analyzer::TypeAnalyzer;

pub struct FuncAnalyzer<'a: 'b, 'b> {
    root: &'a ast::Root,
    type_analyzer: &'b mut TypeAnalyzer<'a>,

    fn_to_ast: HashMap<String, &'a ast::FnDecl>,
    funcs: HashMap<String, Rc<FnId>>,

    locals: Vec<HashMap<String, Rc<Var>>>,
    current_ret_type: Option<Rc<Type>>,
}

impl<'a, 'b> FuncAnalyzer<'a, 'b> {
    pub fn new(root: &'a ast::Root, type_analyzer: &'b mut TypeAnalyzer<'a>) -> Self {
        Self {
            root,
            type_analyzer,
            fn_to_ast: HashMap::new(),
            funcs: HashMap::new(),
            locals: Vec::new(),
            current_ret_type: None,
        }
    }

    pub fn analyze(&mut self) -> Result<HashMap<String, FnDef>, Error<'a>> {
        for decl in self.root.declarations.iter() {
            if let ast::Declaration::Fn(fn_decl) = decl {
                let name = &fn_decl.name.value.as_ref().unwrap().clone();
                self.fn_to_ast.insert(name.clone(), &fn_decl);

                let fn_id = self.analyze_func_id(fn_decl)?;
                self.funcs.insert(name.clone(), Rc::new(fn_id));
            }
        }

        let mut result = HashMap::new();
        for (name, typ) in std::mem::replace(&mut self.fn_to_ast, HashMap::new()).iter() {
            let typ = self.analyze_func(typ)?;
            result.insert(name.clone(), typ);
        }

        Ok(result)
    }

    fn analyze_func_id(&mut self, func: &'a ast::FnDecl) -> Result<FnId, Error<'a>> {
        let typ = self.type_analyzer.get_type_from_token(&func.name)?;
        let name = func.name.value.as_ref().unwrap().clone();

        Ok(FnId { name, typ })
    }

    fn analyze_func(&mut self, func: &'a ast::FnDecl) -> Result<FnDef, Error<'a>> {
        let func_id = Rc::clone(&self.funcs.get(func.name.value.as_ref().unwrap()).unwrap());

        let mut symbol_table = HashMap::new();
        if let TypeKind::Fn(fn_type) = &func_id.typ.kind {
            for param in fn_type.params.iter() {
                symbol_table.insert(param.name.clone(), Rc::clone(param));
            }
            self.current_ret_type = Some(Rc::clone(&fn_type.ret_type));
        }
        self.locals.push(symbol_table);

        let body = self.analyze_block_statement(&func.body)?;

        self.locals.pop();

        Ok(FnDef {
            id: Rc::clone(&func_id),
            body,
        })
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
        let var = Rc::new(Var {
            name: stmt.name.value.as_ref().unwrap().clone(),
            typ: self.type_analyzer.analyze_type(&stmt.typ),
        });

        let symbol_table = self.locals.last_mut().unwrap();
        symbol_table.insert(stmt.name.value.as_ref().unwrap().clone(), Rc::clone(&var));

        let value = stmt.value.as_ref();
        let value = if let Some(value) = value {
            Some(self.analyze_expr(value)?)
        } else {
            None
        };

        Ok(Statement::Var(VarStmt {
            receiver: Rc::clone(&var),
            value,
        }))
    }

    fn analyze_assign_statement(&mut self, stmt: &'a ast::Assign) -> Result<Statement, Error<'a>> {
        let target = self.analyze_expr(&stmt.receiver)?;
        if !target.assignable {
            return Err(Error::CannotAssignToValue {
                expr: &stmt.receiver,
                pos: &stmt.op.pos,
            });
        }

        let value = self.analyze_expr(&stmt.value)?;
        if !self.is_type_assignable_to(target.typ.borrow(), value.typ.borrow()) {
            return Err(Error::MismatchType {
                expected: Rc::clone(&target.typ),
                got: Rc::clone(&value.typ),
                pos: &stmt.value.pos,
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
                pos: &stmt.value.pos,
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
                pos: &stmt.cond.pos,
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
                pos: &stmt.cond.pos,
            });
        }

        let body = Box::new(self.analyze_block_statement(&stmt.body)?);
        Ok(Statement::While(WhileStmt { cond, body }))
    }

    fn analyze_block_statement(
        &mut self,
        stmt: &'a ast::BlockStatement,
    ) -> Result<Statement, Error<'a>> {
        self.locals.push(HashMap::new());

        let mut statements = Vec::new();
        for body in stmt.body.iter() {
            statements.push(self.analyze_statement(body)?);
        }

        self.locals.pop();

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
        match &expr.kind {
            ast::ExprKind::Ident(expr) => self.analyze_ident_expr(expr),
            ast::ExprKind::IntegerLit(expr) => self.analyze_int_lit_expr(expr),
            ast::ExprKind::FloatLit(expr) => self.analyze_float_lit_expr(expr),
            ast::ExprKind::BoolLit(expr) => self.analyze_bool_lit_expr(expr),
            ast::ExprKind::Binary(expr) => self.analyze_binary_expr(expr),
            ast::ExprKind::Unary(expr) => self.analyze_unary_expr(expr),
            ast::ExprKind::FunctionCall(expr) => self.analyze_func_call_expr(expr),
            ast::ExprKind::Cast(expr) => self.analyze_cast_expr(expr),
            ast::ExprKind::Selector(expr) => self.analyze_selector_expr(expr),
            x => unimplemented!("{:?}", x),
        }
    }

    fn analyze_ident_expr(&mut self, ident: &'a token::Token) -> Result<Expr, Error<'a>> {
        if let Some(var) = self.get_var(ident) {
            let var = Rc::clone(var);
            Ok(Expr {
                typ: Rc::clone(&var.typ),
                kind: ExprKind::VarExpr(VarExpr { var }),
                assignable: true,
            })
        } else if let Some(func_id) = self.funcs.get(ident.value.as_ref().unwrap()) {
            Ok(Expr {
                typ: Rc::clone(&func_id.typ),
                kind: ExprKind::FnExpr(FnExpr {
                    func: Rc::clone(&func_id),
                }),
                assignable: false,
            })
        } else {
            Err(Error::UndefinedIdent { token: ident })
        }
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
                        pos: &binary.op.pos,
                    });
                }
                if a_expr.typ != b_expr.typ {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&a_expr.typ),
                        got: Rc::clone(&b_expr.typ),
                        pos: &binary.b.pos,
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
                if a_expr.typ != self.type_analyzer.bool_type{
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&self.type_analyzer.bool_type),
                        got: Rc::clone(&a_expr.typ),
                        pos: &binary.a.pos,
                    });
                }

                if b_expr.typ != self.type_analyzer.bool_type {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&self.type_analyzer.bool_type),
                        got: Rc::clone(&a_expr.typ),
                        pos: &binary.b.pos,
                    });
                }

                Rc::clone(&self.type_analyzer.bool_type)
            }
            token::TokenKind::Eq | token::TokenKind::NotEq => {
                if a_expr.typ != b_expr.typ {
                    return Err(Error::MismatchType {
                        expected: Rc::clone(&a_expr.typ),
                        got: Rc::clone(&b_expr.typ),
                        pos: &binary.b.pos,
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

    fn analyze_float_lit_expr(&mut self, ident: &'a token::Token) -> Result<Expr, Error<'a>> {
        let value: f32 = ident.value.as_ref().unwrap().parse().unwrap(); // TODO: don't unwrap this. report error on wrong float.
        let typ = Rc::clone(&self.type_analyzer.f32_type);
        Ok(Expr {
            typ,
            kind: ExprKind::F32Lit(value),
            assignable: false,
        })
    }

    fn analyze_bool_lit_expr(&mut self, bool_value: &'a token::Token) -> Result<Expr, Error<'a>> {
        let value = match bool_value.kind {
            token::TokenKind::True => true,
            token::TokenKind::False => false,
            _ => panic!(
                "invalid token. Expected bool literal, found {:?}",
                bool_value
            ),
        };
        let typ = Rc::clone(&self.type_analyzer.bool_type);
        Ok(Expr {
            typ,
            kind: ExprKind::BoolLit(value),
            assignable: false,
        })
    }

    fn analyze_unary_expr(&mut self, unary: &'a ast::Unary) -> Result<Expr, Error<'a>> {
        let val_expr = self.analyze_expr(&unary.val)?;

        let ret_type = match &unary.op.kind {
            token::TokenKind::Plus | token::TokenKind::Minus | token::TokenKind::BitNot => {
                if !matches!(val_expr.typ.kind, TypeKind::Int(_) | TypeKind::Float(_)) {
                    return Err(Error::CannotPerformOp {
                        typ: Rc::clone(&val_expr.typ),
                        pos: &unary.op.pos,
                    });
                }
                Rc::clone(&val_expr.typ)
            }
            token::TokenKind::Mul => {
                if let TypeKind::Ptr(typ) = val_expr.typ.kind.borrow() {
                    Rc::clone(&typ.elem)
                } else {
                    return Err(Error::CannotPerformOp {
                        typ: Rc::clone(&val_expr.typ),
                        pos: &unary.op.pos,
                    });
                }
            }
            token::TokenKind::BitAnd => panic!(),
            token::TokenKind::Not => panic!(),
            k => panic!("Unrecognized unary opearator {:?}", k),
        };

        let kind = match &unary.op.kind {
            token::TokenKind::Plus => UnaryOpKind::Plus,
            token::TokenKind::Minus => UnaryOpKind::Minus,
            token::TokenKind::Mul => UnaryOpKind::Deref,
            token::TokenKind::BitNot => UnaryOpKind::BitNot,
            token::TokenKind::BitAnd => UnaryOpKind::Addr,
            token::TokenKind::Not => UnaryOpKind::Not,
            k => panic!("Unrecognized unary opearator {:?}", k),
        };

        let assignable = matches!(&unary.op.kind, token::TokenKind::Mul);

        Ok(Expr {
            typ: Rc::clone(&ret_type),
            assignable,
            kind: ExprKind::UnaryOp(UnaryOp {
                kind,
                typ: Rc::clone(&ret_type),
                a: Box::new(val_expr),
            }),
        })
    }

    fn analyze_func_call_expr(
        &mut self,
        func_call: &'a ast::FunctionCall,
    ) -> Result<Expr, Error<'a>> {
        let ptr = self.analyze_expr(func_call.ptr.borrow())?;

        let ret_type = if let ExprKind::FnExpr(fn_expr) = &ptr.kind {
            if let TypeKind::Fn(fn_type) = &fn_expr.func.typ.kind {
                Rc::clone(&fn_type.ret_type)
            } else {
                panic!("asdf");
            }
        } else {
            panic!("asdf");
        };

        let mut args = Vec::new();
        for arg in func_call.args.iter() {
            let arg_expr = self.analyze_expr(arg)?;
            args.push(arg_expr);
        }

        if let ExprKind::FnExpr(fn_expr) = &ptr.kind {
            if let TypeKind::Fn(fn_type) = &fn_expr.func.typ.kind {
                for (i, param) in fn_type.params.iter().enumerate() {
                    if !self.is_type_assignable_to(param.typ.borrow(), args[i].typ.borrow()) {
                        panic!("wrong argument"); // TODO: use error instead of panic.
                    }
                }
            }
        }

        Ok(Expr {
            typ: Rc::clone(&ret_type),
            assignable: false,
            kind: ExprKind::FnCall(FnCall {
                ptr: Box::new(ptr),
                args,
                typ: Rc::clone(&ret_type),
            }),
        })
    }

    fn analyze_cast_expr(&mut self, cast: &'a ast::Cast) -> Result<Expr, Error<'a>> {
        let target_type = self.type_analyzer.analyze_type(&cast.target);
        let source_expr = self.analyze_expr(&cast.val)?;

        let mut matched = matches!(&target_type.kind, TypeKind::Int(_))
            && matches!(&source_expr.typ.kind, TypeKind::Int(_));
        matched = matched
            || (matches!(&target_type.kind, TypeKind::Float(_))
                && matches!(&source_expr.typ.kind, TypeKind::Float(_)));
        matched = matched
            || (matches!(&target_type.kind, TypeKind::Ptr(_))
                && matches!(&source_expr.typ.kind, TypeKind::Ptr(_)));

        if matched {
            return Ok(Expr {
                typ: Rc::clone(&target_type),
                kind: ExprKind::Cast(CastExpr {
                    value: Box::new(source_expr),
                    target: Rc::clone(&target_type),
                }),
                assignable: false,
            });
        }

        return Err(Error::UnsupportedCast {
            target: Rc::clone(&target_type),
            source: Rc::clone(&source_expr.typ),
            pos: &cast.val.pos,
        });
    }

    fn analyze_selector_expr(&mut self, _selector: &'a ast::Selector) -> Result<Expr, Error<'a>> {
        unimplemented!();
    }

    fn get_var(&self, name: &'a token::Token) -> Option<&Rc<Var>> {
        for symbol_table in self.locals.iter().rev() {
            if let Some(v) = symbol_table.get(name.value.as_ref().unwrap()) {
                return Some(v);
            }
        }
        None
    }
}
