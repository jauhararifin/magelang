use crate::assignable::{get_assignable_from_ast, Assignable};
use crate::def::{FuncId, GenFuncId};
use crate::error::Loc;
use crate::expr::{get_expr_from_ast, Expr, ExprDb, ExprKind};
use crate::native::{NativeDb, NativeFunc};
use crate::package::AstInfo;
use crate::scope::{get_typeparams_scope, Object, Scope, ScopeKind};
use crate::symbol::SymbolId;
use crate::ty::{get_type_from_expr, is_assignable, Type, TypeArgsId};
use indexmap::IndexMap;
use magelang_syntax::{
    AssignStatementNode, AstNode, BlockStatementNode, ElseNode, IfStatementNode, LetKind, LetStatementNode,
    ReturnStatementNode, SignatureNode, StatementNode, Token, WhileStatementNode,
};
use std::iter::zip;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Invalid,
    NewLocal(Expr),
    Block(Vec<Statement>),
    If(IfStatement),
    While(WhileStatement),
    Return(Option<Expr>),
    Expr(Expr),
    Assign(Assignable, Expr),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

#[derive(Debug)]
pub enum FuncBody {
    User(Statement),
    Native(NativeFunc),
}

pub trait StatementDb: ExprDb {
    fn get_func_body(&self, func_id: FuncId) -> Rc<FuncBody>;
    fn get_generic_func_body(&self, gen_func_id: GenFuncId) -> Rc<FuncBody>;
    fn get_generic_func_inst_body(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> Rc<FuncBody>;
}

pub fn get_func_body(db: &(impl StatementDb + NativeDb), func_id: FuncId) -> Rc<FuncBody> {
    let ast_info = db.get_package_ast(func_id.package());
    let func_node = db.get_ast_by_def_id(func_id.into()).expect("ast not found");

    if func_node.is_native_function() {
        return Rc::new(FuncBody::Native(db.get_native_func(func_id)));
    }

    let func_node = func_node.as_function().expect("ast if not a function node");

    assert!(
        func_node.signature.type_params.is_empty(),
        "ast is not a function, but a generic func"
    );

    let scope = db.get_package_scope(func_id.package());
    let (scope, last_unused_local) = get_func_scope(db, &ast_info, &scope, &func_node.signature);

    let result = get_stmt_from_block_ast(
        db,
        &ast_info,
        &scope,
        last_unused_local,
        ScopeKind::Basic,
        &func_node.body,
    );

    let return_type_id = scope.return_type().unwrap();
    let return_type = db.get_type(return_type_id);
    if !return_type.is_void() && !result.is_returning {
        db.missing_return_statement(Loc::new(ast_info.path, func_node.body.get_pos()));
    }

    Rc::new(FuncBody::User(result.statement))
}

fn get_func_scope(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    sign_node: &SignatureNode,
) -> (Rc<Scope>, usize) {
    let mut symbol_table = IndexMap::<SymbolId, Object>::default();
    let mut last_unused_local = 0;
    for param in &sign_node.parameters {
        let name = db.define_symbol(param.name.value.clone());
        if !symbol_table.contains_key(&name) {
            let type_id = get_type_from_expr(db, ast_info, scope, &param.type_expr);
            symbol_table.insert(
                name,
                Object::Local {
                    ty: type_id,
                    idx: last_unused_local,
                },
            );
            last_unused_local += 1;
        }
    }

    let return_type = if let Some(ref return_type_node) = sign_node.return_type {
        get_type_from_expr(db, ast_info, scope, return_type_node)
    } else {
        db.define_void_type()
    };

    let scope = scope.new_child(ScopeKind::Function(return_type), symbol_table);
    (scope, last_unused_local)
}

pub fn get_generic_func_body(db: &(impl StatementDb + NativeDb), gen_func_id: GenFuncId) -> Rc<FuncBody> {
    let ast_info = db.get_package_ast(gen_func_id.package());
    let func_node = db.get_ast_by_def_id(gen_func_id.into()).expect("ast not found");

    if func_node.is_native_function() {
        return Rc::new(FuncBody::Native(db.get_generic_native_func(gen_func_id)));
    }

    let func_node = func_node.as_function().expect("ast is not generic function node");
    assert!(
        !func_node.signature.type_params.is_empty(),
        "ast is not a generic function, but a normal function"
    );

    let scope = db.get_package_scope(gen_func_id.package());
    let scope = get_typeparams_scope(db, &ast_info, &scope, &func_node.signature.type_params);
    let (scope, last_unused_local) = get_func_scope(db, &ast_info, &scope, &func_node.signature);

    let result = get_stmt_from_block_ast(
        db,
        &ast_info,
        &scope,
        last_unused_local,
        ScopeKind::Basic,
        &func_node.body,
    );

    let return_type_id = scope.return_type().unwrap();
    let return_type = db.get_type(return_type_id);
    if !return_type.is_void() && !result.is_returning {
        db.missing_return_statement(Loc::new(ast_info.path, func_node.body.get_pos()));
    }

    Rc::new(FuncBody::User(result.statement))
}

pub fn get_generic_func_inst_body(
    db: &(impl StatementDb + NativeDb),
    gen_func_id: GenFuncId,
    typeargs_id: TypeArgsId,
) -> Rc<FuncBody> {
    let ast_info = db.get_package_ast(gen_func_id.package());
    let func_node = db.get_ast_by_def_id(gen_func_id.into()).expect("ast not found");

    if func_node.is_native_function() {
        return Rc::new(FuncBody::Native(
            db.get_generic_native_func_inst(gen_func_id, typeargs_id),
        ));
    }

    let func_node = func_node.as_function().expect("ast is not generic function node");
    assert!(
        !func_node.signature.type_params.is_empty(),
        "ast is not a generic function, but a normal function"
    );

    let scope = db.get_package_scope(gen_func_id.package());

    let typeargs = db.get_typeargs(typeargs_id);
    let mut type_table = IndexMap::<SymbolId, Object>::default();
    for (typeparam, typearg) in zip(func_node.signature.type_params.iter(), typeargs.iter()) {
        let name = db.define_symbol(typeparam.name.value.clone());
        type_table.entry(name).or_insert(Object::Type(*typearg));
    }
    let scope = scope.new_child(ScopeKind::Basic, type_table);

    let (scope, last_unused_local) = get_func_scope(db, &ast_info, &scope, &func_node.signature);

    let result = get_stmt_from_block_ast(
        db,
        &ast_info,
        &scope,
        last_unused_local,
        ScopeKind::Basic,
        &func_node.body,
    );

    let return_type_id = scope.return_type().unwrap();
    let return_type = db.get_type(return_type_id);
    if !return_type.is_void() && !result.is_returning {
        db.missing_return_statement(Loc::new(ast_info.path, func_node.body.get_pos()));
    }

    Rc::new(FuncBody::User(result.statement))
}

struct StatementResult {
    statement: Statement,
    new_scope: Option<Rc<Scope>>,
    is_returning: bool,
    last_unused_local: usize,
}

fn get_stmt_from_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &StatementNode,
) -> StatementResult {
    match node {
        StatementNode::Let(node) => get_stmt_from_let_ast(db, ast_info, scope, last_unused_local, node),
        StatementNode::Assign(node) => get_stmt_from_assign_ast(db, ast_info, scope, last_unused_local, node),
        StatementNode::Block(node) => {
            get_stmt_from_block_ast(db, ast_info, scope, last_unused_local, ScopeKind::Basic, node)
        }
        StatementNode::If(node) => get_stmt_from_if_ast(db, ast_info, scope, last_unused_local, node),
        StatementNode::While(node) => get_stmt_from_while_ast(db, ast_info, scope, last_unused_local, node),
        StatementNode::Continue(token) => get_stmt_from_continue_ast(db, ast_info, scope, last_unused_local, token),
        StatementNode::Break(token) => get_stmt_from_break_ast(db, ast_info, scope, last_unused_local, token),
        StatementNode::Return(node) => get_stmt_from_return_ast(db, ast_info, scope, last_unused_local, node),
        StatementNode::Expr(node) => StatementResult {
            statement: Statement::Expr(get_expr_from_ast(
                db,
                ast_info,
                scope,
                node,
                Some(db.define_void_type()),
            )),
            new_scope: None,
            is_returning: false,
            last_unused_local,
        },
    }
}

fn get_stmt_from_let_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &LetStatementNode,
) -> StatementResult {
    let expr = match &node.kind {
        LetKind::TypeOnly { ty } => {
            let type_id = get_type_from_expr(db, ast_info, scope, ty);
            Expr {
                type_id,
                kind: ExprKind::Zero(type_id),
            }
        }
        LetKind::TypeValue { ty, value } => {
            let type_id = get_type_from_expr(db, ast_info, scope, ty);
            let value_expr = get_expr_from_ast(db, ast_info, scope, value, Some(type_id));
            let mut kind = value_expr.kind;
            if !is_assignable(db, value_expr.type_id, type_id) {
                db.type_mismatch(
                    Loc::new(ast_info.path, value.get_pos()),
                    db.get_type(type_id).display(db),
                    db.get_type(value_expr.type_id).display(db),
                );
                kind = ExprKind::Invalid;
            }
            Expr { type_id, kind }
        }
        LetKind::ValueOnly { value } => get_expr_from_ast(db, ast_info, scope, value, None),
    };

    let name = db.define_symbol(node.name.value.clone());
    let mut new_table = IndexMap::<SymbolId, Object>::default();
    new_table.insert(
        name,
        Object::Local {
            ty: expr.type_id,
            idx: last_unused_local,
        },
    );
    StatementResult {
        statement: Statement::NewLocal(expr),
        new_scope: Some(scope.new_child(ScopeKind::Basic, new_table)),
        is_returning: false,
        last_unused_local: last_unused_local + 1,
    }
}

fn get_stmt_from_assign_ast(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &AssignStatementNode,
) -> StatementResult {
    let receiver = get_assignable_from_ast(db, ast_info, scope, &node.receiver);

    let value = get_expr_from_ast(db, ast_info, scope, &node.value, Some(receiver.type_id));
    if !is_assignable(db, value.type_id, receiver.type_id) {
        db.type_mismatch(
            Loc::new(ast_info.path, node.value.get_pos()),
            db.get_type(receiver.type_id).display(db),
            db.get_type(value.type_id).display(db),
        );
        return StatementResult {
            statement: Statement::Invalid,
            new_scope: None,
            is_returning: false,
            last_unused_local,
        };
    }

    let statement = Statement::Assign(receiver, value);
    StatementResult {
        statement,
        new_scope: None,
        is_returning: false,
        last_unused_local,
    }
}

fn get_stmt_from_block_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    scope_kind: ScopeKind,
    node: &BlockStatementNode,
) -> StatementResult {
    let mut scope = scope.new_child(scope_kind, IndexMap::default());
    let mut statements = Vec::default();
    let mut last_unused_local = last_unused_local;
    let mut is_returning = false;
    let mut unreachable_error = false;
    for stmt in &node.statements {
        if is_returning && !unreachable_error {
            db.unreachable_stmt(Loc::new(ast_info.path, stmt.get_pos()));
            unreachable_error = true;
        }

        let stmt_result = get_stmt_from_ast(db, &ast_info, &scope, last_unused_local, stmt);
        statements.push(stmt_result.statement);
        last_unused_local = stmt_result.last_unused_local;
        if stmt_result.is_returning {
            is_returning = true;
        }
        if let Some(new_scope) = stmt_result.new_scope {
            scope = new_scope;
        }
    }
    StatementResult {
        statement: Statement::Block(statements),
        new_scope: None,
        is_returning,
        last_unused_local,
    }
}

fn get_stmt_from_if_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &IfStatementNode,
) -> StatementResult {
    let bool_type_id = db.define_type(Rc::new(Type::Bool));
    let cond = get_expr_from_ast(db, ast_info, scope, &node.condition, Some(bool_type_id));

    if cond.type_id != bool_type_id {
        db.type_mismatch(
            Loc::new(ast_info.path, node.condition.get_pos()),
            Type::Bool.display(db),
            db.get_type(cond.type_id).display(db),
        );
    }

    let result = get_stmt_from_block_ast(db, ast_info, scope, last_unused_local, ScopeKind::Basic, &node.body);
    let body = result.statement;
    let body_is_returning = result.is_returning;
    let last_unused_local = result.last_unused_local;

    let else_result = match &node.else_node {
        ElseNode::None => None,
        ElseNode::ElseIf(stmt) => Some(get_stmt_from_if_ast(
            db,
            ast_info,
            scope,
            result.last_unused_local,
            stmt,
        )),
        ElseNode::Else(stmt) => Some(get_stmt_from_block_ast(
            db,
            ast_info,
            scope,
            result.last_unused_local,
            ScopeKind::Basic,
            stmt,
        )),
    };

    let (else_stmt, last_unused_local, else_is_returning) = if let Some(result) = else_result {
        (Some(result.statement), result.last_unused_local, result.is_returning)
    } else {
        (None, last_unused_local, false)
    };

    StatementResult {
        statement: Statement::If(IfStatement {
            cond,
            body: Box::new(body),
            else_stmt: else_stmt.map(Box::new),
        }),
        new_scope: None,
        is_returning: body_is_returning && else_is_returning,
        last_unused_local,
    }
}

fn get_stmt_from_while_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &WhileStatementNode,
) -> StatementResult {
    let bool_type_id = db.define_type(Rc::new(Type::Bool));
    let condition = get_expr_from_ast(db, ast_info, scope, &node.condition, Some(bool_type_id));

    if condition.type_id != bool_type_id {
        db.type_mismatch(
            Loc::new(ast_info.path, node.condition.get_pos()),
            Type::Bool.display(db),
            db.get_type(condition.type_id).display(db),
        );
    }

    let body_stmt = get_stmt_from_block_ast(db, ast_info, scope, last_unused_local, ScopeKind::Loop, &node.body);
    StatementResult {
        statement: Statement::While(WhileStatement {
            cond: condition,
            body: Box::new(body_stmt.statement),
        }),
        new_scope: None,
        is_returning: false,
        last_unused_local: body_stmt.last_unused_local,
    }
}

fn get_stmt_from_continue_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    token: &Token,
) -> StatementResult {
    let statement = if !scope.inside_loop() {
        db.not_in_a_loop(Loc::new(ast_info.path, token.pos.clone()), "continue");
        Statement::Invalid
    } else {
        Statement::Continue
    };
    StatementResult {
        statement,
        new_scope: None,
        is_returning: false,
        last_unused_local,
    }
}

fn get_stmt_from_break_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    token: &Token,
) -> StatementResult {
    let statement = if !scope.inside_loop() {
        db.not_in_a_loop(Loc::new(ast_info.path, token.pos.clone()), "break");
        Statement::Invalid
    } else {
        Statement::Break
    };
    StatementResult {
        statement,
        new_scope: None,
        is_returning: false,
        last_unused_local,
    }
}

fn get_stmt_from_return_ast(
    db: &impl StatementDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    last_unused_local: usize,
    node: &ReturnStatementNode,
) -> StatementResult {
    let return_type_id = scope.return_type().expect("not in a function");
    let return_type = db.get_type(return_type_id);

    let statement = if let Some(ref value_node) = node.value {
        let value_expr = get_expr_from_ast(db, ast_info, scope, value_node, Some(return_type_id));
        if !is_assignable(db, value_expr.type_id, return_type_id) {
            db.type_mismatch(
                Loc::new(ast_info.path, node.get_pos()),
                return_type.display(db),
                db.get_type(value_expr.type_id).display(db),
            );
            Statement::Return(Some(Expr {
                type_id: return_type_id,
                kind: ExprKind::Invalid,
            }))
        } else {
            Statement::Return(Some(Expr {
                type_id: return_type_id,
                kind: value_expr.kind,
            }))
        }
    } else if !return_type.is_void() {
        db.type_mismatch(
            Loc::new(ast_info.path, node.get_pos()),
            return_type.display(db),
            Type::Void.display(db),
        );
        Statement::Return(Some(Expr {
            type_id: return_type_id,
            kind: ExprKind::Invalid,
        }))
    } else {
        Statement::Return(None)
    };

    StatementResult {
        statement,
        new_scope: None,
        is_returning: true,
        last_unused_local,
    }
}
