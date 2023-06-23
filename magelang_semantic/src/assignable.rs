use crate::def::GlobalId;
use crate::error::Loc;
use crate::expr::{get_expr_from_ast, Expr, ExprDb};
use crate::package::AstInfo;
use crate::scope::{Object, Scope, ScopeDb};
use crate::ty::{Type, TypeDb, TypeId};
use magelang_syntax::{AstNode, DerefExprNode, ExprNode, IndexExprNode, SelectionExprNode, Token};
use std::rc::Rc;

#[derive(Debug)]
pub struct Assignable {
    pub type_id: TypeId,
    pub kind: AssignableKind,
}

#[derive(Debug)]
pub enum AssignableKind {
    Invalid,
    Deref(Box<Assignable>),
    ArrayPtrIndex(Expr, Expr),
    Local(usize),
    Global(GlobalId),
    Element(Box<Assignable>, usize),
}

pub fn get_assignable_from_ast(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    expr_node: &ExprNode,
) -> Assignable {
    match expr_node {
        ExprNode::Ident(token) => get_assignable_from_ident_ast(db, ast_info, scope, token),
        ExprNode::Deref(node) => get_assignable_from_deref_ast(db, ast_info, scope, node),
        ExprNode::Selection(node) => get_assignable_from_selection_ast(db, ast_info, scope, node),
        ExprNode::Index(node) => get_assignable_from_index_ast(db, ast_info, scope, node),
        ExprNode::Grouped(node) => get_assignable_from_ast(db, ast_info, scope, &node.value),
        ExprNode::IntegerLiteral(..)
        | ExprNode::RealLiteral(..)
        | ExprNode::BooleanLit(..)
        | ExprNode::StringLit(..)
        | ExprNode::Binary(..)
        | ExprNode::Unary(..)
        | ExprNode::Call(..)
        | ExprNode::Cast(..)
        | ExprNode::ArrayPtr(..)
        | ExprNode::StructLit(..) => {
            db.not_assignable(Loc::new(ast_info.path, expr_node.get_pos()));
            Assignable {
                type_id: db.define_unknown_type(),
                kind: AssignableKind::Invalid,
            }
        }
    }
}

fn get_assignable_from_ident_ast(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    token: &Token,
) -> Assignable {
    let name = db.define_symbol(token.value.clone());
    let Some(object) = scope.get(name) else {
        db.undeclared_symbol(Loc::new(ast_info.path, token.pos), &token.value);
        return Assignable{type_id: db.define_unknown_type(), kind: AssignableKind::Invalid};
    };

    match object {
        Object::Invalid
        | Object::Import { .. }
        | Object::GenericStruct { .. }
        | Object::GenericFunc { .. }
        | Object::Type(..)
        | Object::Func { .. } => {
            db.not_assignable(Loc::new(ast_info.path, token.pos));
            Assignable {
                type_id: db.define_unknown_type(),
                kind: AssignableKind::Invalid,
            }
        }
        Object::Local { ty, idx } => Assignable {
            type_id: ty,
            kind: AssignableKind::Local(idx),
        },
        Object::Global(global_id) => Assignable {
            type_id: db.get_global_type_id(global_id),
            kind: AssignableKind::Global(global_id),
        },
    }
}

fn get_assignable_from_deref_ast(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &DerefExprNode,
) -> Assignable {
    let addr = get_assignable_from_ast(db, ast_info, scope, &node.value);
    let ty = db.get_type(addr.type_id);
    let Type::Pointer(element_type_id) = ty.as_ref() else {
        db.cannot_deref_non_pointer(Loc::new(ast_info.path, node.get_pos()));
        return Assignable {
            type_id: db.define_unknown_type(),
            kind: AssignableKind::Invalid,
        };
    };
    Assignable {
        type_id: *element_type_id,
        kind: AssignableKind::Deref(Box::new(addr)),
    }
}

fn get_assignable_from_selection_ast(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &SelectionExprNode,
) -> Assignable {
    if let Some(assianble) = get_assignable_from_package_selection(db, ast_info, scope, node) {
        return assianble;
    }

    let mut assignable = get_assignable_from_ast(db, ast_info, scope, &node.value);
    if let Type::Pointer(element_type_id) = db.get_type(assignable.type_id).as_ref() {
        assignable = Assignable {
            type_id: *element_type_id,
            kind: AssignableKind::Deref(Box::new(assignable)),
        };
    }

    let ty = db.get_type(assignable.type_id);
    if !ty.is_struct() {
        db.not_a_struct(Loc::new(ast_info.path, node.get_pos()), ty.display(db));
        return Assignable {
            type_id: db.define_unknown_type(),
            kind: AssignableKind::Invalid,
        };
    };
    let struct_field = db.get_struct_field(assignable.type_id.into());

    let field_name = db.define_symbol(node.selection.value.clone());
    let Some((idx, _, type_id)) = struct_field.fields.get_full(&field_name) else {
        db.no_such_field(Loc::new(ast_info.path, node.selection.pos), &node.selection.value);
        return Assignable {
            type_id: db.define_unknown_type(),
            kind: AssignableKind::Invalid,
        };
    };
    Assignable {
        type_id: *type_id,
        kind: AssignableKind::Element(Box::new(assignable), idx),
    }
}

fn get_assignable_from_package_selection(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Scope,
    node: &SelectionExprNode,
) -> Option<Assignable> {
    let ExprNode::Ident(package_node) = node.value.as_ref() else { return None };
    let package_name = db.define_symbol(package_node.value.clone());
    let object = scope.get(package_name)?;
    let Object::Import (package_name ) = object else { return None };
    let package_scope = db.get_package_scope(package_name);
    let selection = db.define_symbol(node.selection.value.clone());
    let object = package_scope.get(selection)?;
    Some(match object {
        Object::Invalid
        | Object::Import { .. }
        | Object::GenericStruct { .. }
        | Object::GenericFunc { .. }
        | Object::Type(..)
        | Object::Func { .. } => {
            db.not_assignable(Loc::new(ast_info.path, node.get_pos()));
            Assignable {
                type_id: db.define_unknown_type(),
                kind: AssignableKind::Invalid,
            }
        }
        Object::Local { ty, idx } => Assignable {
            type_id: ty,
            kind: AssignableKind::Local(idx),
        },
        Object::Global(global_id) => Assignable {
            type_id: db.get_global_type_id(global_id),
            kind: AssignableKind::Global(global_id),
        },
    })
}

fn get_assignable_from_index_ast(
    db: &(impl TypeDb + ScopeDb + ExprDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &IndexExprNode,
) -> Assignable {
    let value = get_expr_from_ast(db, ast_info, scope, &node.value, None);
    let ty = db.get_type(value.type_id);

    let Type::ArrayPtr(element_type_id) = ty.as_ref() else {
        db.not_indexable(Loc::new(ast_info.path, node.value.get_pos()));
        return Assignable {
            type_id: db.define_unknown_type(),
            kind: AssignableKind::Invalid,
        };
    };

    if node.index.is_empty() {
        db.unexpected_index_num(Loc::new(ast_info.path, node.get_pos()), 1, 0);
        return Assignable {
            type_id: *element_type_id,
            kind: AssignableKind::Invalid,
        };
    }
    if node.index.len() != 1 {
        db.unexpected_index_num(Loc::new(ast_info.path, node.get_pos()), 1, node.index.len());
    }

    let index = get_expr_from_ast(db, ast_info, scope, &node.index[0], None);
    let index_type = db.get_type(index.type_id);

    if !index_type.is_int() {
        db.non_int_index(Loc::new(ast_info.path, node.index[0].get_pos()));
        return Assignable {
            type_id: *element_type_id,
            kind: AssignableKind::Invalid,
        };
    }
    Assignable {
        type_id: *element_type_id,
        kind: AssignableKind::ArrayPtrIndex(value, index),
    }
}
