use crate::def::{FuncId, GenFuncId};
use crate::error::Loc;
use crate::expr::ExprDb;
use crate::package::AstInfo;
use crate::scope::Scope;
use crate::ty::{get_type_from_expr, IntType, Type};
use crate::value::value_from_string_lit;
use magelang_syntax::{AstNode, SignatureNode, TagNode, TokenKind};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum NativeFunc {
    Invalid,
    SizeOf,
    AlignOf,
    Link(Rc<[u8]>),
}

pub trait NativeDb: ExprDb {
    fn get_native_func(&self, func_id: FuncId) -> NativeFunc;
    fn get_generic_native_func(&self, gen_func_id: GenFuncId) -> NativeFunc;
}

pub fn get_native_func(db: &impl NativeDb, func_id: FuncId) -> NativeFunc {
    let ast_info = db.get_package_ast(func_id.package());
    let func_node = db.get_ast_by_def_id(func_id.into()).expect("ast not found");
    let signature = func_node
        .as_native_function()
        .expect("ast is not a native function node");
    assert!(
        signature.type_params.is_empty(),
        "ast is not a function, but a generic function"
    );

    let mut tag_map = HashMap::<&str, &TagNode>::default();
    for tag in &signature.tags {
        tag_map.insert(tag.name.value.as_ref(), tag);
    }

    if tag_map.contains_key("linkname") {
        let linkname = tag_map.get("linkname").unwrap();

        if linkname.arguments.len() != 1 {
            db.wrong_number_of_tag_arguments(
                Loc::new(ast_info.path, linkname.get_pos()),
                "linkname",
                1,
                linkname.arguments.len(),
            );
            return NativeFunc::Invalid;
        }

        let value_tok = linkname.arguments.first().unwrap();
        if value_tok.kind != TokenKind::StringLit {
            db.report_error(
                Loc::new(ast_info.path, linkname.get_pos()),
                format!("Expected string literal as linkname, but found {}", value_tok.kind),
            );
            return NativeFunc::Invalid;
        }

        let Some(linkname) = value_from_string_lit(&value_tok.value) else {
            return NativeFunc::Invalid;
        };

        NativeFunc::Link(linkname)
    } else {
        db.invalid_native_func(Loc::new(ast_info.path, signature.get_pos()));
        NativeFunc::Invalid
    }
}

pub fn get_generic_native_func(db: &impl NativeDb, gen_func_id: GenFuncId) -> NativeFunc {
    let ast_info = db.get_package_ast(gen_func_id.package());
    let func_node = db.get_ast_by_def_id(gen_func_id.into()).expect("ast not found");
    let signature = func_node
        .as_native_function()
        .expect("ast is not a native generic function node");
    assert!(
        !signature.type_params.is_empty(),
        "ast is not a generic function, but a normal function"
    );

    let mut tag_map = HashMap::<&str, &TagNode>::default();
    for tag in &signature.tags {
        tag_map.insert(tag.name.value.as_ref(), tag);
    }

    let scope = db.get_package_scope(gen_func_id.package());
    if tag_map.contains_key("builtin_size_of") {
        check_builtin_sizeof(db, &ast_info, &scope, signature);
        NativeFunc::SizeOf
    } else if tag_map.contains_key("builtin_align_of") {
        check_builtin_alignof(db, &ast_info, &scope, signature);
        NativeFunc::AlignOf
    } else {
        db.invalid_native_func(Loc::new(ast_info.path, signature.get_pos()));
        NativeFunc::Invalid
    }
}

fn check_builtin_sizeof(db: &impl NativeDb, ast_info: &AstInfo, scope: &Rc<Scope>, signature: &SignatureNode) {
    check_builtin_alignof(db, ast_info, scope, signature);
}

fn check_builtin_alignof(db: &impl NativeDb, ast_info: &AstInfo, scope: &Rc<Scope>, signature: &SignatureNode) {
    if !signature.parameters.is_empty() {
        db.wrong_number_of_arguments(
            Loc::new(ast_info.path, signature.get_pos()),
            0,
            signature.parameters.len(),
        );
    }
    let return_type = signature
        .return_type
        .as_ref()
        .map(|type_expr| get_type_from_expr(db, ast_info, scope, type_expr))
        .unwrap_or(db.define_void_type());

    let expected_return_type = Type::Int(IntType::usize());
    let expected_return_type_id = db.define_type(Rc::new(expected_return_type));
    if return_type != expected_return_type_id {
        db.invalid_signature_return_type(
            Loc::new(ast_info.path, signature.get_pos()),
            db.get_type(expected_return_type_id).display(db),
            db.get_type(return_type).display(db),
        );
    }
}
