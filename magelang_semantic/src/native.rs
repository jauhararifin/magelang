use crate::ast::{SignatureNode, TagNode};
use crate::def::{FuncId, GenFuncId};
use crate::expr::ExprDb;
use crate::scope::{Object, Scope, ScopeKind};
use crate::symbol::SymbolId;
use crate::ty::{get_type_from_expr, substitute_type_with_typeargs, IntType, Type, TypeArgsId, TypeId};
use crate::value::value_from_string_lit;
use indexmap::IndexMap;
use magelang_syntax::TokenKind;
use std::collections::HashMap;
use std::iter::zip;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum NativeFunc {
    Invalid,
    SizeOf(TypeId),
    AlignOf(TypeId),
    Link(Rc<[u8]>),
}

pub trait NativeDb: ExprDb {
    fn get_native_func(&self, func_id: FuncId) -> NativeFunc;
    fn get_generic_native_func(&self, gen_func_id: GenFuncId) -> NativeFunc;
    fn get_generic_native_func_inst(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> NativeFunc;
}

pub fn get_native_func(db: &impl NativeDb, func_id: FuncId) -> NativeFunc {
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
            db.wrong_number_of_tag_arguments(linkname.loc, "linkname", 1, linkname.arguments.len());
            return NativeFunc::Invalid;
        }

        let value_tok = linkname.arguments.first().unwrap();
        if value_tok.kind != TokenKind::StringLit {
            db.report_error(
                linkname.loc,
                format!("Expected string literal as linkname, but found {}", value_tok.kind),
            );
            return NativeFunc::Invalid;
        }

        let Some(linkname) = value_from_string_lit(&value_tok.value) else {
            return NativeFunc::Invalid;
        };

        NativeFunc::Link(linkname)
    } else {
        db.invalid_native_func(signature.loc);
        NativeFunc::Invalid
    }
}

pub fn get_generic_native_func(db: &impl NativeDb, gen_func_id: GenFuncId) -> NativeFunc {
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
        let type_id = check_builtin_sizeof(db, &scope, signature);
        NativeFunc::SizeOf(type_id)
    } else if tag_map.contains_key("builtin_align_of") {
        let type_id = check_builtin_alignof(db, &scope, signature);
        NativeFunc::AlignOf(type_id)
    } else {
        db.invalid_native_func(signature.loc);
        NativeFunc::Invalid
    }
}

fn check_builtin_sizeof(db: &impl NativeDb, scope: &Rc<Scope>, signature: &SignatureNode) -> TypeId {
    check_builtin_alignof(db, scope, signature)
}

fn check_builtin_alignof(db: &impl NativeDb, scope: &Rc<Scope>, signature: &SignatureNode) -> TypeId {
    if !signature.parameters.is_empty() {
        db.wrong_number_of_arguments(signature.loc, 0, signature.parameters.len());
    }
    let return_type = signature
        .return_type
        .as_ref()
        .map(|type_expr| get_type_from_expr(db, scope, type_expr))
        .unwrap_or(db.define_void_type());

    let expected_return_type = Type::Int(IntType::usize());
    let expected_return_type_id = db.define_type(Rc::new(expected_return_type));
    if return_type != expected_return_type_id {
        db.invalid_signature_return_type(
            signature.loc,
            db.get_type(expected_return_type_id).display(db),
            db.get_type(return_type).display(db),
        );
    }

    if signature.type_params.len() != 1 {
        db.wrong_number_of_type_arguments(signature.loc, 1, signature.type_params.len());
        return db.define_unknown_type();
    }

    let symbol_id = db.define_symbol(signature.type_params[0].name.value.clone());
    db.define_generic_arg_type(symbol_id)
}

pub fn get_generic_native_func_inst(db: &impl NativeDb, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> NativeFunc {
    let func_node = db.get_ast_by_def_id(gen_func_id.into()).expect("ast not found");
    let signature = func_node
        .as_native_function()
        .expect("ast is not a native generic function node");
    assert!(
        !signature.type_params.is_empty(),
        "ast is not a generic function, but a normal function"
    );

    let type_params = &signature.type_params;
    let scope = db.get_package_scope(gen_func_id.package());
    let typeargs = db.get_typeargs(typeargs_id);
    let mut type_table = IndexMap::<SymbolId, Object>::default();
    for (typeparam, typearg) in zip(type_params.iter(), typeargs.iter()) {
        let name = db.define_symbol(typeparam.name.value.clone());
        type_table.entry(name).or_insert(Object::Type(*typearg));
    }
    let scope = scope.new_child(ScopeKind::Basic, type_table);

    let native_func = get_generic_native_func(db, gen_func_id);
    match native_func {
        NativeFunc::Invalid => NativeFunc::Invalid,
        NativeFunc::SizeOf(type_id) => NativeFunc::SizeOf(substitute_type_with_typeargs(db, &scope, type_id)),
        NativeFunc::AlignOf(type_id) => NativeFunc::AlignOf(substitute_type_with_typeargs(db, &scope, type_id)),
        NativeFunc::Link(name) => NativeFunc::Link(name),
    }
}
