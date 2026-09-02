//! Phase 2: what the source declares. `Definitions` is built once and never mutated
//! afterwards; the `OnceCell`/`Cell` fields are filled by the later phases noted on them.

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::expr::Expr;
use crate::scope::{ImportEntry, Scope, Scopes, TypeEntry, ValueEntry};
use crate::ty::{BitSize, FloatType, Type, TypeRepr};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use magelang_syntax::{
    AnnotationNode, ErrorReporter, FunctionNode, GlobalNode, ItemNode, PackageNode, Pos,
    StructNode, TypeParameterNode,
};
use std::cell::{Cell, OnceCell};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Annotation {
    pub pos: Pos,
    pub name: String,
    pub arguments: Vec<String>,
}

pub(crate) struct StructDef<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) pos: Pos,
    pub(crate) type_params: &'a [Symbol<'a>],
    pub(crate) node: StructNode,
    /// The instance whose type arguments are the definition's own parameters. Phase 3.
    pub(crate) identity: OnceCell<&'a Type<'a>>,
}

pub(crate) struct FuncDef<'a> {
    pub(crate) def_id: DefId<'a>,
    /// Position of the signature; this is the position reported for the function.
    pub(crate) pos: Pos,
    pub(crate) type_params: &'a [Symbol<'a>],
    pub(crate) node: FunctionNode,
    pub(crate) annotations: Rc<[Annotation]>,
    /// The function type under the definition's own type parameters. Phase 3. For a
    /// non-generic function this is simply its type.
    pub(crate) sig: OnceCell<&'a Type<'a>>,
    /// Whether the body of a generic function passed the definition check. Phase 4.
    /// Instances of a failed definition are not checked again.
    pub(crate) body_ok: Cell<bool>,
}

pub(crate) struct GlobalDef<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) node: GlobalNode,
    pub(crate) annotations: Rc<[Annotation]>,
    /// Phase 3.
    pub(crate) ty: OnceCell<&'a Type<'a>>,
    /// Phase 4.
    pub(crate) value: OnceCell<Expr<'a>>,
}

#[derive(Default)]
pub(crate) struct Definitions<'a> {
    /// Top-level scope of every package, in load order.
    pub(crate) packages: IndexMap<Symbol<'a>, Scopes<'a>>,
    pub(crate) structs: IndexMap<DefId<'a>, StructDef<'a>>,
    pub(crate) funcs: IndexMap<DefId<'a>, FuncDef<'a>>,
    pub(crate) globals: IndexMap<DefId<'a>, GlobalDef<'a>>,
}

impl<'a> Definitions<'a> {
    pub(crate) fn package_scope(&self, package: Symbol<'a>) -> Option<&Scopes<'a>> {
        self.packages.get(package)
    }

    pub(crate) fn struct_def(&self, def_id: DefId<'a>) -> &StructDef<'a> {
        self.structs.get(&def_id).expect("struct definition exists")
    }

    pub(crate) fn func_def(&self, def_id: DefId<'a>) -> &FuncDef<'a> {
        self.funcs.get(&def_id).expect("function definition exists")
    }

    pub(crate) fn global_def(&self, def_id: DefId<'a>) -> &GlobalDef<'a> {
        self.globals.get(&def_id).expect("global definition exists")
    }
}

pub(crate) fn declare<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    package_asts: IndexMap<Symbol<'a>, PackageNode>,
) -> Definitions<'a> {
    let builtin_scope = builtin_type_scope(ctx);
    let mut defs = Definitions::default();

    for (package_name, ast) in package_asts {
        let mut imports = IndexMap::<Symbol<'a>, ImportEntry<'a>>::default();
        let mut types = IndexMap::<Symbol<'a>, TypeEntry<'a>>::default();
        let mut values = IndexMap::<Symbol<'a>, ValueEntry<'a>>::default();
        let mut import_pos = HashMap::<Symbol<'a>, Pos>::default();
        let mut type_pos = HashMap::<Symbol<'a>, Pos>::default();
        let mut value_pos = HashMap::<Symbol<'a>, Pos>::default();

        for item in ast.items {
            let name = ctx.define_symbol(item.name());
            let pos = item.pos();
            let def_id = DefId {
                package: package_name,
                name,
            };

            match item {
                ItemNode::Import(node) => {
                    let Ok(package_path) = std::str::from_utf8(&node.path.value) else {
                        ctx.errors.invalid_utf8_package(node.path.pos);
                        continue;
                    };
                    if declare_and_check(ctx, &mut import_pos, name, pos) {
                        continue;
                    }
                    let package = ctx.define_symbol(package_path);
                    imports.insert(name, ImportEntry { package });
                }
                ItemNode::Struct(node) => {
                    if declare_and_check(ctx, &mut type_pos, name, pos) {
                        continue;
                    }
                    let type_params = collect_type_params(ctx, &node.type_params);
                    types.insert(name, TypeEntry::Struct(def_id));
                    defs.structs.insert(
                        def_id,
                        StructDef {
                            def_id,
                            pos,
                            type_params,
                            node,
                            identity: OnceCell::new(),
                        },
                    );
                }
                ItemNode::Global(node) => {
                    if declare_and_check(ctx, &mut value_pos, name, pos) {
                        continue;
                    }
                    let annotations = build_annotations(ctx, &node.annotations);
                    values.insert(name, ValueEntry::Global(def_id));
                    defs.globals.insert(
                        def_id,
                        GlobalDef {
                            def_id,
                            node,
                            annotations,
                            ty: OnceCell::new(),
                            value: OnceCell::new(),
                        },
                    );
                }
                ItemNode::Function(node) => {
                    if declare_and_check(ctx, &mut value_pos, name, pos) {
                        continue;
                    }
                    let annotations = build_annotations(ctx, &node.signature.annotations);
                    let type_params = collect_type_params(ctx, &node.signature.type_params);
                    values.insert(name, ValueEntry::Func(def_id));
                    defs.funcs.insert(
                        def_id,
                        FuncDef {
                            def_id,
                            pos: node.signature.pos,
                            type_params,
                            node,
                            annotations,
                            sig: OnceCell::new(),
                            body_ok: Cell::new(false),
                        },
                    );
                }
            }
        }

        defs.packages.insert(
            package_name,
            Scopes {
                imports: Scope::new(imports),
                types: builtin_scope.new_child(types),
                values: Scope::new(values),
            },
        );
    }

    defs
}

fn declare_and_check<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    declared: &mut HashMap<Symbol<'a>, Pos>,
    name: Symbol<'a>,
    pos: Pos,
) -> bool {
    if let Some(declared_at) = declared.get(&name) {
        let declared_at = ctx.files.location(*declared_at);
        ctx.errors.redeclared_symbol(pos, declared_at, name);
        true
    } else {
        declared.insert(name, pos);
        false
    }
}

fn collect_type_params<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    nodes: &[TypeParameterNode],
) -> &'a [Symbol<'a>] {
    let mut names = Vec::with_capacity(nodes.len());
    let mut declared = HashMap::<Symbol<'a>, Pos>::default();
    for node in nodes {
        let name = ctx.define_symbol(&node.name.value);
        names.push(name);
        if let Some(declared_at) = declared.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(node.name.pos, declared_at, name);
        } else {
            declared.insert(name, node.name.pos);
        }
    }
    ctx.arena.alloc_slice_copy(&names)
}

fn build_annotations<E: ErrorReporter>(
    ctx: &Context<'_, '_, E>,
    nodes: &[AnnotationNode],
) -> Rc<[Annotation]> {
    let mut annotations = Vec::default();
    for node in nodes {
        let mut arguments = Vec::default();
        let mut valid = true;
        for arg in &node.arguments {
            let Ok(value) = std::str::from_utf8(&arg.value) else {
                ctx.errors.invalid_utf8_string(arg.pos);
                valid = false;
                continue;
            };
            arguments.push(value.to_string());
        }
        if valid {
            annotations.push(Annotation {
                pos: node.pos,
                name: node.name.value.clone(),
                arguments,
            });
        }
    }
    annotations.into()
}

fn builtin_type_scope<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) -> Scope<'a, TypeEntry<'a>> {
    let builtins = [
        ("i8", TypeRepr::Int(true, BitSize::I8)),
        ("i16", TypeRepr::Int(true, BitSize::I16)),
        ("i32", TypeRepr::Int(true, BitSize::I32)),
        ("i64", TypeRepr::Int(true, BitSize::I64)),
        ("isize", TypeRepr::Int(true, BitSize::ISize)),
        ("u8", TypeRepr::Int(false, BitSize::I8)),
        ("u16", TypeRepr::Int(false, BitSize::I16)),
        ("u32", TypeRepr::Int(false, BitSize::I32)),
        ("u64", TypeRepr::Int(false, BitSize::I64)),
        ("usize", TypeRepr::Int(false, BitSize::ISize)),
        ("f32", TypeRepr::Float(FloatType::F32)),
        ("f64", TypeRepr::Float(FloatType::F64)),
        ("void", TypeRepr::Void),
        ("opaque", TypeRepr::Opaque),
        ("bool", TypeRepr::Bool),
    ];

    let mut table = IndexMap::default();
    for (name, repr) in builtins {
        let ty = ctx.define_type(Type::anonymous(repr));
        table.insert(ctx.define_symbol(name), TypeEntry::Type(ty));
    }
    Scope::new(table)
}
