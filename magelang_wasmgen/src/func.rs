use crate::context::Context;
use crate::errors::CodegenError;
use crate::mangling::Mangle;
use crate::ty::{build_val_type, TypeManager};
use magelang_syntax::{ErrorReporter, Pos};
use magelang_typecheck::{Annotation, Func, Statement};
use std::collections::HashMap;
use wasm_helper as wasm;

pub(crate) struct Function<'ctx> {
    name: &'ctx str,
    pos: Pos,
    type_id: wasm::TypeIdx,
    import: Option<(&'ctx str, &'ctx str)>,
    export: Option<&'ctx str>,
    intrinsic: Option<Intrinsic>,
    body: Option<&'ctx Statement<'ctx>>,
    is_main: bool,
}

pub(crate) enum Intrinsic {
    MemorySize,
    MemoryGrow,
    F32Floor,
    F64Floor,
    F32Ceil,
    F64Ceil,
    DataEnd,
    TableGet,
    TableSet,
    SizeOf,
    AlignOf,
    Trap,
}

const WASM_IMPORT_ANNOTATION: &str = "wasm_import";
const WASM_EXPORT_ANNOTATION: &str = "wasm_export";
const INTRINSIC_ANNOTATION: &str = "intrinsic";
const MAIN_ANNOTATION: &str = "main";

pub(crate) fn setup_functions<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    type_manager: &TypeManager<'ctx>,
) -> Vec<Function<'ctx>> {
    let mut functions = init_functions(ctx, type_manager);

    // it is important that the imported function appear first due to wasm specification
    // require all imported functions to occupied the first indexes.
    functions.sort_by(|a, b| match (a.import, b.import) {
        (Some(..), Some(..)) | (None, None) => a.name.cmp(b.name),
        (Some(..), None) => std::cmp::Ordering::Less,
        (None, Some(..)) => std::cmp::Ordering::Greater,
    });

    check_functions(ctx, &functions);

    functions
}

fn init_functions<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    type_manager: &TypeManager<'ctx>,
) -> Vec<Function<'ctx>> {
    let mut results = Vec::default();

    let functions = ctx.module.packages.iter().flat_map(|pkg| &pkg.functions);
    for func in functions {
        let func_type = func.ty.as_func().expect("func type is not a function");

        let mut parameters = Vec::default();
        for param in func_type.params {
            let val_types = build_val_type(param);
            parameters.extend(val_types);
        }

        let returns = build_val_type(func_type.return_type);
        let func_wasm_type_id = type_manager.get_func_type(wasm::FuncType {
            parameters,
            returns,
        });

        let func_name = func.get_mangled_name(ctx);

        let body = if matches!(func.statement, Statement::Native) {
            None
        } else {
            Some(func.statement)
        };

        let mut result = Function {
            name: func_name,
            pos: func.pos,
            type_id: func_wasm_type_id,
            import: None,
            export: None,
            intrinsic: None,
            body,
            is_main: false,
        };

        for annotation in func.annotations.iter().rev() {
            match annotation.name.as_str() {
                WASM_IMPORT_ANNOTATION => {
                    if annotation.arguments.len() != 2 {
                        ctx.errors.annotation_arg_mismatch(&annotation, 2);
                        continue;
                    }

                    if result.import.is_some() {
                        ctx.errors.duplicated_annotation(&annotation);
                        continue;
                    }

                    if func.typeargs.is_some() {
                        ctx.errors.import_generic_func(func.pos);
                        continue;
                    }

                    let import_module = annotation.arguments.get(0).unwrap();
                    let import_name = annotation.arguments.get(1).unwrap();
                    result.import = Some((import_module, import_name));
                }
                WASM_EXPORT_ANNOTATION => {
                    if annotation.arguments.len() != 1 {
                        ctx.errors.annotation_arg_mismatch(&annotation, 1);
                        continue;
                    }

                    if result.export.is_some() {
                        ctx.errors.duplicated_annotation(&annotation);
                        continue;
                    }

                    if func.typeargs.is_some() {
                        ctx.errors.import_generic_func(func.pos);
                        continue;
                    }

                    let export_name = annotation.arguments.get(0).unwrap();
                    result.export = Some(export_name);
                }
                INTRINSIC_ANNOTATION => {
                    if annotation.arguments.len() != 1 {
                        ctx.errors.annotation_arg_mismatch(&annotation, 1);
                        continue;
                    }

                    if result.intrinsic.is_some() {
                        ctx.errors.duplicated_annotation(&annotation);
                        continue;
                    }

                    if let Some(instr) = setup_func_intrinsic(ctx, annotation, func) {
                        result.intrinsic = Some(instr);
                    }
                }
                MAIN_ANNOTATION => {
                    if !annotation.arguments.is_empty() {
                        ctx.errors.annotation_arg_mismatch(&annotation, 0);
                        continue;
                    }
                    if result.is_main {
                        ctx.errors.duplicated_annotation(&annotation);
                        continue;
                    }

                    let is_valid = func.typeargs.is_none()
                        && func.ty.as_func().is_some_and(|f| f.params.is_empty())
                        && func.ty.as_func().is_some_and(|f| f.return_type.is_void());
                    if !is_valid {
                        ctx.errors.invalid_main_signature(func.pos);
                        continue;
                    }

                    result.is_main = true;
                }
                _ => ctx.errors.unknown_annotation(annotation),
            }
        }

        results.push(result);
    }

    results
}

const INTRINSIC_MEMORY_SIZE: &str = "memory.size";
const INTRINSIC_MEMORY_GROW: &str = "memory.grow";
const INTRINSIC_F32_FLOOR: &str = "f32.floor";
const INTRINSIC_F64_FLOOR: &str = "f64.floor";
const INTRINSIC_F32_CEIL: &str = "f32.ceil";
const INTRINSIC_F64_CEIL: &str = "f64.ceil";
const INTRINSIC_DATA_END: &str = "data.end";
const INTRINSIC_TABLE_GET: &str = "table.get";
const INTRINSIC_TABLE_SET: &str = "table.set";
const INTRINSIC_SIZE_OF: &str = "size_of";
const INTRINSIC_ALIGN_OF: &str = "align_of";
const INTRINSIC_TRAP: &str = "unreachable";

fn setup_func_intrinsic<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    annotation: &Annotation,
    func: &Func<'ctx>,
) -> Option<Intrinsic> {
    let intrinsic_name = annotation.name.as_str();
    let intrinsic = match intrinsic_name {
        INTRINSIC_MEMORY_SIZE => Some(Intrinsic::MemorySize),
        INTRINSIC_MEMORY_GROW => Some(Intrinsic::MemoryGrow),
        INTRINSIC_F32_FLOOR => Some(Intrinsic::F32Floor),
        INTRINSIC_F64_FLOOR => Some(Intrinsic::F64Floor),
        INTRINSIC_F32_CEIL => Some(Intrinsic::F32Ceil),
        INTRINSIC_F64_CEIL => Some(Intrinsic::F64Ceil),
        INTRINSIC_DATA_END => Some(Intrinsic::DataEnd),
        INTRINSIC_TABLE_GET => Some(Intrinsic::TableGet),
        INTRINSIC_TABLE_SET => Some(Intrinsic::TableSet),
        INTRINSIC_SIZE_OF => Some(Intrinsic::SizeOf),
        INTRINSIC_ALIGN_OF => Some(Intrinsic::AlignOf),
        INTRINSIC_TRAP => Some(Intrinsic::Trap),
        instr_name => {
            ctx.errors.unknown_intrinsic(annotation.pos, instr_name);
            None
        }
    }?;

    let func_type = func.ty.as_func().unwrap();
    let is_valid = match intrinsic {
        Intrinsic::MemorySize => {
            func.typeargs.is_none()
                && func_type.return_type.is_usize()
                && func_type.params.is_empty()
        }
        Intrinsic::MemoryGrow => {
            func.typeargs.is_none()
                && func_type.return_type.is_usize()
                && func_type.params.len() == 1
                && func_type.params[0].is_usize()
        }
        Intrinsic::F32Floor => {
            func.typeargs.is_none()
                && func_type.return_type.is_f32()
                && func_type.params.len() == 1
                && func_type.params[0].is_f32()
        }
        Intrinsic::F64Floor => {
            func.typeargs.is_none()
                && func_type.return_type.is_f64()
                && func_type.params.len() == 1
                && func_type.params[0].is_f64()
        }
        Intrinsic::F32Ceil => {
            func.typeargs.is_none()
                && func_type.return_type.is_f32()
                && func_type.params.len() == 1
                && func_type.params[0].is_f32()
        }
        Intrinsic::F64Ceil => {
            func.typeargs.is_none()
                && func_type.return_type.is_f64()
                && func_type.params.len() == 1
                && func_type.params[0].is_f64()
        }
        Intrinsic::DataEnd => {
            func.typeargs.is_none()
                && func_type.return_type.is_usize()
                && func_type.params.is_empty()
        }
        Intrinsic::TableGet => {
            func.typeargs.is_none()
                && func_type.return_type.is_opaque()
                && func_type.params.len() == 1
                && func_type.params[0].is_usize()
        }
        Intrinsic::TableSet => {
            func.typeargs.is_none()
                && func_type.return_type.is_void()
                && func_type.params.len() == 2
                && func_type.params[0].is_usize()
                && func_type.params[1].is_opaque()
        }
        Intrinsic::SizeOf => {
            func.typeargs.is_some_and(|type_args| type_args.len() == 1)
                && func_type.return_type.is_usize()
                && func_type.params.is_empty()
        }
        Intrinsic::AlignOf => {
            func.typeargs.is_some_and(|type_args| type_args.len() == 1)
                && func_type.return_type.is_usize()
                && func_type.params.is_empty()
        }
        Intrinsic::Trap => {
            func.typeargs.is_none()
                && func_type.return_type.is_void()
                && func_type.params.is_empty()
        }
    };

    if !is_valid {
        ctx.errors.intrinsic_signature_mismatch(func.pos);
    }

    Some(intrinsic)
}

fn check_functions<'ctx, E: ErrorReporter>(ctx: &Context<'ctx, E>, functions: &[Function<'ctx>]) {
    check_duplicated_main(ctx, functions);
    check_imports_exports(ctx, functions);
    check_compilation_strategy(ctx, functions);
}

fn check_duplicated_main<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    functions: &[Function<'ctx>],
) {
    let mut main = None;
    for func in functions {
        if func.is_main {
            if let Some(declared_at) = main {
                ctx.errors
                    .multiple_main(func.pos, ctx.files.location(declared_at));
            } else {
                main = Some(func.pos);
            }
        }
    }
}

fn check_imports_exports<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    functions: &[Function<'ctx>],
) {
    let mut imports = HashMap::<(&str, &str), Pos>::default();
    let mut exports = HashMap::<&str, Pos>::default();

    for func in functions {
        if let Some((im_module, im_name)) = &func.import {
            if let Some(declared_at) = imports.get(&(im_module, im_name)) {
                ctx.errors.duplicated_import(
                    func.pos,
                    im_module,
                    im_name,
                    ctx.files.location(*declared_at),
                );
            } else {
                imports.insert((*im_module, *im_name), func.pos);
            }
        }

        if let Some(name) = &func.export {
            if let Some(declared_at) = exports.get(name) {
                ctx.errors
                    .duplicated_export(func.pos, name, ctx.files.location(*declared_at));
            } else {
                exports.insert(name, func.pos);
            }
        }

        if func.export.is_some() && func.import.is_some() {
            ctx.errors.func_both_imported_and_exported(func.pos);
        }
    }
}

fn check_compilation_strategy<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    functions: &[Function<'ctx>],
) {
    for func in functions {
        let is_import = func.import.is_some();
        let is_export = func.export.is_some();
        let is_intrinsic = func.intrinsic.is_some();
        let is_user = func.body.is_some();

        let is_valid = (is_import && !is_export && !is_intrinsic && !is_user)
            || (!is_import && is_export && is_intrinsic && !is_user)
            || (!is_import && is_export && !is_intrinsic && is_user)
            || (!is_import && !is_export && is_intrinsic && !is_user)
            || (!is_import && !is_export && !is_intrinsic && is_user);

        if !is_valid {
            ctx.errors.unknown_compilation_strategy(func.pos);
        }
    }
}
