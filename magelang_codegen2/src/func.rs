use crate::data::Data;
use crate::layout::LayoutManager;
use crate::ty::build_val_type;
use crate::var::LocalManager;
use indexmap::IndexMap;
use magelang_typecheck::{DefId, Func, InternTypeArgs, Module, Statement, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use wasm_helper as wasm;

pub(crate) struct FuncManager<'a, 'ctx> {
    data: &'a Data,
    layouts: &'a LayoutManager<'ctx>,
    locals: &'a LocalManager,

    func_type_cache: RefCell<IndexMap<wasm::FuncType, wasm::TypeIdx>>,

    func_map: HashMap<FuncRef<'ctx>, wasm::FuncIdx>,
    next_func_id: wasm::FuncIdx,
    functions: Vec<wasm::Func>,
    exports: Vec<wasm::Export>,
    imports: Vec<wasm::Import>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct FuncRef<'ctx> {
    name: DefId<'ctx>,
    typeargs: Option<InternTypeArgs<'ctx>>,
}

impl<'ctx> From<&Func<'ctx>> for FuncRef<'ctx> {
    fn from(value: &Func<'ctx>) -> Self {
        Self {
            name: value.name,
            typeargs: value.typeargs,
        }
    }
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
    Unreachable,
}

impl<'a, 'ctx> FuncManager<'a, 'ctx> {
    pub(crate) fn build(
        data: &'a Data,
        layouts: &'a LayoutManager<'ctx>,
        locals: &'a LocalManager,
        module: &Module<'ctx>,
    ) -> Self {
        let mut s = FuncManager {
            data,
            layouts,
            locals,

            func_type_cache: RefCell::default(),

            func_map: HashMap::default(),
            next_func_id: 0,
            functions: Vec::default(),
            exports: Vec::default(),
            imports: Vec::default(),
        };

        struct MappedFunction<'a, 'ctx> {
            id: FuncRef<'ctx>,
            name: String,
            index: usize,
            is_native: bool,
            type_id: wasm::TypeIdx,

            func: &'a Func<'ctx>,

            import: Option<(&'a str, &'a str)>,
            export: Option<&'a str>,
            intrinsic: Option<Intrinsic>,
        }

        let mut main_func: Option<FuncRef<'ctx>> = None;
        let mut mapped_functions = Vec::default();

        let funcs = module.packages.iter().flat_map(|pkg| &pkg.functions);
        for (index, func) in funcs.enumerate() {
            let func_type = func.ty.as_func().expect("func type is not a function");

            let mut parameters = Vec::default();
            for param in &func_type.params {
                let val_types = build_val_type(*param);
                parameters.extend(val_types);
            }

            let returns = build_val_type(func_type.return_type);
            let func_wasm_type_id = s.get_func_type(wasm::FuncType {
                parameters,
                returns,
            });

            let func_name = func.get_mangled_name();
            let is_native = matches!(func.statement.as_ref(), Statement::Native);

            let mut import = None;
            let mut export = None;
            let mut wasm_instr = None;

            // initialize function kinds, is it import, export, wasm intrinsic, user defined?
            for annotation in func.annotations.iter() {
                match annotation.name.as_str() {
                    "wasm_import" => {
                        if import.is_some() {
                            todo!("found duplicate wasm_import annotation");
                        }
                        if annotation.arguments.len() != 2 {
                            todo!(
                                "expected to have 2 arguments in wasm_import, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(import_module) = annotation.arguments.get(0) else {
                            todo!("missing module name in the wasm_import annotation");
                        };
                        let Some(import_name) = annotation.arguments.get(1) else {
                            todo!("missing import name in the wasm_import annotation");
                        };
                        import = Some((import_module.as_str(), import_name.as_str()));
                    }
                    "wasm_export" => {
                        if export.is_some() {
                            todo!("found duplicate wasm_export annotation");
                        }
                        if annotation.arguments.len() != 1 {
                            todo!(
                                "expected to have 1 argument in wasm_export, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(export_name) = annotation.arguments.get(0) else {
                            todo!("missing export name in the wasm_export annotation");
                        };
                        export = Some(export_name.as_str());
                    }
                    "intrinsic" => {
                        if wasm_instr.is_some() {
                            todo!("found duplicate wasm_instr annotation");
                        }
                        if annotation.arguments.len() != 1 {
                            todo!(
                                "expected to have 1 argument in wasm_instr, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(name) = annotation.arguments.get(0) else {
                            todo!("missing wasm instr in the wasm_instr annotation");
                        };
                        // TODO: check function signature. They should match the wasm instructions.
                        let instr = match name.as_str() {
                            "memory.size" => Intrinsic::MemorySize,
                            "memory.grow" => Intrinsic::MemoryGrow,
                            "f32.floor" => Intrinsic::F32Floor,
                            "f64.floor" => Intrinsic::F64Floor,
                            "f32.ceil" => Intrinsic::F32Ceil,
                            "f64.ceil" => Intrinsic::F64Ceil,
                            "data.end" => Intrinsic::DataEnd,
                            "table.get" => Intrinsic::TableGet,
                            "table.set" => Intrinsic::TableSet,
                            "size_of" => Intrinsic::SizeOf,
                            "align_of" => Intrinsic::AlignOf,
                            "unreachable" => Intrinsic::Unreachable,
                            instr_name => todo!("unknwon wasm instr {instr_name}"),
                        };
                        wasm_instr = Some(instr);
                    }
                    "main" => {
                        if main_func.is_some() {
                            todo!("found duplicate main annotation");
                        }
                        if !annotation.arguments.is_empty() {
                            todo!(
                                "expected to have no argument in main annotation, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        main_func = Some(func.into());
                    }
                    name => todo!("found unknown annotation {name}"),
                }
            }

            mapped_functions.push(MappedFunction {
                id: func.into(),
                name: func_name,
                index,
                is_native,
                func,
                type_id: func_wasm_type_id,
                import,
                export,
                intrinsic: wasm_instr,
            });
        }

        // it is important that the imported function appear first due to wasm spec
        mapped_functions.sort_by(|a, b| match (a.import, b.import) {
            (Some(..), Some(..)) | (None, None) => std::cmp::Ordering::Equal,
            (Some(..), None) => std::cmp::Ordering::Less,
            (None, Some(..)) => std::cmp::Ordering::Greater,
        });

        let mut func_with_bodies = Vec::default();

        // build native functions
        for mapped_func in mapped_functions.into_iter() {
            let MappedFunction {
                id,
                name,
                index: _,
                is_native,
                func,
                type_id,
                import,
                export,
                intrinsic: wasm_instr,
            } = mapped_func;
            match (import, export, wasm_instr) {
                (None, None, None) => {
                    if is_native {
                        todo!("cannot compile native function");
                    }

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;
                    func_with_bodies.push((id, (s.functions.len(), func)));

                    s.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(vec![]),
                    });
                }
                (None, None, Some(native_wasm)) => {
                    if !is_native {
                        todo!("function with native wasm should be a native func");
                    }

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;

                    s.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(match native_wasm {
                            Intrinsic::MemorySize => vec![wasm::Instr::MemorySize],
                            Intrinsic::MemoryGrow => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::MemoryGrow]
                            }
                            Intrinsic::F32Floor => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F32Floor]
                            }
                            Intrinsic::F64Floor => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F64Floor]
                            }
                            Intrinsic::F32Ceil => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F32Ceil]
                            }
                            Intrinsic::F64Ceil => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F64Ceil]
                            }
                            Intrinsic::Unreachable => vec![wasm::Instr::Unreachable],
                            Intrinsic::DataEnd => {
                                vec![wasm::Instr::I32Const(s.data.data_end() as i32)]
                            }
                            Intrinsic::TableGet => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::TableGet(1)]
                            }
                            Intrinsic::TableSet => {
                                vec![
                                    wasm::Instr::LocalGet(0),
                                    wasm::Instr::LocalGet(1),
                                    wasm::Instr::TableSet(1),
                                ]
                            }
                            Intrinsic::SizeOf => {
                                let Some(typeargs) = func.typeargs else {
                                    todo!("size_of requires a generic func");
                                };

                                if typeargs.len() != 1 {
                                    todo!(
                                        "size_of expects 1 generic parameter but found {}",
                                        typeargs.len()
                                    );
                                }
                                let ty = typeargs[0];
                                let layout = s.layouts.get_mem_layout(ty);
                                vec![wasm::Instr::I32Const(layout.size as i32)]
                            }
                            Intrinsic::AlignOf => {
                                let Some(typeargs) = func.typeargs else {
                                    todo!("align_of requires a generic func");
                                };

                                if typeargs.len() != 1 {
                                    todo!(
                                        "align_of expects 1 generic parameter but found {}",
                                        typeargs.len()
                                    );
                                }
                                let ty = typeargs[0];
                                let layout = s.layouts.get_mem_layout(ty);
                                vec![wasm::Instr::I32Const(layout.align as i32)]
                            }
                        }),
                    });
                }
                (None, Some(export_name), None) => {
                    if is_native {
                        todo!("cannot compile native function");
                    }

                    let func_id = s.next_func_id;
                    s.func_map.insert(id, func_id);
                    s.next_func_id += 1;
                    func_with_bodies.push((id, (s.functions.len(), func)));

                    s.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(vec![]),
                    });
                    s.exports.push(wasm::Export {
                        name: export_name.to_string(),
                        desc: wasm::ExportDesc::Func(func_id),
                    });
                }
                (None, Some(..), Some(..)) => todo!("cannot export wasm instrinsic"),
                (Some((import_module, import_name)), None, None) => {
                    if !is_native {
                        todo!("cannot import function to non native function");
                    }

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;

                    s.imports.push(wasm::Import {
                        module: import_module.to_string(),
                        name: import_name.to_string(),
                        desc: wasm::ImportDesc::Func(type_id),
                    });
                }
                (Some(..), None, Some(..)) => todo!("cannot import wasm instrinsic"),
                (Some(..), Some(..), None) => todo!("cannot both import and export function"),
                (Some(..), Some(..), Some(..)) => todo!("cannot compile function"),
            }
        }

        // build user defined function body
        for (_, (wasm_idx, func)) in func_with_bodies {
            let func_type = func.ty.as_func().expect("not a func type");

            let mut locals = Vec::default();
            for ty in &func_type.params {
                let val_types = build_val_type(*ty);
                locals.push(
                    val_types
                        .iter()
                        .map(|ty| wasm::Local {
                            name: "".to_string(),
                            ty: *ty,
                        })
                        .collect(),
                );
            }
            s.locals.set_params(locals.into_iter());

            let body = s.build_statement(0, 0, &func.statement);

            s.functions[wasm_idx].body = wasm::Expr(body);
            s.functions[wasm_idx].locals = s.locals.take();
        }

        s
    }

    fn get_func_type(&self, func_type: wasm::FuncType) -> wasm::TypeIdx {
        let mut func_type_cache = self.func_type_cache.borrow_mut();
        let next_id = func_type_cache.len() as wasm::TypeIdx;
        *func_type_cache.entry(func_type).or_insert(next_id)
    }

    fn build_statement(
        &self,
        continue_label: u32,
        break_label: u32,
        stmt: &Statement,
    ) -> Vec<wasm::Instr> {
        todo!();
    }
}

trait Mangle {
    fn get_mangled_name(&self) -> String;
}

impl<'ctx> Mangle for DefId<'ctx> {
    fn get_mangled_name(&self) -> String {
        format!("{}", self)
    }
}

impl<'ctx> Mangle for Func<'ctx> {
    fn get_mangled_name(&self) -> String {
        let mut result = format!("{}", self.name);

        if let Some(ref typeargs) = self.typeargs {
            result.push('<');
            for (i, arg) in typeargs.iter().enumerate() {
                if i > 0 {
                    result.push(',');
                }
                result.push_str(&arg.get_mangled_name());
            }
            result.push('>');
        }

        result
    }
}

impl<'ctx> Mangle for Type<'ctx> {
    fn get_mangled_name(&self) -> String {
        match self {
            Type::Unknown => unreachable!("found unknown type arg"),
            Type::Struct(struct_type) => struct_type.def_id.get_mangled_name(),
            Type::Inst(inst_type) => {
                let mut result = format!("{}", inst_type.def_id);

                result.push('<');
                for (i, arg) in inst_type.type_args.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&arg.get_mangled_name());
                }
                result.push('>');

                result
            }
            Type::Func(..)
            | Type::Void
            | Type::Opaque
            | Type::Bool
            | Type::Int(..)
            | Type::Float(..)
            | Type::Ptr(..)
            | Type::ArrayPtr(..) => format!("{}", self),
            Type::TypeArg(..) => unreachable!("found typearg in type"),
        }
    }
}
