use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, InstBuilder, StackSlotData, StackSlotKind};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{BinOp, Expr, ExprKind, Package, Statement, Type, TypeDisplay, TypeLoader, UnOp};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

pub struct Compiler<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

impl<'sym, 'typ> Compiler<'sym, 'typ> {
    pub fn new(symbol_loader: &'sym SymbolLoader, type_loader: &'typ TypeLoader) -> Self {
        Self {
            symbol_loader,
            type_loader,
        }
    }

    pub fn compile(&self, packages: Vec<Rc<Package>>, main_package: SymbolId) {
        for pkg in &packages {
            self.compile_package(pkg);
        }
    }

    fn compile_package(&self, pkg: &Package) {
        let target = target_lexicon::Triple::host();
        let isa_builder = cranelift_codegen::isa::lookup(target.clone()).unwrap();
        let flag_builder = cranelift_codegen::settings::builder();
        let flags = cranelift_codegen::settings::Flags::new(flag_builder);
        let isa = isa_builder.finish(flags).unwrap();
        let isa: Arc<(dyn cranelift_codegen::isa::TargetIsa + 'static)> = Arc::from(isa);
        let libcall_names = cranelift_module::default_libcall_names();

        let pkg_name = self.symbol_loader.get_symbol(pkg.name).unwrap();
        let object_builder = ObjectBuilder::new(isa, pkg_name.as_ref(), libcall_names).unwrap();
        let mut object_module = cranelift_object::ObjectModule::new(object_builder);

        for func in &pkg.functions {
            // make function signature
            let mut sig = object_module.make_signature();
            if let Some(ret_type) = func.func_type.return_type {
                let ret_type = self.type_loader.get_type(ret_type).unwrap();
                let ret_type = to_cl_type(&target, &ret_type);
                sig.returns.push(AbiParam::new(ret_type));
            }
            for param_ty in &func.func_type.parameters {
                let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                let param_ty = to_cl_type(&target, &param_ty);
                sig.params.push(AbiParam::new(param_ty));
            }

            // prepare builder
            let mut ctx = object_module.make_context();
            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            // declare locals in func preamble
            let mut locals = vec![];
            for local in &func.locals {
                let ty = self.type_loader.get_type(*local).unwrap();
                let ty = to_cl_type(&target, &ty);
                let var = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.bytes()));
                locals.push(var);
            }

            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            builder.seal_block(block);
            for i in 0..func.func_type.parameters.len() {
                let param_var = builder.block_params(block)[i];
                builder.ins().stack_store(param_var, locals[i], 0);
                builder.ins().return_(&[]);
            }

            builder.ins().return_(&[]);
            builder.finalize();

            let func_name = self.symbol_loader.get_symbol(func.function_name).unwrap();
            let mangled_name = mangle_func(&pkg_name, &func_name);
            let func_id = object_module
                .declare_function(&mangled_name, cranelift_module::Linkage::Local, &sig)
                .unwrap();
            println!("{mangled_name}");
            println!("{}", ctx.func.display());
            object_module.define_function(func_id, &mut ctx).unwrap();
            object_module.clear_context(&mut ctx);
        }

        let object = object_module.finish();
        let object = object.object;

        let filename = get_object_file_path(&pkg_name);
        let filedir = filename.parent().unwrap();
        std::fs::create_dir_all(filedir).unwrap();
        let file = std::fs::File::create(filename).unwrap();
        object.write_stream(&file).unwrap();
    }
}

fn get_object_file_path(pkg_name: &str) -> PathBuf {
    let mut result = PathBuf::new();
    result.push(".cache/");
    for seg in pkg_name.split("/") {
        result.push(seg);
    }
    result
}

fn mangle_func(pkg_name: &str, func_name: &str) -> String {
    format!("{}.{}", pkg_name, func_name)
}

fn to_cl_type(triple: &target_lexicon::Triple, ty: &Type) -> types::Type {
    match ty {
        Type::I64 | Type::U64 => types::I64,
        Type::I32 | Type::U32 => types::I32,
        Type::I16 | Type::U16 => types::I16,
        Type::I8 | Type::U8 => types::I8,
        Type::F64 => types::F64,
        Type::F32 => types::F32,
        Type::ArrayPtr(_) => types::Type::triple_pointer_type(triple),
        _ => todo!(),
    }
}
