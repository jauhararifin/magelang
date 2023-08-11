use crate::program::ProgramBuilder;
use crate::ir::Module;
use magelang_semantic::{Db, PackageId};

mod ir;
mod program;

pub fn build_wasm_ir(db: &Db, main_package: PackageId) -> Module {
    let mut builder = ProgramBuilder::new(db, main_package);
    builder.build_module();
    let module = builder.take_module();
    module
}
