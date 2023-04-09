use magelang_common::{ErrorAccumulator, FileLoader, SymbolLoader};
use magelang_compiler::Compiler;
use magelang_package::PackageUtil;
use magelang_semantic::TypeLoader;
use magelang_syntax::AstLoader;
use magelang_typecheck::TypeChecker;
use std::fs::File;

fn main() {
    let err_accumulator = ErrorAccumulator::new();
    let symbol_loader = SymbolLoader::new();
    let file_loader = FileLoader::new(&err_accumulator);
    let type_loader = TypeLoader::new();
    let ast_loader = AstLoader::new(&err_accumulator, &file_loader, &symbol_loader);
    let package_util = PackageUtil::new(&file_loader, &ast_loader, &symbol_loader);
    let type_checker = TypeChecker::new(
        &err_accumulator,
        &symbol_loader,
        &file_loader,
        &ast_loader,
        &package_util,
        &type_loader,
    );

    let main_package = symbol_loader.declare_symbol("examples/a");
    let packages = type_checker.check_all(main_package);

    println!("Errors:");
    let mut has_error = false;
    for err in err_accumulator.take() {
        has_error = true;
        if let Some(span) = &err.span {
            let file_info = file_loader.get_file(span.file_id).unwrap();
            let pos = file_info.get_pos(&span);
            println!("{}: {}", pos, err.message);
        } else {
            println!("{}", err.message);
        }
    }

    if has_error {
        return;
    }

    let mut output_file = File::create("result.wasm").expect("cannot open file");

    let compiler = Compiler::new(&symbol_loader, &type_loader);
    compiler.compile(packages, main_package, &output_file);
}
