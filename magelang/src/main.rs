use clap::{Args, Parser, Subcommand};
use magelang_common::{ErrorAccumulator, FileLoader, SymbolLoader};
use magelang_package::PackageUtil;
use magelang_semantic::TypeLoader;
use magelang_syntax::AstLoader;
use magelang_typecheck::TypeChecker;
use magelang_wasm::Compiler;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct CliArgs {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Compile(CompileArgs),
}

#[derive(Args, Debug)]
struct CompileArgs {
    package_name: String,

    #[arg(short = 'o', long = "output")]
    output_file: Option<String>,
}

fn main() {
    let args = CliArgs::parse();

    let err_accumulator = ErrorAccumulator::default();
    let symbol_loader = SymbolLoader::default();
    let file_loader = FileLoader::new(&err_accumulator);
    let type_loader = TypeLoader::default();
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

    match args.command {
        Command::Compile(arg) => {
            let main_package = symbol_loader.declare_symbol(&arg.package_name);
            let packages = type_checker.check_all(main_package);

            let mut has_error = false;
            for err in err_accumulator.take() {
                has_error = true;
                if let Some(span) = &err.span {
                    let file_info = file_loader.get_file(span.file_id).unwrap();
                    let pos = file_info.get_pos(span);
                    eprintln!("{}: {}", pos, err.message);
                } else {
                    eprintln!("{}", err.message);
                }
            }

            if has_error {
                eprintln!("Compilation failed due to some error(s)");
                std::process::exit(1);
            }

            let mut path = PathBuf::from(&arg.package_name);
            path.set_extension("wasm");
            let filename = arg
                .output_file
                .as_deref()
                .or_else(|| path.file_name().and_then(|v| v.to_str()))
                .unwrap_or("main.wasm");
            let compiler = Compiler::new(&symbol_loader, &type_loader);
            compiler
                .compile(packages, main_package, filename)
                .expect("cannot compile package");
        }
    }
}
