use magelang_common::{ErrorAccumulator, FileLoader, SymbolLoader};
use magelang_package::PackageUtil;
use magelang_semantic::{TypeLoader, TypePrinter};
use magelang_syntax::AstLoader;
use magelang_typecheck::TypeChecker;
use magelang_wasm::Compiler;
use std::path::PathBuf;
use wasi_common::pipe::{ReadPipe, WritePipe};
use wasmtime::*;
use wasmtime_wasi::sync::WasiCtxBuilder;

fn test_with_stdin_and_stdout(source_code: &str, stdin: &str, expected_stdout: &str) {
    let mut manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR")
        .expect("cannot get crate dir")
        .into();
    manifest_dir.pop();
    std::env::set_var("MAGELANG_ROOT", &manifest_dir);

    let err_accumulator = ErrorAccumulator::default();
    let symbol_loader = SymbolLoader::default();
    let file_loader = FileLoader::new(&err_accumulator);
    let type_loader = TypeLoader::default();
    let type_printer = TypePrinter::new(&symbol_loader, &type_loader);
    let ast_loader = AstLoader::new(&err_accumulator, &file_loader);
    let package_util = PackageUtil::new(&file_loader, &ast_loader, &symbol_loader);
    let type_checker = TypeChecker::new(
        &err_accumulator,
        &symbol_loader,
        &file_loader,
        &ast_loader,
        &package_util,
        &type_loader,
        &type_printer,
    );

    let mut source_code_path = std::env::current_dir().expect("cannot get current dir");
    source_code_path.push("testing/");
    source_code_path.push("main.mag");
    file_loader.declare_file_with_source(source_code_path.into(), source_code.into());
    let main_package = symbol_loader.declare_symbol("testing/main");
    let packages = type_checker.check_all(main_package);

    let mut has_error = false;
    for err in err_accumulator.take() {
        has_error = true;
        if let Some(pos) = &err.pos {
            let file_info = file_loader.get_file(pos.file_id).unwrap();
            let pos = file_info.get_pos(pos);
            eprintln!("{}: {}", pos, err.message);
        } else {
            eprintln!("{}", err.message);
        }
    }

    if has_error {
        panic!("Compilation failed due to some error(s)");
    }

    let compiler = Compiler::new(&symbol_loader, &type_loader, &type_printer);
    let module_bytes = compiler
        .compile(packages, main_package)
        .expect("cannot compile package");

    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(&mut linker, |s| s).expect("cannot add wasi to linker");

    let stdout = WritePipe::new_in_memory();
    let wasi = WasiCtxBuilder::new()
        .stdin(Box::new(ReadPipe::from(stdin)))
        .stdout(Box::new(stdout.clone()))
        .inherit_env()
        .expect("cannot build wasi context")
        .inherit_args()
        .expect("cannot build wasi context")
        .build();
    let mut store = Store::new(&engine, wasi);

    let module = Module::new(&engine, module_bytes).expect("cannot load module");
    linker
        .module(&mut store, "some_module_name", &module)
        .expect("cannot set module");
    drop(store);

    let actual_output = stdout.try_into_inner().unwrap().into_inner();
    let actual_output = String::from_utf8(actual_output).unwrap();
    assert_eq!(&actual_output, expected_stdout);
}

macro_rules! test_stdin_stdout {
    ($name:ident, $source_code:ident, $stdin:ident, $stdout:ident) => {
        #[test]
        fn $name() {
            test_with_stdin_and_stdout($source_code, $stdin, $stdout);
        }
    };
}

const INTEGER_CAST_SOURCE: &str = r#"
import fmt "std/fmt";
fn main() {
    let x: i8 = 0;
    let y = x as i64;
    fmt.print_i64(y);
}
"#;
const INTEGER_CAST_STDIN: &str = "";
const INTEGER_CAST_STDOUT: &str = "0";

test_stdin_stdout!(
    integer_casting,
    INTEGER_CAST_SOURCE,
    INTEGER_CAST_STDIN,
    INTEGER_CAST_STDOUT
);

const TRIANGLE_SOURCE: &str = r#"
import fmt "std/fmt";
import mem "std/mem";
import wasi "std/wasi";

fn main() {
  let n = fmt.read_i64();

  let i = 0;
  while i < n {
    let j = 0;
    while j < n-i-1 {
      wasi.fd_write(1, " " as i32, 1, 0);
      j = j + 1;
    }
    let j = 0;
    while j < (i*2+1) {
      wasi.fd_write(1, "*" as i32, 1, 0);
      j = j + 1;
    }
    wasi.fd_write(1, "\n" as i32, 1, 0);
    i = i + 1;
  }
}

"#;
const TRIANGLE_STDIN: &str = "10\n";
const TRIANGLE_STDOUT: &str = "         *
        ***
       *****
      *******
     *********
    ***********
   *************
  ***************
 *****************
*******************
";

test_stdin_stdout!(
    print_triangle,
    TRIANGLE_SOURCE,
    TRIANGLE_STDIN,
    TRIANGLE_STDOUT
);
