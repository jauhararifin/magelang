use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck::analyze;
use magelang_wasmgen::generate;
use std::fs::read_to_string;
use std::path::PathBuf;
use wasm_helper::Serializer;
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::sync::WasiCtxBuilder;

macro_rules! test_success {
    ($name:ident) => {
        #[test]
        fn $name() {
            test_package(stringify!($name));
        }
    };
}

test_success!(test_000);
test_success!(test_001);
test_success!(test_002);
test_success!(test_003);
test_success!(test_004);
test_success!(test_001_fail);
test_success!(test_002_fail);

fn test_package(name: &str) {
    unsafe {
        std::env::set_var("MAGELANG_ROOT", env!("CARGO_MANIFEST_DIR"));
    }
    let package_name = format!("tests/{}/main", name);

    let expected_error_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join(name)
        .join("expected_errors");
    let should_error = expected_error_path.exists();

    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
    if !module.is_valid {
        if should_error {
            let mut errors = String::default();
            for error in error_manager.take() {
                let location = file_manager.location(error.pos);
                let message = error.message;
                errors.push_str(&format!("{location}: {message}\n"));
            }
            let expected_error =
                read_to_string(expected_error_path).expect("cannot read expected error");
            assert_eq!(expected_error, errors);
            return;
        } else {
            for error in error_manager.take() {
                let location = file_manager.location(error.pos);
                let message = error.message;
                eprintln!("{location}: {message}");
            }
            panic!("compilation failed");
        }
    };

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        if should_error {
            let mut errors = String::default();
            for error in error_manager.take() {
                let location = file_manager.location(error.pos);
                let message = error.message;
                errors.push_str(&format!("{location}: {message}\n"));
            }
            let expected_error =
                read_to_string(expected_error_path).expect("cannot read expected error");
            assert_eq!(expected_error, errors);
            return;
        } else {
            for error in error_manager.take() {
                let location = file_manager.location(error.pos);
                let message = error.message;
                eprintln!("{location}: {message}");
            }
            panic!("codegen failed");
        }
    };

    if should_error {
        panic!("nothing fails, but it should");
    }

    let mut module = Vec::<u8>::default();
    wasm_module
        .serialize(&mut module)
        .expect("cannot write wasm to target file");

    let engine = Engine::default();

    let module = Module::from_binary(&engine, &module).expect("cannot load wasm module");
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(&mut linker, |s| s).expect("cannot link wasi to the linker");
    let wasi = WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_args()
        .expect("cannot build wasi context")
        .build();
    let mut store = Store::new(&engine, wasi);
    linker.instantiate(&mut store, &module).unwrap();
}
