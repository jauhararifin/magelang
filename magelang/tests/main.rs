use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck::analyze;
use magelang_wasmgen::generate;
use wasm_helper::Serializer;
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::sync::WasiCtxBuilder;

macro_rules! test_success {
    ($name:ident) => {
        #[test]
        fn $name() {
            // SAFETY: there is no other thread calling set_var
            unsafe {
                std::env::set_var("MAGELANG_ROOT", env!("CARGO_MANIFEST_DIR"));
            }
            let package_name = format!("tests/{}/main", stringify!($name));

            let mut error_manager = ErrorManager::default();
            let mut file_manager = FileManager::default();

            let arena = Bump::default();
            let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
            if !module.is_valid {
                for error in error_manager.take() {
                    let location = file_manager.location(error.pos);
                    let message = error.message;
                    eprintln!("{location}: {message}");
                }
                panic!("compilation failed");
            };

            let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
                for error in error_manager.take() {
                    let location = file_manager.location(error.pos);
                    let message = error.message;
                    eprintln!("{location}: {message}");
                }
                panic!("codegen failed");
            };

            let mut module = Vec::<u8>::default();
            wasm_module
                .serialize(&mut module)
                .expect("cannot write wasm to target file");

            let engine = Engine::default();

            let module = Module::from_binary(&engine, &module).expect("cannot load wasm module");
            let mut linker = Linker::new(&engine);
            wasmtime_wasi::add_to_linker(&mut linker, |s| s)
                .expect("cannot link wasi to the linker");
            let wasi = WasiCtxBuilder::new()
                .inherit_stdio()
                .inherit_args()
                .expect("cannot build wasi context")
                .build();
            let mut store = Store::new(&engine, wasi);
            linker.instantiate(&mut store, &module).unwrap();
        }
    };
}

test_success!(test_000);
