use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck::analyze;
use magelang_wasmgen::generate;
use std::fs::read_to_string;
use std::path::PathBuf;
use wasm_helper::Serializer;
use wasmtime::{Engine, Instance, Linker, Module, Store, Trap};
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::WasiCtxBuilder;

macro_rules! test_success {
    ($name:ident) => {
        #[test]
        fn $name() {
            test_package(stringify!($name), |_, _| {});
        }
    };
}

test_success!(test_000);
test_success!(test_001);
test_success!(test_002);
test_success!(test_003);
test_success!(test_004);
test_success!(test_005);
test_success!(test_006);
test_success!(test_007);
test_success!(test_008);
test_success!(test_009);
test_success!(test_010);
test_success!(test_011);
test_success!(test_012);
test_success!(test_013);
test_success!(test_014);
test_success!(test_015);
test_success!(test_017);
test_success!(test_001_fail);
test_success!(test_002_fail);
test_success!(test_003_fail);
test_success!(test_004_fail);
test_success!(test_005_fail);
test_success!(test_006_fail);
test_success!(test_007_fail);
test_success!(test_008_fail);
test_success!(test_009_fail);
test_success!(test_018_fail);
test_success!(test_019_fail);
test_success!(test_020_fail);
test_success!(test_021_fail);
test_success!(test_022);
test_success!(test_023_fail);
test_success!(test_024_fail);
test_success!(test_025_fail);
test_success!(test_026_fail);
test_success!(test_027_fail);
test_success!(test_028);
test_success!(test_029_fail);
test_success!(test_030_fail);
test_success!(test_slice_ptr);
test_success!(test_slice_ptr_fail);

#[test]
fn test_slice_ptr_bounds() {
    test_package("test_slice_ptr_bounds", |store, instance| {
        for name in
            ["index_i8", "index_u8", "index_i16", "index_u16", "index_i32", "index_u32", "index_isize", "index_usize"]
        {
            let index = instance.get_typed_func::<(i32, i32), i32>(&mut *store, name).unwrap();
            assert_eq!(index.call(&mut *store, (0, 2)).unwrap(), 1024, "{name}");
            assert_eq!(index.call(&mut *store, (1, 2)).unwrap(), 1025, "{name}");
            for args in [(2, 2), (3, 2), (0, 0)] {
                let error = index.call(&mut *store, args).unwrap_err();
                assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached), "{name}: {args:?}");
            }
            if name.starts_with("index_i") {
                for args in [(-1, 2), (-1, -1)] {
                    let error = index.call(&mut *store, args).unwrap_err();
                    assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached), "{name}: {args:?}");
                }
            }
        }
        for name in ["index_u32", "index_usize"] {
            let index = instance.get_typed_func::<(i32, i32), i32>(&mut *store, name).unwrap();
            assert_eq!(index.call(&mut *store, (i32::MAX, -1)).unwrap(), i32::MAX.wrapping_add(1024));
            let error = index.call(&mut *store, (-1, -1)).unwrap_err();
            assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached), "{name}");
        }
        for name in ["index_i64", "index_u64"] {
            let index = instance.get_typed_func::<(i64, i32), i32>(&mut *store, name).unwrap();
            assert_eq!(index.call(&mut *store, (1, 2)).unwrap(), 1025);
            for args in
                [(2, 2), (0, 0), (-1, 2), (-1, -1), (1 << 32, 2), ((1 << 32) + 1, 2), (-(1 << 32), 2), (i64::MAX, 2)]
            {
                let error = index.call(&mut *store, args).unwrap_err();
                assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached), "{name}: {args:?}");
            }
        }
        let empty = instance.get_typed_func::<(), i32>(&mut *store, "empty").unwrap();
        let error = empty.call(&mut *store, ()).unwrap_err();
        assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached));
        let read = instance.get_typed_func::<i32, i32>(&mut *store, "read").unwrap();
        assert_eq!(read.call(&mut *store, 1).unwrap(), 0);
        let error = read.call(&mut *store, 2).unwrap_err();
        assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached));
        for name in ["write", "update"] {
            let write = instance.get_typed_func::<i32, ()>(&mut *store, name).unwrap();
            write.call(&mut *store, 1).unwrap();
            let error = write.call(&mut *store, 2).unwrap_err();
            assert_eq!(error.downcast_ref::<Trap>(), Some(&Trap::UnreachableCodeReached), "{name}");
        }
        assert_eq!(read.call(&mut *store, 1).unwrap(), 8);
    });
}

#[test]
fn missing_source_diagnostics_have_no_position() {
    for command in ["parse", "analyze", "compile", "run"] {
        let source =
            if command == "parse" { "tests/missing_source_diagnostic.mg" } else { "tests/missing_source_diagnostic" };
        let output = std::process::Command::new(env!("CARGO_BIN_EXE_magelang"))
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .args([command, source])
            .output()
            .unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(stderr.starts_with("Cannot open file "), "{command}: {stderr}");
        assert!(stderr.contains("missing_source_diagnostic.mg"), "{command}: {stderr}");
        assert!(!stderr.contains(":1:1:"), "{command}: {stderr}");
        if command != "analyze" {
            assert!(!output.status.success(), "{command}");
        }
    }
}

fn test_package(name: &str, check: impl FnOnce(&mut Store<WasiP1Ctx>, Instance)) {
    unsafe {
        std::env::set_var("MAGELANG_ROOT", env!("CARGO_MANIFEST_DIR"));
    }
    let package_name = format!("tests/{}/main", name);

    let expected_error_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join(name).join("expected_errors");
    let should_error = expected_error_path.exists();

    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
    if !module.is_valid {
        if should_error {
            let mut errors = String::default();
            for error in error_manager.take() {
                errors.push_str(&format!("{}\n", error.display(&file_manager)));
            }
            let expected_error = read_to_string(expected_error_path).expect("cannot read expected error");
            assert_eq!(expected_error, errors);
            return;
        } else {
            for error in error_manager.take() {
                eprintln!("{}", error.display(&file_manager));
            }
            panic!("compilation failed");
        }
    };

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        if should_error {
            let mut errors = String::default();
            for error in error_manager.take() {
                errors.push_str(&format!("{}\n", error.display(&file_manager)));
            }
            let expected_error = read_to_string(expected_error_path).expect("cannot read expected error");
            assert_eq!(expected_error, errors);
            return;
        } else {
            for error in error_manager.take() {
                eprintln!("{}", error.display(&file_manager));
            }
            panic!("codegen failed");
        }
    };

    if should_error {
        panic!("nothing fails, but it should");
    }

    let mut module = Vec::<u8>::default();
    wasm_module.serialize(&mut module).expect("cannot write wasm to target file");

    let engine = Engine::default();

    let module = Module::from_binary(&engine, &module).expect("cannot load wasm module");
    let mut linker: Linker<WasiP1Ctx> = Linker::new(&engine);
    p1::add_to_linker_sync(&mut linker, |s| s).expect("cannot link wasi to the linker");
    let wasi = WasiCtxBuilder::new().inherit_stdio().inherit_args().build_p1();
    let mut store = Store::new(&engine, wasi);
    let instance = linker.instantiate(&mut store, &module).unwrap();
    check(&mut store, instance);
}
