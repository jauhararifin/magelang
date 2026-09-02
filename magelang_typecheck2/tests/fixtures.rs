//! Runs `analyze` over the packages in `tests/fixtures/`.
//!
//! A fixture with an `expected_errors` file must fail with exactly those diagnostics. A
//! fixture without one must check cleanly; if it also has an `expected_instances` file,
//! the concrete instances of generic functions in the produced module must match it.
//! (Running the produced code needs the code generator; that is covered by the CLI crate's
//! tests once this crate is integrated.)

use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck2::{analyze, Module};
use std::fs::read_to_string;
use std::path::{Path, PathBuf};
use std::sync::Once;

macro_rules! fixture {
    ($name:ident) => {
        #[test]
        fn $name() {
            check_fixture(stringify!($name));
        }
    };
}

fixture!(generics_roundtrip);
fixture!(linked_list);
fixture!(nominal_structs_fail);
fixture!(infinite_type_fail);
fixture!(instantiation_depth_fail);
fixture!(definition_check_fail);

fn check_fixture(name: &str) {
    let crate_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    point_at_stdlib(&crate_root);
    let fixture_dir = crate_root.join("tests").join("fixtures").join(name);
    let package_name = format!("tests/fixtures/{name}/main");

    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);

    let mut errors = String::new();
    for error in error_manager.take() {
        let location = file_manager.location(error.pos);
        errors.push_str(&format!("{location}: {}\n", error.message));
    }

    if let Ok(expected) = read_to_string(fixture_dir.join("expected_errors")) {
        assert!(!module.is_valid, "{name}: expected errors, but the module is valid");
        assert_eq!(expected, errors, "{name}: unexpected diagnostics");
        return;
    }

    assert!(module.is_valid, "{name}: unexpected diagnostics:\n{errors}");
    assert!(errors.is_empty());

    let actual = instances(&module);
    match read_to_string(fixture_dir.join("expected_instances")) {
        Ok(expected) => assert_eq!(expected, actual, "{name}: unexpected instances:\n{actual}"),
        Err(..) => eprintln!("{name}: no expected_instances file; the module contains:\n{actual}"),
    }
}

/// Every concrete instance of a generic function in the module, one per line, sorted.
fn instances(module: &Module<'_>) -> String {
    let mut lines = Vec::new();
    for package in &module.packages {
        for func in &package.functions {
            let Some(typeargs) = func.typeargs else {
                continue;
            };
            let args = typeargs
                .iter()
                .map(|ty| ty.to_string())
                .collect::<Vec<_>>()
                .join(",");
            lines.push(format!("{}<{args}>\n", func.name));
        }
    }
    lines.sort();
    lines.concat()
}

/// The fixtures import `std/*`, which lives in the CLI crate; `analyze` finds it through
/// `MAGELANG_ROOT`. Setting an environment variable is unsafe because other threads may
/// be reading the environment; `Once` makes every test wait until the single write is done
/// before it runs `analyze`, and nothing else in this process reads the environment.
fn point_at_stdlib(crate_root: &Path) {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        // SAFETY: see above.
        unsafe { std::env::set_var("MAGELANG_ROOT", crate_root.join("..").join("magelang")) };
    });
}
