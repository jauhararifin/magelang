#![cfg(all(any(target_os = "linux", target_os = "macos"), any(target_arch = "aarch64", target_arch = "x86_64")))]

const BACKEND: &str = "llvm";
include!("common/native.rs");

#[test]
fn llvm_ir_can_be_emitted_without_a_toolchain() {
    let directory = source_directory(include_str!("native/core.mg"));
    success(
        compiler(directory.path())
            .arg("--emit-llvm")
            .env("LLVM_CLANG", "/no/clang/needed")
            .env("CC", "/no/linker/needed")
            .output()
            .unwrap(),
    );
    let ir = std::fs::read_to_string(directory.path().join("a.ll")).unwrap();
    assert!(ir.contains("define i32 @main()"));
    assert!(ir.contains("@llvm.trap()"));
    assert!(ir.contains("@\"puts\""));
    assert!(!directory.path().join("a.o").exists());
    assert!(!directory.path().join("a.out").exists());
    success(
        Command::new(std::env::var_os("LLVM_CLANG").unwrap_or_else(|| "clang".into()))
            .current_dir(directory.path())
            .args(["-x", "ir", "-c", "a.ll", "-o", "verified.o"])
            .output()
            .unwrap(),
    );
    success(
        Command::new(std::env::var_os("CC").unwrap_or_else(|| "cc".into()))
            .current_dir(directory.path())
            .args(["verified.o", "-o", "verified", "-lm"])
            .output()
            .unwrap(),
    );
    let output = success(Command::new(directory.path().join("verified")).output().unwrap());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "native core ok\n");
}

#[test]
fn llvm_tool_and_option_errors() {
    let directory = source_directory("@main() fn main() {}");
    std::fs::write(directory.path().join("a.out"), "existing").unwrap();
    let output = compiler(directory.path()).env("LLVM_CLANG", directory.path().join("missing-clang")).output().unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8(output.stderr).unwrap().contains("starting LLVM compiler"));
    assert_eq!(std::fs::read_to_string(directory.path().join("a.out")).unwrap(), "existing");
    let failing_clang = directory.path().join("failing-clang");
    std::fs::write(&failing_clang, "#!/bin/sh\necho 'LLVM failure detail' >&2\nexit 2\n").unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&failing_clang, std::fs::Permissions::from_mode(0o755)).unwrap();
    let output = compiler(directory.path()).env("LLVM_CLANG", failing_clang).output().unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("LLVM compiler") && stderr.contains("LLVM failure detail"), "{stderr}");
    assert_eq!(std::fs::read_to_string(directory.path().join("a.out")).unwrap(), "existing");
    for args in [
        vec!["compile", "main", "--backend", "llvm"],
        vec!["compile", "main", "--target", "native", "--emit-llvm"],
        vec!["compile", "main", "--target", "native", "--backend", "llvm", "--emit-llvm", "--emit-object"],
    ] {
        let output =
            Command::new(env!("CARGO_BIN_EXE_magelang")).current_dir(directory.path()).args(args).output().unwrap();
        assert!(!output.status.success());
        assert!(!String::from_utf8(output.stderr).unwrap().contains("panicked"));
    }
    let source = r#"@main() fn main() {} @native_import("llvm.trap") fn imported();"#;
    let directory = source_directory(source);
    let output = compiler(directory.path()).arg("--emit-llvm").output().unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8(output.stderr).unwrap().contains("reserved native symbol"));
}

#[test]
fn llvm_arithmetic_edge_cases() {
    let directory = source_directory(include_str!("native/llvm_arithmetic.mg"));
    for optimize in [false, true] {
        let mut command = compiler(directory.path());
        if !optimize {
            command.arg("-n");
        }
        success(command.output().unwrap());
        success(Command::new(directory.path().join("a.out")).current_dir(directory.path()).output().unwrap());
    }
}

#[test]
fn llvm_arithmetic_traps_survive_optimization() {
    use std::os::unix::process::ExitStatusExt;
    for operation in [
        "let x: i64 = 7; let y: i64 = 0; x / y;",
        "let x: u32 = 7; let y: u32 = 0; x % y;",
        "let x: i64 = -9223372036854775808; x / -1;",
        "let x: i32 = -2147483648; x / -1;",
        "let zero: f64 = 0.0; (zero / zero) as i64;",
        "let zero: f64 = 0.0; (zero / zero) as *u8;",
        "let zero: f64 = 0.0; (1.0 / zero) as u64;",
        "let x: f64 = 9223372036854775808.0; x as i64;",
        "let x: f64 = 18446744073709551616.0; x as u64;",
        "let x: f64 = -2147483649.0; x as i32;",
        "let x: f32 = -1.0; x as u32;",
    ] {
        let directory = source_directory(&format!("@main() fn main() {{ {operation} }}"));
        for optimize in [false, true] {
            let mut command = compiler(directory.path());
            if !optimize {
                command.arg("-n");
            }
            success(command.output().unwrap());
            let output = Command::new(directory.path().join("a.out")).current_dir(directory.path()).output().unwrap();
            assert!(matches!(output.status.signal(), Some(4 | 5)), "{operation}: {}", output.status);
        }
    }
}
