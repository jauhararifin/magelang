use std::fs;
use std::process::{Command, Output};

fn compile(directory: &std::path::Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_magelang"))
        .current_dir(directory)
        .args(["compile", "main"])
        .args(args)
        .output()
        .unwrap()
}

#[test]
fn assembly_output_and_backend_specific_defaults() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.mg"), "@main() fn main() { let a: i64 = 20; let b: i64 = 22; a + b; }").unwrap();
    let output = compile(dir.path(), &["--backend", "naive", "--emit-asm"]);
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let assembly = fs::read_to_string(dir.path().join("a.s")).unwrap();
    assert!(assembly.contains(".globl _main"));
    assert!(assembly.contains("add x9, x9, x10"));

    let output = compile(dir.path(), &["-n"]);
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(&fs::read(dir.path().join("a.wasm")).unwrap()[..4], b"\0asm");
}

#[test]
fn sdl_example_emits_assembly_without_linking_sdl() {
    let dir = tempfile::tempdir().unwrap();
    let output_path = dir.path().join("hello-sdl.s");
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_magelang"))
        .current_dir(root)
        .args(["compile", "examples/hello-sdl/main", "--backend", "naive", "--emit-asm", "-o"])
        .arg(&output_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let assembly = fs::read_to_string(output_path).unwrap();
    assert!(assembly.contains("bl _SDL_GL_CreateContext"));
    assert!(assembly.contains("bl _glDrawArrays"));
}

#[test]
fn invalid_backend_options_and_codegen_errors_fail_cleanly() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.mg"), "@wasm_import(\"env\", \"foo\") fn foo();").unwrap();
    for (args, message) in [
        (vec!["--emit-asm"], "--emit-asm requires --backend naive"),
        (vec!["--backend", "naive", "--emit-asm"], "Unsupported annotation @wasm_import"),
    ] {
        let output = compile(dir.path(), &args);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains(message));
    }
    assert!(!dir.path().join("a.s").exists());
    assert!(!dir.path().join("a.out").exists());
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn links_and_runs_native_executables() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.mg"), include_str!("../../examples/native_hello.mg")).unwrap();
    for args in [vec!["--backend", "naive"], vec!["--backend", "naive", "-n", "-o", "hello native"]] {
        let output = compile(dir.path(), &args);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let name = if args.len() == 2 { "a.out" } else { "hello native" };
        let output = Command::new(dir.path().join(name)).output().unwrap();
        assert!(output.status.success());
        assert_eq!(output.stdout, b"Hello from native Magelang!\n");
    }
}
