use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck::analyze;
use std::fs;

fn assembly(source: &str) -> Result<String, String> {
    let directory = tempfile::tempdir().unwrap();
    let package = directory.path().join("main");
    fs::write(package.with_extension("mg"), source).unwrap();
    let arena = Bump::default();
    let mut files = FileManager::default();
    let mut errors = ErrorManager::default();
    let module = analyze(&arena, &mut files, &errors, package.to_str().unwrap());
    magelang_naivegen::generate(&errors, &module).ok_or_else(|| {
        errors.take().iter().map(|error| error.display(&files).to_string()).collect::<Vec<_>>().join("\n")
    })
}

#[test]
fn emits_stack_evaluation_and_native_frames() {
    let code = assembly("fn add(a: i64, b: i64): i64 { return a + b; } @main() fn main() { add(20, 22); }").unwrap();
    assert!(code.contains("stp x29, x30, [sp, #-16]!"));
    assert!(code.contains("ldr x10, [sp], #16\n    ldr x9, [sp], #16\n    add x9, x9, x10\n    str x9, [sp, #-16]!"));
    assert!(code.contains(".globl _main"));
    assert!(code.contains("blr x9"));
}

#[test]
fn emits_all_ir_fixtures() {
    for source in
        [include_str!("fixtures/core.mg"), include_str!("fixtures/numbers.mg"), include_str!("fixtures/memory.mg")]
    {
        assembly(source).unwrap();
    }
}

#[test]
fn rejects_unsupported_annotations_and_invalid_signatures() {
    for (source, message) in [
        ("@wasm_import(\"env\", \"read\") fn read();", "Unsupported annotation @wasm_import"),
        ("@wasm_export(\"foo\") fn foo() {}", "Unsupported annotation @wasm_export"),
        ("@intrinsic(\"memory.size\") fn pages(): usize;", "Unsupported intrinsic"),
        ("@intrinsic(\"size_of\") fn size_of[T](): i8; @main() fn main() { size_of[i64](); }", "Invalid signature"),
        ("fn missing();", "bodyless function requires"),
        ("@native_import(\"puts\") fn puts() {}", "bodyless function requires"),
        ("@native_import(\"bad;symbol\") fn bad();", "requires a C symbol"),
        ("@native_import(\"puts\", \"extra\") fn puts();", "expects 1 arguments"),
        ("struct S { a: i64 } @native_import(\"foo\") fn foo(a: S);", "scalar/pointer signature"),
        ("@native_import(\"foo\") fn foo(cb: fn());", "no aggregates or callbacks"),
        ("@main() fn first() {} @main() fn second() {}", "Multiple @main"),
        ("@main() fn main(a: i64) {}", "@main requires"),
        ("@main() @main() fn main() {}", "Duplicated annotation"),
        ("@other() let value: i32;", "Native globals only support"),
    ] {
        let error = assembly(source).unwrap_err();
        assert!(error.contains(message), "{source}\n{error}");
    }
}

#[test]
fn invalid_frontend_input_returns_no_assembly() {
    assert!(assembly("@main() fn main() { missing(); }").is_err());
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
mod execution {
    use super::*;
    use std::process::{Command, Output};

    fn run(source: &str) -> Output {
        let directory = tempfile::tempdir().unwrap();
        let executable = directory.path().join("program");
        let code = assembly(source).unwrap();
        magelang_naivegen::compile(&code, &executable).unwrap();
        let bytes = fs::read(&executable).unwrap();
        assert_eq!(&bytes[..4], &[0xcf, 0xfa, 0xed, 0xfe]);
        Command::new(executable).output().unwrap()
    }

    fn succeeds(source: &str) -> Output {
        let result = run(source);
        assert!(result.status.success(), "{}: {}", result.status, String::from_utf8_lossy(&result.stderr));
        result
    }

    #[test]
    fn calls_globals_control_flow_and_defers() {
        succeeds(include_str!("fixtures/core.mg"));
    }

    #[test]
    fn integer_float_operations_and_casts() {
        succeeds(include_str!("fixtures/numbers.mg"));
    }

    #[test]
    fn pointers_structs_slices_and_libc() {
        let output = succeeds(include_str!("fixtures/memory.mg"));
        assert_eq!(output.stdout, b"native memory ok\n");
    }

    #[test]
    fn traps_on_bounds_and_invalid_arithmetic() {
        for statement in [
            "let s = *[u8]{len: 2}; s[2];",
            "let s = *[u8]{len: 18446744073709551615}; s[-2];",
            "let s: *[u8]; s[0];",
            "let s = *[u8]{len: 2}; s[4294967296 as u64];",
            "let zero: i64; let one: i64 = 1; one / zero;",
            "let zero: u32; let one: u32 = 1; one % zero;",
            "let min: i64 = -9223372036854775808; min / -1;",
            "let min: i32 = -2147483648; min / -1;",
            "let zero: f64; let nan = zero / zero; nan as i64;",
            "let large: f64 = 18446744073709551616.0; large as u64;",
            "let negative: f32 = -1.0; negative as u32;",
        ] {
            let output = run(&format!("@main() fn main() {{ {statement} }}"));
            assert!(!output.status.success(), "did not trap: {statement}");
        }
    }

    #[test]
    fn large_stack_frames_and_expression_depth() {
        let mut source = String::from("@intrinsic(\"unreachable\") fn trap(); @main() fn main() {\n");
        for index in 0..400 {
            source.push_str(&format!("let v{index}: i64 = {index};\n"));
        }
        source.push_str("let sum = ");
        source.push_str(&(0..64).map(|i| format!("v{i}")).collect::<Vec<_>>().join(" + "));
        source.push_str("; if sum != 2016 || v399 != 399 { trap(); } }");
        succeeds(&source);
    }

    #[test]
    fn imports_use_apple_register_and_stack_argument_abi() {
        let source = r#"
            @native_import("mixed")
            fn mixed(a: i8, x: f32, b: u16, y: f64, c: i32, d: i64, e: i64, f: i64, g: i64, h: i64,
                     i: i8, j: u16, k: i32, l: i64, z: f64, p: f64, q: f64, r: f64, s: f64, t: f64,
                     u: f32, v: f64): f64;
            @native_import("small") fn small(a: i8): i8;
            @native_import("single") fn single(a: f32): f32;
            @intrinsic("unreachable") fn trap();
            @main() fn main() {
                let f = mixed;
                if f(-1, 1.5, 65535, 2.5, -3, 4, 5, 6, 7, 8, -9, 65534, -11, 12,
                     3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5) != 60.0 { trap(); }
                if small(-120) != -121 || single(1.5) != 3.0 { trap(); }
            }
        "#;
        let c = r#"
            #include <stdint.h>
            #include <stdlib.h>
            double mixed(int8_t a, float x, uint16_t b, double y, int32_t c, int64_t d,
                         int64_t e, int64_t f, int64_t g, int64_t h, int8_t i, uint16_t j,
                         int32_t k, int64_t l, double z, double p, double q, double r,
                         double s, double t, float u, double v) {
                if (a != -1 || b != 65535 || c != -3 || d != 4 || e != 5 || f != 6 || g != 7 || h != 8
                    || i != -9 || j != 65534 || k != -11 || l != 12) abort();
                return x + y + z + p + q + r + s + t + u + v;
            }
            int8_t small(int8_t a) { return a - 1; }
            float single(float a) { return a * 2; }
        "#;
        let dir = tempfile::tempdir().unwrap();
        let asm_path = dir.path().join("program.s");
        let c_path = dir.path().join("bridge.c");
        let exe = dir.path().join("program");
        fs::write(&asm_path, assembly(source).unwrap()).unwrap();
        fs::write(&c_path, c).unwrap();
        let output = Command::new("clang")
            .args(["-target", "arm64-apple-macos11"])
            .arg(&asm_path)
            .arg(&c_path)
            .arg("-o")
            .arg(&exe)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert!(Command::new(exe).status().unwrap().success());
    }

    #[test]
    fn empty_entry_and_embedded_data() {
        succeeds("fn unused() {}");
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("message.txt");
        fs::write(&path, b"embedded bytes").unwrap();
        let source = format!(
            r#"
            @embed_file("{}") let message: *[u8];
            @native_import("write") fn write(fd: i32, bytes: [*]u8, len: usize): isize;
            @intrinsic("unreachable") fn trap();
            @main() fn main() {{
                if message.len != 14 || message.ptr[message.len].* != 0 {{ trap(); }}
                write(1, message.ptr, message.len);
            }}
        "#,
            path.display()
        );
        assert_eq!(succeeds(&source).stdout, b"embedded bytes");
    }

    #[test]
    fn clang_failures_are_reported() {
        let dir = tempfile::tempdir().unwrap();
        let error = magelang_naivegen::compile("not_valid_assembly", dir.path().join("program")).unwrap_err();
        assert!(error.to_string().contains("clang failed"));
        assert!(error.to_string().contains("error:"));
    }
}
