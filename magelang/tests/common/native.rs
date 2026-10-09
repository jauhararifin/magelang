use std::path::Path;
use std::process::{Command, Output};
use tempfile::TempDir;

fn compiler(directory: &Path) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_magelang"));
    command.current_dir(directory).env("MAGELANG_ROOT", env!("CARGO_MANIFEST_DIR"));
    command.args(["compile", "main", "--target", "native", "--backend", BACKEND]);
    command
}

fn source_directory(source: &str) -> TempDir {
    let directory = tempfile::tempdir().unwrap();
    std::fs::write(directory.path().join("main.mg"), source).unwrap();
    directory
}

fn success(output: Output) -> Output {
    assert!(
        output.status.success(),
        "{}\nstdout: {}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

#[test]
fn native_hello_default_output() {
    let directory = source_directory(include_str!("../../../examples/native_hello.mg"));
    success(compiler(directory.path()).output().unwrap());
    let output = success(Command::new(directory.path().join("a.out")).output().unwrap());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "Hello from a native binary!\n");
    assert!(!directory.path().join("a.wasm").exists());
}

#[test]
fn native_language_features() {
    let directory = source_directory(include_str!("../native/core.mg"));
    for optimize in [false, true] {
        let mut command = compiler(directory.path());
        if !optimize {
            command.arg("-n");
        }
        command.args(["-o", "native program"]);
        success(command.output().unwrap());
        let output = success(Command::new(directory.path().join("native program")).output().unwrap());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "native core ok\n");
    }
}

#[test]
fn shared_control_flow_and_defer_regressions() {
    for source in [
        include_str!("../test_008/main.mg"),
        include_str!("../test_010/main.mg"),
        include_str!("../test_011/main.mg"),
        include_str!("../test_013/main.mg"),
    ] {
        let source = source
            .replace("import wasm \"std/wasm\";", "@intrinsic(\"unreachable\") fn trap();")
            .replace("wasm.unreachable()", "trap()");
        let directory = source_directory(&source);
        for optimize in [false, true] {
            let mut command = compiler(directory.path());
            if !optimize {
                command.arg("-n");
            }
            success(command.output().unwrap());
            success(Command::new(directory.path().join("a.out")).output().unwrap());
        }
    }
}

#[test]
fn native_packages_and_initializers() {
    let directory = source_directory(
        r#"
        import dep "dep";
        let value: i64 = dep.read();
        fn id[T](value: T): T { return value; }
        @intrinsic("unreachable") fn trap();
        @main() fn main() {
            if value != 42 || dep.id[i32](7) != 7 || id[i32](9) != 9 { trap(); }
            let result = dep.id[dep.Pair](dep.Pair{a: 1, b: 2});
            if result.a != 1 || result.b != 2 { trap(); }
        }
    "#,
    );
    std::fs::write(
        directory.path().join("dep.mg"),
        r#"
        struct Pair { a: i32, b: i64 }
        let value: i64 = 42;
        fn read(): i64 { return value; }
        fn id[T](value: T): T { return value; }
    "#,
    )
    .unwrap();
    success(compiler(directory.path()).output().unwrap());
    success(Command::new(directory.path().join("a.out")).output().unwrap());
}

#[test]
fn native_object_and_c_abi() {
    let directory = source_directory(
        r#"
        @native_import("probe")
        fn probe(a: i8, b: u8, c: i16, d: u16, e: i32, f: u32, g: i64, h: u64,
                 i: f32, j: f64, k: i32, message: [*]u8, callback: fn(i8): i64): i8;
        struct Record { tag: u8, flag: bool, count: i32, wide: i64, ratio: f64 }
        @native_import("record") fn record(): *Record;
        @native_import("check_record") fn check_record(value: *Record, size: usize): bool;
        @native_import("negate") fn negate(value: bool, callback: fn(bool): bool): bool;
        @intrinsic("size_of") fn size_of[T](): usize;
        @intrinsic("unreachable") fn trap();
        fn callback(value: i8): i64 { return value as i64 - 1; }
        fn invert(value: bool): bool { return !value; }
        @main() fn main() {
            let indirect = probe;
            let result = indirect(-1, 255, -1234, 60000, -98765, 4000000000,
                -4294967296, 18446744073709551615, 1.5, -2.25, 42, "FFI", callback);
            if result != -42 { trap(); }
            if negate(true, invert) || !negate(false, invert) { trap(); }
            let value = record();
            if value.tag.* != 255 || !value.flag.* || value.count.* != -123 ||
                value.wide.* != -4294967296 || value.ratio.* != 1.25 { trap(); }
            value.flag.* = false;
            value.count.* = 42;
            if !check_record(value, size_of[Record]()) { trap(); }
        }
    "#,
    );
    success(compiler(directory.path()).arg("--emit-object").env("CC", "/no/linker/needed").output().unwrap());
    assert!(directory.path().join("a.o").exists());
    assert!(!directory.path().join("a.out").exists());
    std::fs::write(
        directory.path().join("probe.c"),
        r#"
        #include <stdint.h>
        #include <string.h>
        #include <stdbool.h>
        struct Record { uint8_t tag; bool flag; int32_t count; int64_t wide; double ratio; };
        struct Record *record(void) {
            static struct Record value = {255, true, -123, -INT64_C(4294967296), 1.25};
            return &value;
        }
        bool check_record(struct Record *value, size_t size) {
            return size == sizeof(*value) && value->tag == 255 && !value->flag &&
                value->count == 42 && value->wide == -INT64_C(4294967296) && value->ratio == 1.25;
        }
        bool negate(bool value, bool (*callback)(bool)) { return callback(value); }
        int8_t probe(int8_t a, uint8_t b, int16_t c, uint16_t d, int32_t e, uint32_t f,
                     int64_t g, uint64_t h, float i, double j, int32_t k,
                     const char *message, int64_t (*callback)(int8_t)) {
            if (a != -1 || b != 255 || c != -1234 || d != 60000 || e != -98765 ||
                f != UINT32_C(4000000000) || g != -INT64_C(4294967296) || h != UINT64_MAX ||
                i != 1.5f || j != -2.25 || k != 42 || strcmp(message, "FFI") || callback(-3) != -4)
                return 0;
            return -42;
        }
    "#,
    )
    .unwrap();
    success(
        Command::new(std::env::var_os("CC").unwrap_or_else(|| "cc".into()))
            .current_dir(directory.path())
            .args(["a.o", "probe.c", "-o", "ffi", "-lm"])
            .output()
            .unwrap(),
    );
    success(Command::new(directory.path().join("ffi")).output().unwrap());
}

#[test]
fn native_slice_bounds_trap() {
    use std::os::unix::process::ExitStatusExt;
    for (index_type, index, length) in [
        ("i8", "-1", "2"),
        ("i16", "-2", "18446744073709551615"),
        ("i64", "-1", "18446744073709551615"),
        ("u64", "18446744073709551615", "2"),
        ("usize", "2", "2"),
        ("i64", "4294967296", "2"),
        ("i32", "0", "0"),
    ] {
        let source = format!(
            r#"
            @native_import("exit") fn exit(status: i32);
            fn index(value: {index_type}): usize {{
                let slice = *[u8]{{ptr: "abc", len: {length}}};
                return slice[value] as usize;
            }}
            @main() fn main() {{
                let address = index({index});
                exit((address & 0) as i32);
            }}
        "#
        );
        let directory = source_directory(&source);
        success(compiler(directory.path()).output().unwrap());
        let output = Command::new(directory.path().join("a.out")).current_dir(directory.path()).output().unwrap();
        assert!(output.status.signal().is_some(), "{index_type} {index}/{length}: {}", output.status);
    }
}

#[test]
fn native_diagnostics() {
    for (source, expected) in [
        ("fn unused() {}", "require a function annotated with @main()"),
        ("@main() fn a() {} @main() fn b() {}", "multiple functions annotated with @main()"),
        ("@main() fn main(value: i32) {}", "@main() requires a non-generic function"),
        ("@main() fn main() {} fn missing();", "a function needs a body"),
        (
            r#"@main() fn main() {} @wasm_import("wasi_snapshot_preview1", "fd_write") fn write();"#,
            "@wasm_import is not supported",
        ),
        (
            r#"@main() fn main() {} @intrinsic("memory.grow") fn grow(size: usize): usize;"#,
            "is not supported by the native backend",
        ),
        (r#"@main() fn main() {} @intrinsic("f32.floor") fn floor(): f32;"#, "invalid f32.floor signature"),
        (r#"@main() fn main() {} @native_import("puts") fn puts(message: *[u8]): i32;"#, "only support scalar/pointer"),
        (r#"@main() fn main() {} @native_import("main") fn imported();"#, "reserved native symbol"),
        (r#"@main() fn main() {} @native_import() fn imported();"#, "@native_import expects one symbol name"),
        (r#"@main() fn main() {} @native_import("puts") fn imported() {}"#, "a function needs a body"),
        (r#"@main() fn main() { let value: opaque; }"#, "opaque references are not supported"),
        (r#"@main() fn main() {} @unknown() fn other() {}"#, "unsupported native annotation"),
        (
            r#"@main() fn main() {} @embed_file("main.mg") let content: [*]u8;"#,
            "global annotations are not yet supported",
        ),
    ] {
        let directory = source_directory(source);
        std::fs::write(directory.path().join("a.out"), "existing output").unwrap();
        let output = compiler(directory.path()).output().unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(!output.status.success(), "{source}");
        assert!(stderr.contains(expected), "{source}\n{stderr}");
        assert!(!stderr.contains("panicked"), "{stderr}");
        assert_eq!(std::fs::read_to_string(directory.path().join("a.out")).unwrap(), "existing output");
    }
}

#[test]
fn native_linker_errors() {
    let directory = source_directory("@main() fn main() {}");
    let output = compiler(directory.path()).env("CC", directory.path().join("missing-cc")).output().unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8(output.stderr).unwrap().contains("starting linker"));
    let directory = source_directory(
        r#"
        @native_import("magelang_nonexistent_symbol") fn missing();
        @main() fn main() { missing(); }
    "#,
    );
    let output = compiler(directory.path()).output().unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("linker") && stderr.contains("magelang_nonexistent_symbol"), "{stderr}");
}

#[test]
fn wasm_remains_default() {
    let directory = source_directory("@main() fn main() {}");
    success(
        Command::new(env!("CARGO_BIN_EXE_magelang"))
            .current_dir(directory.path())
            .args(["compile", "main", "-n"])
            .output()
            .unwrap(),
    );
    let bytes = std::fs::read(directory.path().join("a.wasm")).unwrap();
    assert_eq!(&bytes[..4], b"\0asm");
    let output = Command::new(env!("CARGO_BIN_EXE_magelang"))
        .current_dir(directory.path())
        .args(["compile", "main", "--emit-object"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8(output.stderr).unwrap().contains("--emit-object requires --target native"));
}
