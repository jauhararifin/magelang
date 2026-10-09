use crate::NativeBackend;
use anyhow::{ensure, Context, Result};
use magelang_typecheck::Module;
use std::path::Path;
use std::process::Command;

pub fn compile<'a>(
    module: &'a Module<'a>,
    optimize: bool,
    backend: NativeBackend,
    emit_object: bool,
    emit_llvm: bool,
    output: &Path,
) -> Result<()> {
    if emit_llvm {
        let ir = magelang_llvm::generate(module)?;
        return std::fs::write(output, ir).with_context(|| format!("writing {}", output.display()));
    }

    let directory = tempfile::tempdir().context("creating native compiler temporary directory")?;
    let object = directory.path().join("program.o");
    match backend {
        NativeBackend::Cranelift => {
            let bytes = magelang_cranelift::generate(module, optimize)?;
            std::fs::write(&object, bytes).context("writing native linker input")?;
        }
        NativeBackend::Llvm => {
            let ir = magelang_llvm::generate(module)?;
            let source = directory.path().join("program.ll");
            std::fs::write(&source, ir).context("writing LLVM IR")?;
            let clang = std::env::var_os("LLVM_CLANG").unwrap_or_else(|| "clang".into());
            let result = Command::new(&clang)
                .args(["-x", "ir", "-c", "-fPIC", "-Wno-override-module", "-target", magelang_llvm::host_triple()?])
                .arg(if optimize { "-O2" } else { "-O0" })
                .arg(&source).arg("-o").arg(&object).output()
                .with_context(|| format!("starting LLVM compiler {clang:?}; LLVM codegen requires Clang 15+ (set LLVM_CLANG to its executable)"))?;
            ensure!(
                result.status.success(),
                "LLVM compiler {clang:?} failed ({}):\n{}",
                result.status,
                String::from_utf8_lossy(&result.stderr)
            );
        }
    }
    if emit_object {
        return std::fs::copy(&object, output).map(|_| ()).with_context(|| format!("writing {}", output.display()));
    }
    let linker = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    let result = Command::new(&linker).arg(&object).arg("-o").arg(output).arg("-lm").output().with_context(|| {
        format!("starting linker {linker:?}; install a system C toolchain or set CC to its compiler executable")
    })?;
    ensure!(
        result.status.success(),
        "linker {linker:?} failed ({}):\n{}",
        result.status,
        String::from_utf8_lossy(&result.stderr)
    );
    Ok(())
}
