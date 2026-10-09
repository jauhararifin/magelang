mod native;

use bumpalo::Bump;
use clap::{Args, Parser, Subcommand, ValueEnum};
use magelang_syntax::{parse, ErrorManager, FileManager};
use magelang_typecheck::analyze;
use magelang_wasmgen::generate;
use std::io::Write;
use wasm_helper::Serializer;
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::WasiCtxBuilder;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Target {
    Wasm,
    Native,
}

#[derive(Clone, Copy, Default, PartialEq, Eq, ValueEnum)]
enum NativeBackend {
    #[default]
    Cranelift,
    Llvm,
}

#[derive(Args)]
struct CompileOptions {
    package_name: String,

    #[arg(short)]
    debug: bool,

    #[arg(short)]
    noopt: bool,

    /// Compilation target (native compiles for the current host).
    #[arg(long, value_enum, default_value = "wasm")]
    target: Target,

    /// Native code generator (defaults to cranelift; requires --target native).
    #[arg(long, value_enum)]
    backend: Option<NativeBackend>,

    /// Emit a native object file without linking (requires --target native).
    #[arg(long)]
    emit_object: bool,

    /// Emit LLVM IR without invoking Clang (requires --target native --backend llvm).
    #[arg(long, conflicts_with = "emit_object")]
    emit_llvm: bool,

    /// Output path (defaults to a.wasm, a.out, a.o, or a.ll).
    #[arg(short, long)]
    output: Option<std::path::PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    Parse {
        file_name: std::path::PathBuf,

        #[arg(short, long)]
        output: Option<std::path::PathBuf>,
    },
    Analyze {
        package_name: String,

        #[arg(short)]
        debug: bool,

        #[arg(short, long)]
        output: Option<std::path::PathBuf>,
    },
    Compile(CompileOptions),
    Run {
        package_name: String,
        #[arg(short)]
        debug: bool,
    },
}

fn main() {
    let args = Cli::parse();
    match args.command {
        Commands::Parse { file_name, output } => parse_ast(file_name, output),
        Commands::Analyze { package_name, debug, output } => analyze_package(package_name, debug, output),
        Commands::Compile(options) => compile(options),
        Commands::Run { package_name, debug } => run(package_name, debug),
    }
}

fn parse_ast(file_name: std::path::PathBuf, output: Option<std::path::PathBuf>) {
    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let displayed_path = file_name.clone();
    let file = match file_manager.open(file_name) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Cannot open file {}: {err}", displayed_path.to_string_lossy());
            std::process::exit(-1);
        }
    };

    let mut writer: Box<dyn std::io::Write> = if let Some(path) = output {
        let file = std::fs::File::create(path).unwrap();
        Box::new(file)
    } else {
        Box::new(std::io::stdout().lock())
    };

    let node = parse(&error_manager, &file);
    if !error_manager.is_empty() {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
        std::process::exit(-1);
    }

    if let Err(err) = write!(writer, "{:#?}", node) {
        eprintln!("Cannot write output: {err}",);
        std::process::exit(-1);
    }
}

fn analyze_package(package_name: String, debug: bool, output: Option<std::path::PathBuf>) {
    let mut error_manager = if debug { ErrorManager::new_for_debug() } else { ErrorManager::default() };
    let mut file_manager = FileManager::default();
    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);

    let mut writer: Box<dyn std::io::Write> = if let Some(path) = output {
        let file = std::fs::File::create(path).unwrap();
        Box::new(file)
    } else {
        Box::new(std::io::stdout().lock())
    };
    let _ = write!(writer, "{module:#?}");

    if !error_manager.is_empty() {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
    }
}

fn compile(options: CompileOptions) {
    let CompileOptions { package_name, debug, noopt, target, backend, emit_object, emit_llvm, output } = options;
    let optimize = !noopt;
    if emit_llvm && (target != Target::Native || backend != Some(NativeBackend::Llvm)) {
        eprintln!("--emit-llvm requires --target native --backend llvm");
        std::process::exit(1);
    }
    if backend.is_some() && target != Target::Native {
        eprintln!("--backend requires --target native");
        std::process::exit(1);
    }
    if emit_object && target != Target::Native {
        eprintln!("--emit-object requires --target native");
        std::process::exit(1);
    }
    let output = output.unwrap_or_else(|| {
        if target == Target::Wasm {
            "a.wasm"
        } else if emit_object {
            "a.o"
        } else if emit_llvm {
            "a.ll"
        } else {
            "a.out"
        }
        .into()
    });
    let mut error_manager = if debug { ErrorManager::new_for_debug() } else { ErrorManager::default() };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
    if !module.is_valid {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
        std::process::exit(-1);
    };

    if target == Target::Native {
        if let Err(error) =
            native::compile(&module, optimize, backend.unwrap_or_default(), emit_object, emit_llvm, &output)
        {
            eprintln!("Native compilation failed: {error:#}");
            std::process::exit(1);
        }
        return;
    }

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
        std::process::exit(-1);
    };

    let mut raw_module = Vec::<u8>::default();
    wasm_module.serialize(&mut raw_module).expect("cannot serialize wasm module");

    let mut f = std::fs::File::create(output).expect("cannot create output file");
    if optimize {
        let mut wasm_module = binaryen::Module::read(&raw_module).expect("can't read wasm module for optimization");
        wasm_module.optimize(&binaryen::CodegenConfig { shrink_level: 2, optimization_level: 2, debug_info: true });

        f.write_all(&wasm_module.write()).expect("cannot write wasm module to output file");
    } else {
        f.write_all(&raw_module).expect("cannot write wasm module to output file");
    }
}

fn run(package_name: String, debug: bool) {
    let mut error_manager = if debug { ErrorManager::new_for_debug() } else { ErrorManager::default() };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
    if !module.is_valid {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
        std::process::exit(-1);
    };

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        for error in error_manager.take() {
            eprintln!("{}", error.display(&file_manager));
        }
        std::process::exit(-1);
    };

    let mut module = Vec::<u8>::default();
    wasm_module.serialize(&mut module).expect("cannot write wasm to target file");

    let engine = Engine::default();

    let module = Module::from_binary(&engine, &module).expect("cannot load wasm module");
    let mut linker: Linker<WasiP1Ctx> = Linker::new(&engine);
    p1::add_to_linker_sync(&mut linker, |s| s).expect("cannot link wasi to the linker");
    let wasi = WasiCtxBuilder::new().inherit_stdio().inherit_args().build_p1();
    let mut store = Store::new(&engine, wasi);
    linker.instantiate(&mut store, &module).unwrap();
}
