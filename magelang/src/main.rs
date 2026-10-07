use bumpalo::Bump;
use clap::{Args, Parser, Subcommand};
use magelang_syntax::{parse, ErrorManager, FileManager};
use magelang_typecheck::{analyze_with_options, AnalyzeOptions, DEFAULT_GENERIC_RECURSION_LIMIT};
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

#[derive(Args)]
struct TypecheckCliOptions {
    #[arg(
        long,
        default_value_t = DEFAULT_GENERIC_RECURSION_LIMIT,
        help = "Stop after reaching this generic instantiation depth"
    )]
    generic_recursion_limit: usize,
}

impl From<TypecheckCliOptions> for AnalyzeOptions {
    fn from(value: TypecheckCliOptions) -> Self {
        Self {
            generic_recursion_limit: value.generic_recursion_limit,
        }
    }
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

        #[command(flatten)]
        typecheck_options: TypecheckCliOptions,
    },
    Compile {
        package_name: String,

        #[arg(short)]
        debug: bool,

        #[arg(short)]
        noopt: bool,

        #[arg(short, long, default_value = "./a.wasm")]
        output: std::path::PathBuf,

        #[command(flatten)]
        typecheck_options: TypecheckCliOptions,
    },
    Run {
        package_name: String,

        #[arg(short)]
        debug: bool,

        #[command(flatten)]
        typecheck_options: TypecheckCliOptions,
    },
}

fn main() {
    let args = Cli::parse();
    match args.command {
        Commands::Parse { file_name, output } => parse_ast(file_name, output),
        Commands::Analyze {
            package_name,
            debug,
            output,
            typecheck_options,
        } => analyze_package(package_name, debug, output, typecheck_options.into()),
        Commands::Compile {
            package_name,
            debug,
            noopt,
            output,
            typecheck_options,
        } => compile(
            package_name,
            debug,
            !noopt,
            output,
            typecheck_options.into(),
        ),
        Commands::Run {
            package_name,
            debug,
            typecheck_options,
        } => run(package_name, debug, typecheck_options.into()),
    }
}

fn parse_ast(file_name: std::path::PathBuf, output: Option<std::path::PathBuf>) {
    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let displayed_path = file_name.clone();
    let file = match file_manager.open(file_name) {
        Ok(file) => file,
        Err(err) => {
            eprintln!(
                "Cannot open file {}: {err}",
                displayed_path.to_string_lossy()
            );
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
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    }

    if let Err(err) = write!(writer, "{:#?}", node) {
        eprintln!("Cannot write output: {err}",);
        std::process::exit(-1);
    }
}

fn analyze_package(
    package_name: String,
    debug: bool,
    output: Option<std::path::PathBuf>,
    analyze_options: AnalyzeOptions,
) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
    let mut file_manager = FileManager::default();
    let arena = Bump::default();
    let module = analyze_with_options(
        &arena,
        &mut file_manager,
        &error_manager,
        &package_name,
        analyze_options,
    );

    let mut writer: Box<dyn std::io::Write> = if let Some(path) = output {
        let file = std::fs::File::create(path).unwrap();
        Box::new(file)
    } else {
        Box::new(std::io::stdout().lock())
    };
    let _ = write!(writer, "{module:#?}");

    if !error_manager.is_empty() {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
    }
}

fn compile(
    package_name: String,
    debug: bool,
    optimize: bool,
    output: std::path::PathBuf,
    analyze_options: AnalyzeOptions,
) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze_with_options(
        &arena,
        &mut file_manager,
        &error_manager,
        &package_name,
        analyze_options,
    );
    if !module.is_valid {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    let mut raw_module = Vec::<u8>::default();
    wasm_module
        .serialize(&mut raw_module)
        .expect("cannot serialize wasm module");

    let mut f = std::fs::File::create(output).expect("cannot create output file");
    if optimize {
        let mut wasm_module =
            binaryen::Module::read(&raw_module).expect("can't read wasm module for optimization");
        wasm_module.optimize(&binaryen::CodegenConfig {
            shrink_level: 2,
            optimization_level: 2,
            debug_info: true,
        });

        f.write_all(&wasm_module.write())
            .expect("cannot write wasm module to output file");
    } else {
        f.write_all(&raw_module)
            .expect("cannot write wasm module to output file");
    }
}

fn run(package_name: String, debug: bool, analyze_options: AnalyzeOptions) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze_with_options(
        &arena,
        &mut file_manager,
        &error_manager,
        &package_name,
        analyze_options,
    );
    if !module.is_valid {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    let mut module = Vec::<u8>::default();
    wasm_module
        .serialize(&mut module)
        .expect("cannot write wasm to target file");

    let engine = Engine::default();

    let module = Module::from_binary(&engine, &module).expect("cannot load wasm module");
    let mut linker: Linker<WasiP1Ctx> = Linker::new(&engine);
    p1::add_to_linker_sync(&mut linker, |s| s).expect("cannot link wasi to the linker");
    let wasi = WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_args()
        .build_p1();
    let mut store = Store::new(&engine, wasi);
    linker.instantiate(&mut store, &module).unwrap();
}
