use bumpalo::Bump;
use clap::{Parser, Subcommand};
use magelang_syntax::{parse, ErrorManager, FileManager};
use magelang_typecheck::analyze;
use magelang_wasmgen::generate;
use wasm_helper::Serializer;
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::sync::WasiCtxBuilder;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
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
    Compile {
        package_name: String,

        #[arg(short)]
        debug: bool,

        #[arg(short, long, default_value = "./a.wasm")]
        output: std::path::PathBuf,
    },
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
        Commands::Analyze {
            package_name,
            debug,
            output,
        } => analyze_package(package_name, debug, output),
        Commands::Compile {
            package_name,
            debug,
            output,
        } => compile(package_name, debug, output),
        Commands::Run {
            package_name,
            debug,
        } => run(package_name, debug),
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
            return;
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
    }
    let _ = write!(writer, "{:#?}", node);
}

fn analyze_package(package_name: String, debug: bool, output: Option<std::path::PathBuf>) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
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
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
    }
}

fn compile(package_name: String, debug: bool, output: std::path::PathBuf) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
    if !module.is_valid {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    let mut f = std::fs::File::create(output).expect("cannot create output file");
    let Some(wasm_module) = generate(&arena, &file_manager, &error_manager, &module) else {
        for error in error_manager.take() {
            let location = file_manager.location(error.pos);
            let message = error.message;
            eprintln!("{location}: {message}");
        }
        std::process::exit(-1);
    };

    wasm_module
        .serialize(&mut f)
        .expect("cannot write wasm to target file");
}

fn run(package_name: String, debug: bool) {
    let mut error_manager = if debug {
        ErrorManager::new_for_debug()
    } else {
        ErrorManager::default()
    };
    let mut file_manager = FileManager::default();

    let arena = Bump::default();
    let module = analyze(&arena, &mut file_manager, &error_manager, &package_name);
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
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(&mut linker, |s| s).expect("cannot link wasi to the linker");
    let wasi = WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_args()
        .expect("cannot build wasi context")
        .build();
    let mut store = Store::new(&engine, wasi);
    linker.instantiate(&mut store, &module).unwrap();
}
