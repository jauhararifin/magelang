#[path = "../tests/support/parser_fuzz.rs"]
mod parser_fuzz;

use clap::Parser;
use std::error::Error;
use std::path::PathBuf;
use std::process::ExitCode;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

#[derive(Parser)]
#[command(about = "Generate seeded, syntactically valid Magelang programs and test the parser")]
struct Options {
    /// First seed; subsequent cases use consecutive seeds. Random when omitted.
    #[arg(long)]
    seed: Option<u64>,

    #[arg(long, default_value_t = 10_000, value_parser = clap::value_parser!(u64).range(1..))]
    cases: u64,

    /// Maximum depth of recursive grammar expansions.
    #[arg(long, default_value_t = 6, value_parser = clap::value_parser!(u8).range(..=64))]
    depth: u8,

    /// Shared recursive expansion budget per program (terminal forms are still emitted).
    #[arg(long, default_value_t = 256)]
    budget: usize,

    /// Directory for reproducer files, in a separate subdirectory for each run.
    #[arg(long, default_value = "target/parser-fuzz")]
    artifacts: PathBuf,

    /// Parse a previously saved input instead of generating new programs.
    #[arg(long, conflicts_with_all = ["seed", "cases", "depth", "budget"])]
    input: Option<PathBuf>,
}

fn run(options: Options) -> Result<(), Box<dyn Error>> {
    if let Some(path) = options.input {
        parser_fuzz::check(&path, std::fs::read_to_string(&path)?)?;
        println!("Parsed {} without errors", path.display());
        return Ok(());
    }

    let first_seed = options.seed.unwrap_or_else(|| fastrand::u64(..));
    let timestamp = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
    let run_dir = options
        .artifacts
        .join(format!("{timestamp}-{}", std::process::id()));
    std::fs::create_dir_all(&run_dir)?;
    let path = run_dir.join("current.mg");
    eprintln!(
        "seed={first_seed} cases={} depth={} budget={}\nInput saved before each parse: {}",
        options.cases,
        options.depth,
        options.budget,
        path.display()
    );

    let start = Instant::now();
    for case in 0..options.cases {
        let seed = first_seed.wrapping_add(case);
        let source = parser_fuzz::generate(seed, options.depth, options.budget);
        // Save before parsing: stack overflow and aborts cannot be caught with catch_unwind.
        std::fs::write(&path, &source)?;
        if let Err(diagnostics) = parser_fuzz::check(&path, source) {
            return Err(format!(
                "seed={seed} depth={} budget={}\n{diagnostics}\nReproducer: {}",
                options.depth,
                options.budget,
                path.display()
            )
            .into());
        }
        if (case + 1) % 1_000 == 0 {
            eprintln!("Parsed {} programs ({:.1?})", case + 1, start.elapsed());
        }
    }
    println!(
        "Parsed {} programs without errors in {:.1?}. Last input: {}",
        options.cases,
        start.elapsed(),
        path.display()
    );
    Ok(())
}

fn main() -> ExitCode {
    match run(Options::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
