use clap::{Args, Parser, Subcommand};
use magelang_semantic::{check_main_package, AstDb, Db, SymbolDb};

#[derive(Parser, Debug)]
struct CliArgs {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Compile(CompileArgs),
    Check(CheckArgs),
    Run(RunArgs),
}

#[derive(Args, Debug)]
struct CompileArgs {
    package_name: String,

    #[arg(short = 'o', long = "output")]
    output_file: Option<String>,
}

#[derive(Args, Debug)]
struct CheckArgs {
    package_name: String,
}

#[derive(Args, Debug)]
struct RunArgs {
    package_name: String,
}

fn main() {
    let args = CliArgs::parse();

    let db = Db::default();

    match args.command {
        Command::Compile(..) => {
            todo!();
        }
        Command::Check(arg) => {
            let main_package = db.define_symbol(arg.package_name.into());
            check_main_package(&db, main_package.into());

            let mut has_error = false;
            for (loc, message) in db.take_errors() {
                has_error = true;
                let location = db.get_location(loc);
                eprintln!("{}: {}", location, message);
            }

            if has_error {
                std::process::exit(1);
            }
        }
        Command::Run(..) => {
            todo!();
        }
    }
}
