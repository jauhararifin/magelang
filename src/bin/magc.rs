use magelang::analyzer::{analyze_root, HeaderCompiler};
use magelang::compiler::SimpleCompiler;
use magelang::lexer::Lexer;
use magelang::linker::Linker;
use magelang::parser::{IParser, Parser};
use magelang::vm::Executor;
use std::env;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("please specify the file you want to compile");
        return;
    }

    let file_name = args.get(1).unwrap();
    let f = File::open(file_name).unwrap();
    let lexer = Lexer::new(f, file_name);
    let mut parser = Parser::new(lexer);
    let root_ast = parser.parse().unwrap();
    let header_compiler = HeaderCompiler::new();
    let header = header_compiler.compile(&root_ast).unwrap();

    let headers = vec![header];
    let unit = analyze_root(&root_ast, &headers[..]).unwrap();

    let mut compiler = SimpleCompiler::new(unit);
    let object = compiler.compile().unwrap();

    let linker = Linker::new();
    let objects = vec![object];
    let program = linker.link(&objects[..]).unwrap();

    // println!("program {:?}", program);

    let mut executor = Executor::new();
    executor.execute(&program);
}
