use magelang::analyzer::{Analyzer, SimpleAnalyzer};
use magelang::compiler::{Compiler, SimpleCompiler};
use magelang::lexer::SimpleLexer;
use magelang::parser::{Parser, SimpleParser};
use std::env;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 0 {
        println!("please specify the list of files you want to compile");
        return
    }

    let f = File::open("./examples/example1.mag").unwrap();
    let lexer = SimpleLexer::new(f);
    let mut parser = SimpleParser::new(lexer);
    let root_ast = parser.parse().unwrap();

    println!("ast: {:?}", root_ast);

    let mut analyzer = SimpleAnalyzer::new(root_ast);
    let semantic = analyzer.analyze().unwrap();
    let mut compiler = SimpleCompiler::new(semantic);
    let program = compiler.compile().unwrap();

    println!("program: {:?}", program);
}
