use magelang::analyzer::{Analyzer, SimpleAnalyzer};
use magelang::compiler::{Compiler, SimpleCompiler};
use magelang::lexer::SimpleLexer;
use magelang::parser::{Parser, SimpleParser};
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let lexer = SimpleLexer::new(f);
    let mut parser = SimpleParser::new(lexer);
    let root_ast = parser.parse().unwrap();
    let mut analyzer = SimpleAnalyzer::new(root_ast);
    let semantic = analyzer.analyze().unwrap();
    let mut compiler= SimpleCompiler::new(semantic);
    let program = compiler.compile().unwrap();
    println!("program: {:?}", program);
}
