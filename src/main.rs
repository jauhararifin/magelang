mod ast;
mod lexer;
mod parser;
mod semantic;
mod token;

use crate::lexer::SimpleLexer;
use crate::parser::SimpleParser;
use ast::Parser;
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let lexer = SimpleLexer::new(f);
    let mut parser = SimpleParser::new(lexer);
    let root_ast = parser.parse().unwrap();
    let mut analyzer = semantic::SimpleAnalyzer::new();
    let program = analyzer.analyze(&root_ast).unwrap();

    println!("{:?}", &program);
}
