mod ast;
mod lexer;
mod parser;
mod token;

use crate::lexer::SimpleLexer;
use crate::parser::SimpleParser;
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let lexer = SimpleLexer::new(f);
    let mut parser = SimpleParser::new(lexer);
    let root_ast = parser.parse().unwrap();

    println!("{:?}", &root_ast);
}
