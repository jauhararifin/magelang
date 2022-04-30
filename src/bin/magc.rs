use magelang::compiler::{Compiler, SimpleCompiler};
use magelang::lexer::SimpleLexer;
use magelang::parser::{Parser, SimpleParser};
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let lexer = SimpleLexer::new(f);

    // loop {
    //     let t = lexer.next().unwrap();
    //     println!("{:?}", t);
    //     if t.kind == TokenKind::Eoi {
    //         break;
    //     }
    // }

    let mut parser = SimpleParser::new(lexer);
    let root_ast = parser.parse().unwrap();
    println!("{:?}", root_ast);
    let program = SimpleCompiler::new(root_ast).compile().unwrap();
    println!("{:?}", &program);
}
