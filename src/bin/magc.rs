use magelang::lexer::{Lexer, SimpleLexer};
use magelang::parser::{Parser, SimpleParser};
use magelang::semantic::SimpleAnalyzer;
use magelang::token::TokenKind;
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let mut lexer = SimpleLexer::new(f);

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
    let mut analyzer = SimpleAnalyzer::new();
    let program = analyzer.analyze(&root_ast).unwrap();

    println!("{:?}", &program);
}
