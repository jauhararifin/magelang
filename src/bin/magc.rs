use magelang::ast::Parser;
use magelang::lexer::{Lexer, SimpleLexer};
use magelang::parser::SimpleParser;
use magelang::semantic::SimpleAnalyzer;
use magelang::token::TokenKind;
use std::fs::File;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let mut lexer = SimpleLexer::new(f);

    loop {
        let token = lexer.next().unwrap();
        println!("{:?}", token);
        if token.kind == TokenKind::Eoi {
            break;
        }
    }

    // let mut parser = SimpleParser::new(lexer);
    // let root_ast = parser.parse().unwrap();
    // let mut analyzer = SimpleAnalyzer::new();
    // let program = analyzer.analyze(&root_ast).unwrap();
    //
    // println!("{:?}", &program);
}
