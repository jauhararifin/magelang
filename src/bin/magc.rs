use magelang::analyzer::{self, Analyzer, SimpleAnalyzer};
// use magelang::compiler::{Compiler, SimpleCompiler};
use magelang::lexer::SimpleLexer;
use magelang::parser::{Parser, SimpleParser};
use std::fs::File;

use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    // let x = {
    //     Rc::downgrade(&Rc::new(3))
    // };
    // let y = Rc::new(4);
    // let z = Rc::downgrade(&y);
    // println!("{:?} {:?} {:?}", x.upgrade(), y, z.upgrade());
    // let y = 10;
    // println!("{:?} {:?}", x.upgrade(), z.upgrade());
    // panic!("asdf");

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
    // let program = SimpleCompiler::new(root_ast).compile().unwrap();
    // println!("{:?}", &program);

    let mut analyzer = SimpleAnalyzer::new(root_ast);
    let semantic = analyzer.analyze().unwrap();
    println!("{:?}", semantic);
}
