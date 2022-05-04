// use magelang::analyzer::{Analyzer, SimpleAnalyzer, analyze_asts};
// use magelang::compiler::{Compiler, SimpleCompiler};
// use magelang::header_processor::{HeaderProcessor, SimpleHeaderProcessor};
use magelang::lexer::SimpleLexer;
use magelang::parser::{Parser, SimpleParser};
use magelang::analyzer::HeaderCompiler;
use std::env;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 0 {
        println!("please specify the list of files you want to compile");
        return;
    }

    let mut root_asts = Vec::new();
    for arg in args.iter().skip(1) {
        let f = File::open(arg).unwrap();
        let lexer = SimpleLexer::new(f);
        let mut parser = SimpleParser::new(lexer);
        let root_ast = parser.parse().unwrap();
        root_asts.push(root_ast);
    }

    let header_processor = HeaderCompiler::new();
    let headers = header_processor.compile(root_asts.as_slice()).unwrap();

    for header in headers.iter() {
        println!("header {:?}", header.package_name);
        println!("{:?}", header);
        // for func in header.functions.iter() {
        //     // let typ = func.typ.borrow();
        //     let typ = typ.unwrap_func();
        //     println!("func {:?} {:?}", func.name, typ);
        //     for arg in typ.arguments.iter() {
        //         println!("arg {:?} {:?}", arg.name, arg.typ.upgrade());
        //     }
        // }
        println!("==========================================");
    }


    // let units = analyze_asts(&root_asts[..], &headers[..]).unwrap();
    // for unit in units.iter() {
    //     println!("{:?}", unit);
    //     println!("=======================================");
    // }

    // let f = File::open("./examples/example1.mag").unwrap();
    // let lexer = SimpleLexer::new(f);
    // let mut parser = SimpleParser::new(lexer);
    // let root_ast = parser.parse().unwrap();
    //
    // println!("ast: {:?}", root_ast);
    //
    // let mut analyzer = SimpleAnalyzer::new(root_ast);
    // let semantic = analyzer.analyze().unwrap();
    // let mut compiler = SimpleCompiler::new(semantic);
    // let program = compiler.compile().unwrap();
    //
    // println!("program: {:?}", program);
}
