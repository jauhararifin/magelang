mod lexer;
mod token;

use std::fs::File;
use crate::token::{Lexer, TokenKind};
use crate::lexer::SimpleLexer;

fn main() {
    let f = File::open("./examples/example1.mag").unwrap();
    let mut lexer = SimpleLexer::new(f);
    loop {
        let token = lexer.next().unwrap();
        if token.kind == TokenKind::EOI {
            break;
        }
        println!("{:?}", token);
    }
}
