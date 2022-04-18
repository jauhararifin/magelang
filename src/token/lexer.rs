use super::{Token, Error};

pub trait Lexer {
    fn next(&mut self) -> Result<Token, Error>;
    fn peek(&mut self) -> Result<&Token, Error>;
}
