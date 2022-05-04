use crate::{ast::RootNode, errors::Error, lexer::ILexer};

pub trait IParser {
    fn parse(&mut self) -> std::result::Result<RootNode, Error>;
}

pub struct Parser<T: ILexer> {
    lexer: T,
}

impl<L: ILexer> Parser<L> {
    pub fn new(lexer: L) -> Self {
        Self { lexer }
    }
}
