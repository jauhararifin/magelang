use super::{ast::Root, error::Error};

pub trait Parser {
    fn parse(&mut self) -> std::result::Result<Root, Error>;
}
