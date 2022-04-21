use super::{error::Error, repr::Root};

pub trait Parser {
    fn parse(&mut self) -> std::result::Result<Root, Error>;
}
