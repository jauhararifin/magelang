use std::{fmt, io};

use crate::token::Pos;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnexpectedSymbol { symbol: char, pos: Pos },
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(err) => err.fmt(f),
            Error::UnexpectedSymbol { symbol, pos } => {
                write!(f, "Unknown symbol. Found \"{}\" sybol, at {}", symbol, pos)
            }
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}
