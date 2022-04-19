
use crate::token::Token;

#[derive(Debug)]
pub enum Error<'a> {
    UndefinedIdent { token: &'a Token },
    TypeCycle { token: Vec<&'a Token> },
}
