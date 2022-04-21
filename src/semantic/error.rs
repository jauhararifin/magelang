use std::rc::Rc;

use crate::{ast::Expr, token::Token};

use super::semantic::Type;

#[derive(Debug)]
pub enum Error<'a> {
    UndefinedIdent { token: &'a Token },
    CannotAssignToValue { expr: &'a Expr },
    MismatchType { expected: Rc<Type>, got: Rc<Type> },
    UnsupportedGlobal, // TODO: support global variable?
    RedeclaredSymbol { symbol: &'a Token },
    CannotPerformOp { typ: Rc<Type> },
    UnsupportedCast { target: Rc<Type>, source: Rc<Type> },
    TypeCycle { token: Vec<&'a Token> },
}
