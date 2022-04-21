use std::rc::Rc;

use crate::{
    ast::Expr,
    token::{Pos, Token},
};

use super::repr::Type;

#[derive(Debug)]
pub enum Error<'a> {
    UndefinedIdent {
        token: &'a Token,
    },
    CannotAssignToValue {
        expr: &'a Expr,
        pos: &'a Pos,
    },
    MismatchType {
        expected: Rc<Type>,
        got: Rc<Type>,
        pos: &'a Pos,
    },
    UnsupportedGlobal, // TODO: support global variable?
    RedeclaredSymbol {
        symbol: &'a Token,
    },
    CannotPerformOp {
        typ: Rc<Type>,
        pos: &'a Pos,
    },
    UnsupportedCast {
        target: Rc<Type>,
        source: Rc<Type>,
        pos: &'a Pos,
    },
    TypeCycle {
        token: Vec<&'a Token>,
    },
}
