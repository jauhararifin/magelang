mod analyze;
mod errors;
mod expr;
mod interner;
mod name;
mod path;
mod scope;
mod statements;
mod symbols;
mod tree_ir;
mod ty;
mod value;

pub use analyze::analyze;
pub use tree_ir::*;
