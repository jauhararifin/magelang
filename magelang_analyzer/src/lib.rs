#![feature(lazy_cell)]

mod analyze;
mod errors;
mod expr;
mod interner;
mod name;
mod path;
mod scope;
mod symbols;
mod ty;
mod value;

pub use analyze::analyze;
