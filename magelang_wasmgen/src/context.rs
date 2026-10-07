use bumpalo::Bump;
use magelang_syntax::{ErrorManager, FileManager};
use magelang_typecheck::Module;

#[derive(Clone, Copy)]
pub(crate) struct Context<'ctx> {
    pub(crate) arena: &'ctx Bump,
    pub(crate) files: &'ctx FileManager,
    pub(crate) errors: &'ctx ErrorManager,
    pub(crate) module: &'ctx Module<'ctx>,
}
