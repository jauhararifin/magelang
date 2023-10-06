use bumpalo::Bump;
use magelang_syntax::FileManager;
use magelang_typecheck::Module;

#[derive(Clone, Copy)]
pub(crate) struct Context<'ctx, E> {
    pub(crate) arena: &'ctx Bump,
    pub(crate) files: &'ctx FileManager,
    pub(crate) errors: &'ctx E,
    pub(crate) module: &'ctx Module<'ctx>,
}
