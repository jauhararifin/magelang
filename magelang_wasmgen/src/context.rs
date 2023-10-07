use bumpalo::Bump;
use magelang_syntax::FileManager;
use magelang_typecheck::Module;

pub(crate) struct Context<'ctx, E> {
    pub(crate) arena: &'ctx Bump,
    pub(crate) files: &'ctx FileManager,
    pub(crate) errors: &'ctx E,
    pub(crate) module: &'ctx Module<'ctx>,
}

impl<'ctx, E> Clone for Context<'ctx, E> {
    fn clone(&self) -> Self {
        Self {
            arena: self.arena,
            files: self.files,
            errors: self.errors,
            module: self.module,
        }
    }
}

impl<'ctx, E> Copy for Context<'ctx, E> {}
