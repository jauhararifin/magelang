use crate::context::Context;
use magelang_typecheck::{DefId, Func, Type, TypeKind};

pub(crate) trait Mangle<'a> {
    fn get_mangled_name<E>(&self, ctx: &Context<'a, E>) -> &'a str;
}

impl<'ctx> Mangle<'ctx> for DefId<'ctx> {
    fn get_mangled_name<E>(&self, ctx: &Context<'ctx, E>) -> &'ctx str {
        ctx.arena.alloc_str(&format!("{}", self))
    }
}

impl<'ctx> Mangle<'ctx> for Func<'ctx> {
    fn get_mangled_name<E>(&self, ctx: &Context<'ctx, E>) -> &'ctx str {
        let mut result = format!("{}", self.name);

        if let Some(typeargs) = self.typeargs {
            result.push('<');
            for (i, arg) in typeargs.iter().enumerate() {
                if i > 0 {
                    result.push(',');
                }
                result.push_str(arg.get_mangled_name(ctx));
            }
            result.push('>');
        }

        ctx.arena.alloc_str(&result)
    }
}

impl<'ctx> Mangle<'ctx> for Type<'ctx> {
    fn get_mangled_name<E>(&self, ctx: &Context<'ctx, E>) -> &'ctx str {
        match self.kind {
            TypeKind::User(ty) => ty.def_id.get_mangled_name(ctx),
            TypeKind::Inst(ty) => {
                let mut result = format!("{}", ty.def_id);

                result.push('<');
                for (i, arg) in ty.type_args.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(arg.get_mangled_name(ctx));
                }
                result.push('>');

                ctx.arena.alloc_str(&result)
            }
            TypeKind::Generic(..) => unreachable!("found un-monomorphized type"),
            TypeKind::Anonymous => ctx.arena.alloc_str(&format!("{}", self.repr)),
        }
    }
}
