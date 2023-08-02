use crate::analyze::TypeCheckContext;
use crate::symbols::SymbolId;
use crate::ty::{display_type_id, TypeArgsId};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DefId {
    pub package: SymbolId,
    pub name: SymbolId,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Name {
    Def(DefId),
    Instance(DefId, TypeArgsId),
}

pub fn display_name<E>(ctx: &TypeCheckContext<'_, E>, name: &Name) -> String {
    match name {
        Name::Def(def_id) => format!(
            "{}.{}",
            ctx.symbols.get(def_id.package),
            ctx.symbols.get(def_id.name)
        ),
        Name::Instance(def_id, typeargs_id) => {
            let type_args = ctx.typeargs.get(*typeargs_id);
            let mut s = format!(
                "{}.{}",
                ctx.symbols.get(def_id.package),
                ctx.symbols.get(def_id.name)
            );
            s.push('<');
            for type_id in type_args.iter() {
                s.push_str(&display_type_id(ctx, *type_id));
            }
            s.push('>');
            s
        }
    }
}
