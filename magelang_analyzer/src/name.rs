pub struct SymbolId(usize);

pub struct DefId {
    package: SymbolId,
    name: SymbolId,
}

pub struct TypeArgsId(usize);

pub enum Name {
    Def(DefId),
    Instance(DefId, TypeArgsId),
}

