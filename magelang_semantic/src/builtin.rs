use crate::scope::{Object, Scope, ScopeKind};
use crate::symbol::SymbolId;
use crate::ty::{FloatType, IntType, Type, TypeDb};
use indexmap::IndexMap;
use std::rc::Rc;

pub const ISIZE: &str = "isize";
pub const I64: &str = "i64";
pub const I32: &str = "i32";
pub const I16: &str = "i16";
pub const I8: &str = "i8";
pub const USIZE: &str = "usize";
pub const U64: &str = "u64";
pub const U32: &str = "u32";
pub const U16: &str = "u16";
pub const U8: &str = "u8";
pub const BOOL: &str = "bool";
pub const F64: &str = "f64";
pub const F32: &str = "f32";
pub const VOID: &str = "void";

pub fn get_builtin_scope(db: &impl TypeDb) -> Rc<Scope> {
    let mut symbols = IndexMap::<SymbolId, Object>::new();

    let isize_ty = Type::Int(IntType::isize());
    let i8_ty = Type::Int(IntType::i8());
    let i16_ty = Type::Int(IntType::i16());
    let i32_ty = Type::Int(IntType::i32());
    let i64_ty = Type::Int(IntType::i64());
    let usize_ty = Type::Int(IntType::usize());
    let u8_ty = Type::Int(IntType::u8());
    let u16_ty = Type::Int(IntType::u16());
    let u32_ty = Type::Int(IntType::u32());
    let u64_ty = Type::Int(IntType::u64());

    symbols.insert(
        db.define_symbol(ISIZE.into()),
        Object::Type(db.define_type(Rc::new(isize_ty))),
    );
    symbols.insert(
        db.define_symbol(I64.into()),
        Object::Type(db.define_type(Rc::new(i64_ty))),
    );
    symbols.insert(
        db.define_symbol(I32.into()),
        Object::Type(db.define_type(Rc::new(i32_ty))),
    );
    symbols.insert(
        db.define_symbol(I16.into()),
        Object::Type(db.define_type(Rc::new(i16_ty))),
    );
    symbols.insert(
        db.define_symbol(I8.into()),
        Object::Type(db.define_type(Rc::new(i8_ty))),
    );
    symbols.insert(
        db.define_symbol(USIZE.into()),
        Object::Type(db.define_type(Rc::new(usize_ty))),
    );
    symbols.insert(
        db.define_symbol(U64.into()),
        Object::Type(db.define_type(Rc::new(u64_ty))),
    );
    symbols.insert(
        db.define_symbol(U32.into()),
        Object::Type(db.define_type(Rc::new(u32_ty))),
    );
    symbols.insert(
        db.define_symbol(U16.into()),
        Object::Type(db.define_type(Rc::new(u16_ty))),
    );
    symbols.insert(
        db.define_symbol(U8.into()),
        Object::Type(db.define_type(Rc::new(u8_ty))),
    );

    symbols.insert(
        db.define_symbol(BOOL.into()),
        Object::Type(db.define_type(Type::Bool.into())),
    );
    symbols.insert(
        db.define_symbol(F64.into()),
        Object::Type(db.define_type(Type::Float(FloatType::f64()).into())),
    );
    symbols.insert(
        db.define_symbol(F32.into()),
        Object::Type(db.define_type(Type::Float(FloatType::f32()).into())),
    );
    symbols.insert(
        db.define_symbol(VOID.into()),
        Object::Type(db.define_type(Rc::new(Type::Void))),
    );

    Scope::new(ScopeKind::Basic, symbols)
}
