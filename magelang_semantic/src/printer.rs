use crate::names::*;
use crate::{Type, TypeId, TypeLoader};
use magelang_common::SymbolLoader;

pub struct TypePrinter<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

impl<'sym, 'typ> TypePrinter<'sym, 'typ> {
    pub fn new(symbol_loader: &'sym SymbolLoader, type_loader: &'typ TypeLoader) -> Self {
        Self {
            symbol_loader,
            type_loader,
        }
    }

    pub fn display_type(&self, ty: &Type) -> String {
        match ty {
            Type::Invalid => String::from(INVALID),
            Type::Void => String::from(VOID),
            Type::Isize => String::from(ISIZE),
            Type::I64 => String::from(I64),
            Type::I32 => String::from(I32),
            Type::I16 => String::from(I16),
            Type::I8 => String::from(I8),
            Type::Usize => String::from(USIZE),
            Type::U64 => String::from(U64),
            Type::U32 => String::from(U32),
            Type::U16 => String::from(U16),
            Type::U8 => String::from(U8),
            Type::F32 => String::from(F32),
            Type::F64 => String::from(F64),
            Type::Bool => String::from(BOOL),
            Type::Func(func_type) => {
                let mut s = String::from(FUNC);
                if !func_type.type_parameters.is_empty() {
                    s.push('[');
                    for (i, type_param_id) in func_type.type_parameters.iter().enumerate() {
                        if i > 0 {
                            s.push(',');
                        }
                        s.push_str(self.symbol_loader.get_symbol(*type_param_id).unwrap().as_ref());
                    }
                    s.push(']');
                }
                s.push('(');
                for (i, param) in func_type.parameters.iter().enumerate() {
                    if i > 0 {
                        s.push(',');
                    }
                    s.push_str(&self.display(*param));
                }
                s.push(')');
                if let Some(ref ret_type) = func_type.return_type {
                    s.push(':');
                    s.push_str(&self.display(*ret_type));
                }
                s
            }
            Type::Slice(slice_type) => {
                format!("[]{}", self.display(slice_type.element_type))
            }
            Type::Pointer(pointer_type) => {
                format!("*{}", self.display(pointer_type.element_type))
            }
            Type::ArrayPtr(array_ptr_type) => {
                format!("[*]{}", self.display(array_ptr_type.element_type))
            }
            Type::Opaque(name) => self.symbol_loader.get_symbol(*name).unwrap().to_string(),
        }
    }

    pub fn display(&self, type_id: TypeId) -> String {
        let ty = self.type_loader.get_type(type_id).unwrap();
        self.display_type(ty.as_ref())
    }
}
