use std::mem::size_of;

use super::value::{IntoValueType, RuntimeValue, ValueType};

pub struct RuntimeStack {
    pub values: Vec<RuntimeValue>,
    pub top_ptr: usize,
    pub data: Vec<u8>,
}

impl RuntimeStack {
    pub fn new(size: usize) -> Self {
        let data = vec![0; size];
        Self {
            values: Vec::new(),
            top_ptr: data.as_ptr() as usize,
            data,
        }
    }

    pub fn top_index(&self) -> usize {
        self.values.len()
    }

    pub fn push_primitive<T: IntoValueType>(&mut self, v: T) {
        self.push_value(T::into_value_type(), v);
    }

    pub fn push_fn_id(&mut self, v: usize) {
        self.push_value(ValueType::FnId, v);
    }

    pub fn push_value<T>(&mut self, typ: ValueType, concrete_value: T) {
        self.values.push(RuntimeValue {
            typ,
            data: self.top_ptr,
        });

        let ptr = self.top_ptr as *mut T;
        unsafe { *ptr = concrete_value }
        self.top_ptr += size_of::<T>();
    }

    pub fn pop_value<T: Copy>(&mut self) -> T {
        let val = self.values.pop().unwrap();
        self.top_ptr -= val.typ.size();
        unsafe { *(val.data as *const T) }
    }

    pub fn pop(&mut self, count: usize) {
        self.values.resize(
            self.values.len() - count,
            RuntimeValue {
                typ: ValueType::Void,
                data: 0,
            },
        );

        if let Some(val) = self.values.last() {
            self.top_ptr = val.data + val.typ.size();
        } else {
            self.top_ptr = self.data.as_ptr() as usize;
        }
    }

    pub fn get_and_push(&mut self, index: isize) {
        let mut val = unsafe { self.values.get_unchecked(index as usize).clone() };
        unsafe {
            std::ptr::copy::<u8>(val.data as *const u8, self.top_ptr as *mut u8, val.typ.size());
        }
        val.data = self.top_ptr;
        self.top_ptr += val.typ.size();
        self.values.push(val);
    }

    pub fn pop_and_set(&mut self, index: isize) {
        let source = self.values.pop().unwrap();
        let size = source.typ.size();
        self.top_ptr -= size;
        let target = unsafe { self.values.get_unchecked(index as usize) };
        unsafe {
            std::ptr::copy::<u8>(source.data as *const u8, target.data as *mut u8, size);
        }
    }
}
