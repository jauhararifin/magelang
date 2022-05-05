use super::value::{RuntimeValue, ValueType};

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

    pub fn pop_i64(&mut self) -> i64 {
        self.top_ptr -= ValueType::I64.size();
        let val = self.values.pop().unwrap();
        unsafe { *(val.data as *const i64) }
    }

    pub fn clear(&mut self, count: usize) {
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
        let val = unsafe { self.values.get_unchecked(index as usize).clone() };
        unsafe {
            std::ptr::copy::<u8>(val.data as *const u8, self.top_ptr as *mut u8, val.typ.size());
        }
        self.top_ptr += val.typ.size();
        self.values.push(val);
    }

    pub fn push_i64(&mut self, v: i64) {
        let ptr = self.top_ptr as *mut i64;
        unsafe { *ptr = v }

        let typ = ValueType::I64;
        self.top_ptr += typ.size();
        self.values.push(RuntimeValue {
            typ,
            data: ptr as usize,
        });
    }

    pub fn push_fn_id(&mut self, v: usize) {
        let ptr = self.top_ptr as *mut usize;
        unsafe { *ptr = v }

        let typ = ValueType::FnId;
        self.top_ptr += typ.size();
        self.values.push(RuntimeValue {
            typ,
            data: ptr as usize,
        });
    }

    pub fn push_void(&mut self) {
        let typ = ValueType::Void;
        self.values.push(RuntimeValue {
            typ,
            data: self.top_ptr,
        });
    }
}
