use std::alloc::{alloc, Layout};

use super::value::{ArrayType, Local, Type};

pub trait IMemoryManager {
    fn alloc_array(&mut self, elem_type: Type, count: u64) -> Local;
}

pub struct MemoryManager {
    objects: Vec<usize>, // list of pointer to objects.
}

impl MemoryManager {
    pub fn new() -> Self {
        Self { objects: Vec::new() }
    }
}

impl IMemoryManager for MemoryManager {
    fn alloc_array(&mut self, elem_type: Type, count: u64) -> Local {
        let layout = Layout::array::<u64>(10).unwrap();
        let data = unsafe { alloc(layout) } as usize;
        self.objects.push(data);

        let size = elem_type.size() * count;
        Local {
            typ: Type::Array(ArrayType {
                size,
                dims: vec![count],
                elem: Box::new(elem_type),
            }),
            data,
        }
    }
}
