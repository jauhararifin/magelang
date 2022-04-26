use super::{program::Program, value::Value};

pub struct Runner {
    program: Program,
    objects: Vec<Box<Value>>,
    stack: Vec<Value>,

    gc_obj: usize, // object count after last gc.
}

impl Runner {
    fn new(program: Program) -> Self {
        Self {
            program,
            objects: Vec::new(),
            stack: Vec::new(),

            gc_obj: 0,
        }
    }

    
}
