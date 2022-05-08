
pub trait IMemoryManager {}

pub struct MemoryManager {
    objects: Vec<usize>, // list of pointer to objects.
}

impl MemoryManager {
    pub fn new() -> Self {
        Self{objects: Vec::new()}
    }
}
