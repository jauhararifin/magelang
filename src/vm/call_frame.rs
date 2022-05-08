#[derive(Clone)]
pub struct CallFrame {
    pub func_id: u64,
    pub instruction_index: usize,
    pub base_stack: usize,
}
