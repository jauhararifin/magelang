#[derive(Clone)]
pub struct CallFrame {
    pub func_id: usize,
    pub instruction_index: usize,
    pub base_stack: usize,
}
