use super::stack::RuntimeStack;

pub trait INativeExecutor {
    fn execute_native(&self, name: &str, stack: &RuntimeStack);
}

pub struct NativeExecutor {}

impl NativeExecutor {
    pub fn new() -> Self {
        Self {}
    }
}

impl INativeExecutor for NativeExecutor {
    fn execute_native(&self, name: &str, stack: &RuntimeStack) {
        match name {
            "print_int" => self.execute_print_int(stack),
            _ => (),
        }
    }
}

impl NativeExecutor {
    fn execute_print_int(&self, stack: &RuntimeStack) {
        let arg1 = stack.values.last().unwrap();
        let v = unsafe { *(arg1.data as *const i64) };
        println!("{}", v);
    }
}
