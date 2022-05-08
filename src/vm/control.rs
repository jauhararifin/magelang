use std::mem::replace;

use crate::bytecode::{Function, Instruction, Program};

use super::call_frame::CallFrame;

pub trait IController {
    fn fetch(&self) -> Option<Instruction>;

    fn current_base(&self) -> usize;

    fn advance(&mut self);
    fn jump(&mut self, offset: isize);
    fn call(&mut self, fn_id: u64, base_stack: usize);
    fn ret(&mut self);
}

pub struct Controller {
    functions: Vec<Function>,

    call_stack: Vec<CallFrame>,
    current_frame: CallFrame,
    is_exited: bool,
}

impl Controller {
    pub fn load(program: Program) -> Self {
        Self {
            functions: program.functions,

            call_stack: vec![],
            current_frame: CallFrame {
                func_id: program.entry_point as u64,
                instruction_index: 0,
                base_stack: 0,
            },
            is_exited: false,
        }
    }
}

impl IController for Controller {
    fn fetch(&self) -> Option<Instruction> {
        let func_id = self.current_frame.func_id;
        let instruction_index = self.current_frame.instruction_index;

        if self.is_exited {
            None
        } else {
            Some(unsafe {
                self.functions
                    .get_unchecked(func_id as usize)
                    .instructions
                    .get_unchecked(instruction_index)
                    .clone()
            })
        }
    }

    fn current_base(&self) -> usize {
        self.current_frame.base_stack
    }

    fn advance(&mut self) {
        self.current_frame.instruction_index += 1;
    }

    fn jump(&mut self, offset: isize) {
        self.current_frame.instruction_index = (self.current_frame.instruction_index as isize + offset) as usize;
    }

    fn call(&mut self, fn_id: u64, base_stack: usize) {
        let new_call_frame = CallFrame {
            func_id: fn_id,
            instruction_index: 0,
            base_stack,
        };
        self.call_stack.push(replace(&mut self.current_frame, new_call_frame));
    }

    fn ret(&mut self) {
        let last_call_stack = self.call_stack.pop();
        if last_call_stack.is_none() {
            self.is_exited = true;
            return;
        }

        self.current_frame = last_call_stack.unwrap();
        self.advance();
    }
}
