use crate::bytecode::{self, Program, Instruction, BitSize};

use super::stack::RuntimeStack;

pub struct Executor {
    runtime_stack: RuntimeStack,
    call_stack: Vec<CallFrame>,
    current_frame: CallFrame,
    functions: Vec<bytecode::Function>,
    entry_point: usize,
}

struct CallFrame {
    func_id: usize,
    instruction_index: usize,
    base_stack: usize,
}

impl Executor {
    pub fn load(program: Program) -> Self {
        Self {
            runtime_stack: RuntimeStack::new(16 * 1024),
            call_stack: vec![],
            current_frame: CallFrame {
                func_id: program.entry_point,
                instruction_index: 0,
                base_stack: 0,
            },
            functions: program.functions,
            entry_point: program.entry_point,
        }
    }

    pub fn run(&mut self) {
        loop {
            let func_id = self.current_frame.func_id;
            let instruction_index = self.current_frame.instruction_index;

            let instruction = unsafe {
                self.functions
                    .get_unchecked(func_id)
                    .instructions
                    .get_unchecked(instruction_index)
                    .clone()
            };

            println!("executing: {:?}", instruction);

            let advance = self.execute_instruction(instruction);

            println!("stack:");
            for val in self.runtime_stack.values.iter() {
                println!("{:?}", val);
            }
            println!("===================================");

            if advance {
                self.current_frame.instruction_index += 1;
            }
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> bool {
        match instruction {
            Instruction::Nop => (),
            Instruction::Constant(val) => self.execute_constant(val),
            Instruction::GetLocal(offset) => self.execute_get_local(offset),
            Instruction::Mod(variant) => self.execute_mod(variant),
            Instruction::Pop(count) => self.execute_pop(count),
            _ => todo!(),
        }

        true
    }

    fn execute_constant(&mut self, val: bytecode::Value) {
        match val {
            bytecode::Value::Void => self.runtime_stack.push_void(),
            bytecode::Value::Bool(_) => todo!(),
            bytecode::Value::I8(_) => todo!(),
            bytecode::Value::I16(_) => todo!(),
            bytecode::Value::I32(_) => todo!(),
            bytecode::Value::I64(val) => self.runtime_stack.push_i64(val),
            bytecode::Value::U8(_) => todo!(),
            bytecode::Value::U16(_) => todo!(),
            bytecode::Value::U32(_) => todo!(),
            bytecode::Value::U64(_) => todo!(),
            bytecode::Value::F32(_) => todo!(),
            bytecode::Value::F64(_) => todo!(),
            bytecode::Value::FnId(val) => self.runtime_stack.push_fn_id(val),
        }
    }

    fn execute_get_local(&mut self, offset: isize) {
        self.runtime_stack
            .get_and_push(self.current_frame.base_stack as isize + offset);
    }

    fn execute_mod(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => todo!(),
            BitSize::I16 => todo!(),
            BitSize::I32 => todo!(),
            BitSize::I64 => {
                let b = self.runtime_stack.pop_i64();
                let a = self.runtime_stack.pop_i64();
                self.runtime_stack.push_i64(a % b);
            }
            BitSize::U8 => todo!(),
            BitSize::U16 => todo!(),
            BitSize::U32 => todo!(),
            BitSize::U64 => todo!(),
            BitSize::F32 => todo!(),
            BitSize::F64 => todo!(),
        }
    }

    fn execute_pop(&mut self, count: usize) {
        self.runtime_stack.clear(count);
    }
}
