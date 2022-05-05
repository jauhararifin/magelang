use crate::bytecode::{self, BitSize, Instruction, Program};

use super::{native::{INativeExecutor, NativeExecutor}, stack::RuntimeStack, errors::Error};

pub struct Executor {
    runtime_stack: RuntimeStack,
    call_stack: Vec<CallFrame>,
    current_frame: CallFrame,
    is_exited: bool,
    functions: Vec<bytecode::Function>,

    native_executor: Box<dyn INativeExecutor>,
}

#[derive(Clone)]
struct CallFrame {
    func_id: usize,
    instruction_index: usize,
    base_stack: usize,
}

impl Executor {
    pub fn load(program: Program) -> Result<Self, Error> {
        Ok(Self {
            runtime_stack: RuntimeStack::allocate(16 * 1024)?,
            call_stack: vec![],
            current_frame: CallFrame {
                func_id: program.entry_point,
                instruction_index: 0,
                base_stack: 0,
            },
            is_exited: false,
            functions: program.functions,

            native_executor: Box::new(NativeExecutor::new()),
        })
    }

    pub fn run(&mut self) {
        while !self.is_exited {
            let func_id = self.current_frame.func_id;
            let instruction_index = self.current_frame.instruction_index;

            let instruction = unsafe {
                self.functions
                    .get_unchecked(func_id)
                    .instructions
                    .get_unchecked(instruction_index)
                    .clone()
            };

            // println!(
            //     "{} {:?}@{:?}",
            //     self.functions.get(func_id).unwrap().name,
            //     func_id,
            //     instruction_index
            // );
            // println!("executing: {:?}", instruction);

            let advance = self.execute_instruction(instruction);

            // println!("stack (base={}):", self.current_frame.base_stack);
            // for val in self.runtime_stack.values.iter() {
            //     println!("{:?}", val);
            // }
            // println!("===================================");

            if advance {
                self.current_frame.instruction_index += 1;
            }
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> bool {
        match instruction {
            Instruction::Nop => true,
            Instruction::Constant(val) => self.execute_constant(val),
            Instruction::GetLocal(offset) => self.execute_get_local(offset),
            Instruction::SetLocal(offset) => self.execute_set_local(offset),
            Instruction::Mod(variant) => self.execute_mod(variant),
            Instruction::NEq(variant) => self.execute_neq(variant),
            Instruction::Pop(count) => self.execute_pop(count),
            Instruction::Jump(offset) => self.execute_jump(offset),
            Instruction::JumpIfFalse(offset) => self.execute_jump_if_false(offset),
            Instruction::Call => self.execute_call(),
            Instruction::Ret => self.execute_ret(),
            Instruction::CallNative(func) => self.execute_call_native(func.as_ref()),
            _ => todo!("{:?}", instruction),
        }
    }

    fn execute_constant(&mut self, val: bytecode::Value) -> bool {
        match val {
            bytecode::Value::Void => self.runtime_stack.push_primitive(()),
            bytecode::Value::Bool(_) => todo!(),
            bytecode::Value::I8(_) => todo!(),
            bytecode::Value::I16(_) => todo!(),
            bytecode::Value::I32(_) => todo!(),
            bytecode::Value::I64(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::U8(_) => todo!(),
            bytecode::Value::U16(_) => todo!(),
            bytecode::Value::U32(_) => todo!(),
            bytecode::Value::U64(_) => todo!(),
            bytecode::Value::F32(_) => todo!(),
            bytecode::Value::F64(_) => todo!(),
            bytecode::Value::FnId(val) => self.runtime_stack.push_fn_id(val),
        }
        true
    }

    fn execute_get_local(&mut self, offset: isize) -> bool {
        self.runtime_stack
            .get_and_push(self.current_frame.base_stack as isize + offset);
        true
    }

    fn execute_neq(&mut self, variant: BitSize) -> bool {
        match variant {
            BitSize::I8 => todo!(),
            BitSize::I16 => todo!(),
            BitSize::I32 => todo!(),
            BitSize::I64 => {
                let b: i64 = self.runtime_stack.pop_value();
                let a: i64 = self.runtime_stack.pop_value();
                self.runtime_stack.push_primitive(a != b);
            }
            BitSize::U8 => todo!(),
            BitSize::U16 => todo!(),
            BitSize::U32 => todo!(),
            BitSize::U64 => todo!(),
            BitSize::F32 => todo!(),
            BitSize::F64 => todo!(),
        }
        true
    }

    fn execute_mod(&mut self, variant: BitSize) -> bool {
        match variant {
            BitSize::I8 => todo!(),
            BitSize::I16 => todo!(),
            BitSize::I32 => todo!(),
            BitSize::I64 => {
                let b: i64 = self.runtime_stack.pop_value();
                let a: i64 = self.runtime_stack.pop_value();
                self.runtime_stack.push_primitive(a % b);
            }
            BitSize::U8 => todo!(),
            BitSize::U16 => todo!(),
            BitSize::U32 => todo!(),
            BitSize::U64 => todo!(),
            BitSize::F32 => todo!(),
            BitSize::F64 => todo!(),
        }
        true
    }

    fn execute_jump_if_false(&mut self, offset: isize) -> bool {
        let v: bool = self.runtime_stack.pop_value();
        if !v {
            self.current_frame.instruction_index = (self.current_frame.instruction_index as isize + offset) as usize;
            false
        } else {
            true
        }
    }

    fn execute_jump(&mut self, offset: isize) -> bool {
        self.current_frame.instruction_index = (self.current_frame.instruction_index as isize + offset) as usize;
        false
    }

    fn execute_set_local(&mut self, offset: isize) -> bool {
        self.runtime_stack
            .pop_and_set(self.current_frame.base_stack as isize + offset);
        true
    }

    fn execute_pop(&mut self, count: usize) -> bool {
        self.runtime_stack.pop(count);
        true
    }

    fn execute_call(&mut self) -> bool {
        let fn_id: usize = self.runtime_stack.pop_value();
        self.call_stack.push(self.current_frame.clone());
        self.current_frame = CallFrame {
            func_id: fn_id,
            instruction_index: 0,
            base_stack: self.runtime_stack.top_index(),
        };
        false
    }

    fn execute_ret(&mut self) -> bool {
        let last_call_stack = self.call_stack.pop();
        if last_call_stack.is_none() {
            self.is_exited = true;
            return true;
        }

        self.current_frame = last_call_stack.unwrap();
        true
    }

    fn execute_call_native(&mut self, native_func: &String) -> bool {
        self.native_executor.execute_native(native_func.as_str(), &self.runtime_stack);
        true
    }
}
