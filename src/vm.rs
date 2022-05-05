// pub struct Executor {
//     memory: HashMap<usize, Value>,
//     mem_id: usize,
//
//     value_stack: Vec<Value>,
// }

use std::{
    fmt::{Debug, Display},
    mem::size_of,
};

use crate::bytecode::{self, BitSize, Instruction, Program};

pub struct Executor {
    runtime_stack: RuntimeStack,
    call_stack: Vec<CallFrame>,
    current_frame: CallFrame,
    functions: Vec<bytecode::Function>,
    entry_point: usize,
}

#[derive(Clone)]
struct RuntimeValue {
    typ: ValueType,
    data: usize, // pointer.
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match &self.typ {
                ValueType::I64 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const i64)),
                _ => todo!(),
            }
        }
    }
}

struct CallFrame {
    func_id: usize,
    instruction_index: usize,
    base_stack: usize,
}

#[derive(Debug, Clone)]
enum ValueType {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    FnId,
}

impl ValueType {
    fn size(&self) -> usize {
        match &self {
            ValueType::Void => 0,
            ValueType::Bool => 1,
            ValueType::I8 => 1,
            ValueType::I16 => 2,
            ValueType::I32 => 4,
            ValueType::I64 => 8,
            ValueType::U8 => 1,
            ValueType::U16 => 2,
            ValueType::U32 => 4,
            ValueType::U64 => 8,
            ValueType::F32 => 4,
            ValueType::F64 => 8,
            ValueType::FnId => size_of::<usize>(),
        }
    }
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

struct RuntimeStack {
    values: Vec<RuntimeValue>,
    top_ptr: usize,
    data: Vec<u8>,
}

impl RuntimeStack {
    fn new(size: usize) -> Self {
        let data = vec![0; size];
        Self {
            values: Vec::new(),
            top_ptr: data.as_ptr() as usize,
            data,
        }
    }

    fn pop_i64(&mut self) -> i64 {
        self.top_ptr -= ValueType::I64.size();
        let val = self.values.pop().unwrap();
        unsafe { *(val.data as *const i64) }
    }

    fn clear(&mut self, count: usize) {
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

    fn get_and_push(&mut self, index: isize) {
        let val = unsafe { self.values.get_unchecked(index as usize).clone() };
        unsafe {
            std::ptr::copy::<u8>(val.data as *const u8, self.top_ptr as *mut u8, val.typ.size());
        }
        self.top_ptr += val.typ.size();
        self.values.push(val);
    }

    fn push_i64(&mut self, v: i64) {
        let ptr = self.top_ptr as *mut i64;
        unsafe { *ptr = v }

        let typ = ValueType::I64;
        self.top_ptr += typ.size();
        self.values.push(RuntimeValue {
            typ,
            data: ptr as usize,
        });
    }

    fn push_fn_id(&mut self, v: usize) {
        let ptr = self.top_ptr as *mut usize;
        unsafe { *ptr = v }

        let typ = ValueType::FnId;
        self.top_ptr += typ.size();
        self.values.push(RuntimeValue {
            typ,
            data: ptr as usize,
        });
    }

    fn push_void(&mut self) {
        let typ = ValueType::Void;
        self.values.push(RuntimeValue {
            typ,
            data: self.top_ptr,
        });
    }
}
