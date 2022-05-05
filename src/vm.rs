use std::collections::HashMap;

use crate::bytecode::{Instruction, Program, Value, ValueKind};

pub struct Executor {
    memory: HashMap<usize, Value>,
    mem_id: usize,

    value_stack: Vec<Value>,
}

struct InstructionContext {
    base: usize,
    memory_id: usize,
    instruction_idx: usize,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            memory: HashMap::new(),
            mem_id: 0,
            value_stack: Vec::new(),
        }
    }
}

macro_rules! num_binary  {
    ($self:expr,$op:tt) => {
        let a = $self.value_stack.pop().unwrap();
        let b = $self.value_stack.pop().unwrap();
        $self.value_stack.push(Value {
            kind: match (a.kind, b.kind) {
                (ValueKind::I8(a), ValueKind::I8(b)) => ValueKind::I8(a $op b),
                (ValueKind::I16(a), ValueKind::I16(b)) => ValueKind::I16(a $op b),
                (ValueKind::I32(a), ValueKind::I32(b)) => ValueKind::I32(a $op b),
                (ValueKind::I64(a), ValueKind::I64(b)) => ValueKind::I64(a $op b),
                (ValueKind::U8(a), ValueKind::U8(b)) => ValueKind::U8(a $op b),
                (ValueKind::U16(a), ValueKind::U16(b)) => ValueKind::U16(a $op b),
                (ValueKind::U32(a), ValueKind::U32(b)) => ValueKind::U32(a $op b),
                (ValueKind::U64(a), ValueKind::U64(b)) => ValueKind::U64(a $op b),
                (ValueKind::F32(a), ValueKind::F32(b)) => ValueKind::F32(a $op b),
                (ValueKind::F64(a), ValueKind::F64(b)) => ValueKind::F64(a $op b),
                _ => todo!(),
            },
        });
    };
}

macro_rules! int_binary  {
    ($self:expr,$op:tt) => {
        let a = $self.value_stack.pop().unwrap();
        let b = $self.value_stack.pop().unwrap();
        $self.value_stack.push(Value {
            kind: match (a.kind, b.kind) {
                (ValueKind::I8(a), ValueKind::I8(b)) => ValueKind::I8(a $op b),
                (ValueKind::I16(a), ValueKind::I16(b)) => ValueKind::I16(a $op b),
                (ValueKind::I32(a), ValueKind::I32(b)) => ValueKind::I32(a $op b),
                (ValueKind::I64(a), ValueKind::I64(b)) => ValueKind::I64(a $op b),
                (ValueKind::U8(a), ValueKind::U8(b)) => ValueKind::U8(a $op b),
                (ValueKind::U16(a), ValueKind::U16(b)) => ValueKind::U16(a $op b),
                (ValueKind::U32(a), ValueKind::U32(b)) => ValueKind::U32(a $op b),
                (ValueKind::U64(a), ValueKind::U64(b)) => ValueKind::U64(a $op b),
                _ => todo!(),
            },
        });
    };
}

impl Executor {
    pub fn execute(&mut self, program: &Program) {
        for val in program.values.iter() {
            self.add_value(val.clone());
        }

        let entry_ins = self.memory.get(&program.entry_point).unwrap();
        let entry_ins = if let ValueKind::Fn(instruction) = &entry_ins.kind {
            instruction
        } else {
            todo!();
        };

        let mut instruction_ptr: (&[Instruction], usize) = (entry_ins, 0); // (value_ref, index)
        let mut base_stack: usize = 0;
        let mut curr_func_id: usize = program.entry_point;
        let mut instruction_stack: Vec<InstructionContext> = Vec::new();

        loop {
            if instruction_ptr.0.len() <= instruction_ptr.1 {
                break;
            }
            let ins = &instruction_ptr.0[instruction_ptr.1];

            // println!("{:?} {:?}", curr_func_id, instruction_ptr.1);
            // println!("base stack: {}", base_stack);
            // println!("{:?}", ins);

            match ins {
                Instruction::Constant(val) => {
                    self.value_stack.push(val.clone());
                }
                Instruction::Add => {
                    num_binary!(self, +);
                }
                Instruction::Sub => {
                    num_binary!(self, -);
                }
                Instruction::Div => {
                    num_binary!(self, /);
                }
                Instruction::Mul => {
                    num_binary!(self, *);
                }
                Instruction::Mod => {
                    int_binary!(self, %);
                }
                Instruction::Shl => {
                    int_binary!(self, <<);
                }
                Instruction::Shr => {
                    int_binary!(self, >>);
                }
                Instruction::Eq => {
                    let a = self.value_stack.pop().unwrap();
                    let b = self.value_stack.pop().unwrap();
                    self.value_stack.push(Value {
                        kind: ValueKind::Bool(a.kind == b.kind),
                    });
                }
                Instruction::NEq => {
                    let a = self.value_stack.pop().unwrap();
                    let b = self.value_stack.pop().unwrap();
                    self.value_stack.push(Value {
                        kind: ValueKind::Bool(a.kind != b.kind),
                    });
                }
                Instruction::LT => todo!(),
                Instruction::LTEq => todo!(),
                Instruction::GT => todo!(),
                Instruction::GTEq => todo!(),
                Instruction::Not => todo!(),
                Instruction::And => todo!(),
                Instruction::Or => todo!(),
                Instruction::Xor => todo!(),
                Instruction::Neg => todo!(),
                Instruction::Alloc(val) => todo!(),
                Instruction::SetLocal(offset) => {
                    let val = self.value_stack.pop().unwrap();
                    self.value_stack[(base_stack as isize + offset) as usize] = val;
                }
                Instruction::GetLocal(offset) => {
                    let v = self.value_stack[(base_stack as isize + offset) as usize].clone();
                    self.value_stack.push(v);
                }
                Instruction::SetGlobal(idx) => todo!(),
                Instruction::GetGlobal(idx) => {
                    let value = self.memory.get(idx).unwrap();
                    let value = if let ValueKind::Fn(_) = &value.kind {
                        Value {
                            kind: ValueKind::Ptr(idx.clone()),
                        }
                    } else {
                        Value {
                            kind: value.kind.clone(),
                        }
                    };
                    self.value_stack.push(value);
                }
                Instruction::GetProp(idx) => todo!(),
                Instruction::SetProp(idx) => todo!(),
                Instruction::Jump(offset) => {
                    instruction_ptr.1 = ((instruction_ptr.1 as isize) + offset) as usize;
                    continue;
                },
                Instruction::JumpIfTrue(offset) => todo!(),
                Instruction::JumpIfFalse(offset) => {
                    let val = self.value_stack.pop().unwrap();
                    if let ValueKind::Bool(v) = &val.kind {
                        if !*v {
                            instruction_ptr.1 = ((instruction_ptr.1 as isize) + offset) as usize;
                            continue;
                        }
                    } else {
                        todo!();
                    };
                }
                Instruction::Pop(num) => {
                    let len = self.value_stack.len() - num;
                    self.value_stack.resize(
                        len,
                        Value {
                            kind: ValueKind::Void,
                        },
                    );
                }
                Instruction::Call(n_args) => {
                    let fn_ptr = self.value_stack.pop().unwrap();

                    let fn_ptr = if let ValueKind::Ptr(mem_id) = &fn_ptr.kind {
                        mem_id
                    } else {
                        todo!();
                    };

                    let fn_value = self.memory.get(fn_ptr).unwrap();
                    if let ValueKind::Fn(instructions) = &fn_value.kind {
                        let ctx = InstructionContext {
                            base: base_stack.clone(),
                            memory_id: curr_func_id.clone(),
                            instruction_idx: instruction_ptr.1.clone(),
                        };
                        instruction_stack.push(ctx);

                        instruction_ptr = (&instructions[..], 0);
                        base_stack = self.value_stack.len() - n_args;
                        curr_func_id = fn_ptr.clone();

                        continue;
                    } else {
                        todo!();
                    }
                }
                Instruction::Ret => {
                    let ctx = instruction_stack.pop();
                    if ctx.is_none() {
                        break;
                    }
                    let ctx = ctx.unwrap();

                    let val = self.value_stack.pop().unwrap(); // return value
                    self.value_stack.resize(
                        base_stack,
                        Value {
                            kind: ValueKind::Void,
                        },
                    ); // clearing the stack.
                    self.value_stack.push(val); // put the return value.

                    let fn_value = self.memory.get(&ctx.memory_id).unwrap();
                    let instructions = if let ValueKind::Fn(instructions) = &fn_value.kind {
                        &instructions[..]
                    } else {
                        todo!();
                    };

                    instruction_ptr = (instructions, ctx.instruction_idx);
                    base_stack = ctx.base;
                    curr_func_id = ctx.memory_id;
                }
                Instruction::CallNative(name) => {
                    self.run_native_function(name.as_str());
                },
            }

            // println!("{:?}", self.value_stack);
            // println!("=======================================");

            instruction_ptr.1 += 1;
        }
    }

    fn add_value(&mut self, value: Value) {
        self.memory.insert(self.mem_id, value);
        self.mem_id += 1;
    }

    fn run_native_function(&self, name: &str) {
        match name {
            "print_int" => {
                let val = self.value_stack.last().unwrap();
                if let ValueKind::I64(v) = val.kind {
                    println!("{}", v);
                }
            },
            _ => todo!(),
        }
    }
}
