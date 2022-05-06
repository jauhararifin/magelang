use std::cmp::PartialEq;
use std::ops::{Add, Div, Mul, Rem, Shl, Shr, Sub, BitAnd, BitOr, BitXor, Not};

use crate::bytecode::{self, BitSize, Instruction, Program};

use super::{
    control::{Controller, IController},
    errors::Error,
    native::{INativeExecutor, NativeExecutor},
    stack::RuntimeStack,
    value::IntoValueType,
};

pub struct Executor {
    runtime_stack: RuntimeStack,

    controller: Box<dyn IController>,
    native_executor: Box<dyn INativeExecutor>,
}

impl Executor {
    pub fn load(program: Program) -> Result<Self, Error> {
        let controller = Box::new(Controller::load(program));
        Ok(Self {
            runtime_stack: RuntimeStack::allocate(16 * 1024)?,
            controller,
            native_executor: Box::new(NativeExecutor::new()),
        })
    }

    pub fn run(&mut self) {
        while let Some(instruction) = self.controller.fetch() {
            // println!(
            //     "{} {:?}@{:?}",
            //     self.functions.get(func_id).unwrap().name,
            //     func_id,
            //     instruction_index
            // );
            // println!("executing: {:?}", instruction);

            match instruction {
                Instruction::Nop => self.controller.advance(),
                Instruction::Constant(val) => self.execute_constant(val),

                Instruction::Add(variant) => self.execute_add(variant),
                Instruction::Sub(variant) => self.execute_sub(variant),
                Instruction::Div(variant) => self.execute_mul(variant),
                Instruction::Mul(variant) => self.execute_div(variant),
                Instruction::Mod(variant) => self.execute_mod(variant),
                Instruction::Shl(variant) => self.execute_shl(variant),
                Instruction::Shr(variant) => self.execute_shr(variant),
                Instruction::Eq(variant) => self.execute_eq(variant),
                Instruction::NEq(variant) => self.execute_neq(variant),
                Instruction::LT(variant) => self.execute_lt(variant),
                Instruction::LTEq(variant) => self.execute_lteq(variant),
                Instruction::GT(variant) => self.execute_gt(variant),
                Instruction::GTEq(variant) => self.execute_gteq(variant),
                Instruction::Not(variant) => self.execute_bit_not(variant),
                Instruction::And(variant) => self.execute_bit_and(variant),
                Instruction::Or(variant) => self.execute_bit_or(variant),
                Instruction::Xor(variant) => self.execute_bit_xor(variant),

                Instruction::SetLocal(offset) => self.execute_set_local(offset),
                Instruction::GetLocal(offset) => self.execute_get_local(offset),

                Instruction::Jump(offset) => self.execute_jump(offset),
                Instruction::JumpIfFalse(offset) => self.execute_jump_if_false(offset),
                Instruction::JumpIfTrue(offset) => self.execute_jump_if_true(offset),

                Instruction::Pop(count) => self.execute_pop(count),

                Instruction::Call => self.execute_call(),
                Instruction::Ret => self.execute_ret(),

                Instruction::CallNative(func) => self.execute_call_native(func.as_ref()),
            }

            // println!("stack (base={}):", self.current_frame.base_stack);
            // for val in self.runtime_stack.values.iter() {
            //     println!("{:?}", val);
            // }
            // println!("===================================");

            // if advance {
            //     self.current_frame.instruction_index += 1;
            // }
        }
    }

    fn execute_constant(&mut self, val: bytecode::Value) {
        match val {
            bytecode::Value::Void => self.runtime_stack.push_primitive(()),
            bytecode::Value::Bool(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::I8(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::I16(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::I32(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::I64(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::U8(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::U16(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::U32(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::U64(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::F32(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::F64(val) => self.runtime_stack.push_primitive(val),
            bytecode::Value::FnId(val) => self.runtime_stack.push_fn_id(val),
        }
        self.controller.advance();
    }

    fn execute_get_local(&mut self, offset: isize) {
        self.runtime_stack
            .get_and_push(self.controller.current_base() as isize + offset);
        self.controller.advance();
    }

    fn execute_add(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::add),
            BitSize::I16 => self.execute_binop_generic(i16::add),
            BitSize::I32 => self.execute_binop_generic(i32::add),
            BitSize::I64 => self.execute_binop_generic(i64::add),
            BitSize::U8 => self.execute_binop_generic(u8::add),
            BitSize::U16 => self.execute_binop_generic(u16::add),
            BitSize::U32 => self.execute_binop_generic(u32::add),
            BitSize::U64 => self.execute_binop_generic(u64::add),
            BitSize::F32 => self.execute_binop_generic(f32::add),
            BitSize::F64 => self.execute_binop_generic(f64::add),
        }
        self.controller.advance();
    }

    fn execute_sub(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::sub),
            BitSize::I16 => self.execute_binop_generic(i16::sub),
            BitSize::I32 => self.execute_binop_generic(i32::sub),
            BitSize::I64 => self.execute_binop_generic(i64::sub),
            BitSize::U8 => self.execute_binop_generic(u8::sub),
            BitSize::U16 => self.execute_binop_generic(u16::sub),
            BitSize::U32 => self.execute_binop_generic(u32::sub),
            BitSize::U64 => self.execute_binop_generic(u64::sub),
            BitSize::F32 => self.execute_binop_generic(f32::sub),
            BitSize::F64 => self.execute_binop_generic(f64::sub),
        }
        self.controller.advance();
    }

    fn execute_mul(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::mul),
            BitSize::I16 => self.execute_binop_generic(i16::mul),
            BitSize::I32 => self.execute_binop_generic(i32::mul),
            BitSize::I64 => self.execute_binop_generic(i64::mul),
            BitSize::U8 => self.execute_binop_generic(u8::mul),
            BitSize::U16 => self.execute_binop_generic(u16::mul),
            BitSize::U32 => self.execute_binop_generic(u32::mul),
            BitSize::U64 => self.execute_binop_generic(u64::mul),
            BitSize::F32 => self.execute_binop_generic(f32::mul),
            BitSize::F64 => self.execute_binop_generic(f64::mul),
        }
        self.controller.advance();
    }

    fn execute_div(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::div),
            BitSize::I16 => self.execute_binop_generic(i16::div),
            BitSize::I32 => self.execute_binop_generic(i32::div),
            BitSize::I64 => self.execute_binop_generic(i64::div),
            BitSize::U8 => self.execute_binop_generic(u8::div),
            BitSize::U16 => self.execute_binop_generic(u16::div),
            BitSize::U32 => self.execute_binop_generic(u32::div),
            BitSize::U64 => self.execute_binop_generic(u64::div),
            BitSize::F32 => self.execute_binop_generic(f32::div),
            BitSize::F64 => self.execute_binop_generic(f64::div),
        }
        self.controller.advance();
    }

    fn execute_mod(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::rem),
            BitSize::I16 => self.execute_binop_generic(i16::rem),
            BitSize::I32 => self.execute_binop_generic(i32::rem),
            BitSize::I64 => self.execute_binop_generic(i64::rem),
            BitSize::U8 => self.execute_binop_generic(u8::rem),
            BitSize::U16 => self.execute_binop_generic(u16::rem),
            BitSize::U32 => self.execute_binop_generic(u32::rem),
            BitSize::U64 => self.execute_binop_generic(u64::rem),
            BitSize::F32 => todo!("handle invalid f32 modulo"),
            BitSize::F64 => todo!("handle invalid f64 modulo"),
        }
        self.controller.advance();
    }

    fn execute_shl(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::shl),
            BitSize::I16 => self.execute_binop_generic(i16::shl),
            BitSize::I32 => self.execute_binop_generic(i32::shl),
            BitSize::I64 => self.execute_binop_generic(i64::shl),
            BitSize::U8 => self.execute_binop_generic(u8::shl),
            BitSize::U16 => self.execute_binop_generic(u16::shl),
            BitSize::U32 => self.execute_binop_generic(u32::shl),
            BitSize::U64 => self.execute_binop_generic(u64::shl),
            BitSize::F32 => todo!("handle invalid f32 modulo"),
            BitSize::F64 => todo!("handle invalid f64 modulo"),
        }
        self.controller.advance();
    }

    fn execute_shr(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::shr),
            BitSize::I16 => self.execute_binop_generic(i16::shr),
            BitSize::I32 => self.execute_binop_generic(i32::shr),
            BitSize::I64 => self.execute_binop_generic(i64::shr),
            BitSize::U8 => self.execute_binop_generic(u8::shr),
            BitSize::U16 => self.execute_binop_generic(u16::shr),
            BitSize::U32 => self.execute_binop_generic(u32::shr),
            BitSize::U64 => self.execute_binop_generic(u64::shr),
            BitSize::F32 => todo!("handle invalid f32 modulo"),
            BitSize::F64 => todo!("handle invalid f64 modulo"),
        }
        self.controller.advance();
    }

    fn execute_binop_generic<T, F>(&mut self, f: F)
    where
        T: IntoValueType + Copy,
        F: FnOnce(T, T) -> T,
    {
        let b: T = self.runtime_stack.pop_value();
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(a, b));
    }

    fn execute_eq(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::eq),
            BitSize::I16 => self.execute_equality_generic(i16::eq),
            BitSize::I32 => self.execute_equality_generic(i32::eq),
            BitSize::I64 => self.execute_equality_generic(i64::eq),
            BitSize::U8 => self.execute_equality_generic(i8::eq),
            BitSize::U16 => self.execute_equality_generic(u16::eq),
            BitSize::U32 => self.execute_equality_generic(u32::eq),
            BitSize::U64 => self.execute_equality_generic(u64::eq),
            BitSize::F32 => self.execute_equality_generic(f32::eq),
            BitSize::F64 => self.execute_equality_generic(f64::eq),
        }
        self.controller.advance();
    }

    fn execute_neq(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::ne),
            BitSize::I16 => self.execute_equality_generic(i16::ne),
            BitSize::I32 => self.execute_equality_generic(i32::ne),
            BitSize::I64 => self.execute_equality_generic(i64::ne),
            BitSize::U8 => self.execute_equality_generic(i8::ne),
            BitSize::U16 => self.execute_equality_generic(u16::ne),
            BitSize::U32 => self.execute_equality_generic(u32::ne),
            BitSize::U64 => self.execute_equality_generic(u64::ne),
            BitSize::F32 => self.execute_equality_generic(f32::ne),
            BitSize::F64 => self.execute_equality_generic(f64::ne),
        }
        self.controller.advance();
    }

    fn execute_lt(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::lt),
            BitSize::I16 => self.execute_equality_generic(i16::lt),
            BitSize::I32 => self.execute_equality_generic(i32::lt),
            BitSize::I64 => self.execute_equality_generic(i64::lt),
            BitSize::U8 => self.execute_equality_generic(i8::lt),
            BitSize::U16 => self.execute_equality_generic(u16::lt),
            BitSize::U32 => self.execute_equality_generic(u32::lt),
            BitSize::U64 => self.execute_equality_generic(u64::lt),
            BitSize::F32 => self.execute_equality_generic(f32::lt),
            BitSize::F64 => self.execute_equality_generic(f64::lt),
        }
        self.controller.advance();
    }

    fn execute_lteq(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::le),
            BitSize::I16 => self.execute_equality_generic(i16::le),
            BitSize::I32 => self.execute_equality_generic(i32::le),
            BitSize::I64 => self.execute_equality_generic(i64::le),
            BitSize::U8 => self.execute_equality_generic(i8::le),
            BitSize::U16 => self.execute_equality_generic(u16::le),
            BitSize::U32 => self.execute_equality_generic(u32::le),
            BitSize::U64 => self.execute_equality_generic(u64::le),
            BitSize::F32 => self.execute_equality_generic(f32::le),
            BitSize::F64 => self.execute_equality_generic(f64::le),
        }
        self.controller.advance();
    }

    fn execute_gt(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::gt),
            BitSize::I16 => self.execute_equality_generic(i16::gt),
            BitSize::I32 => self.execute_equality_generic(i32::gt),
            BitSize::I64 => self.execute_equality_generic(i64::gt),
            BitSize::U8 => self.execute_equality_generic(i8::gt),
            BitSize::U16 => self.execute_equality_generic(u16::gt),
            BitSize::U32 => self.execute_equality_generic(u32::gt),
            BitSize::U64 => self.execute_equality_generic(u64::gt),
            BitSize::F32 => self.execute_equality_generic(f32::gt),
            BitSize::F64 => self.execute_equality_generic(f64::gt),
        }
        self.controller.advance();
    }

    fn execute_gteq(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_equality_generic(i8::ge),
            BitSize::I16 => self.execute_equality_generic(i16::ge),
            BitSize::I32 => self.execute_equality_generic(i32::ge),
            BitSize::I64 => self.execute_equality_generic(i64::ge),
            BitSize::U8 => self.execute_equality_generic(i8::ge),
            BitSize::U16 => self.execute_equality_generic(u16::ge),
            BitSize::U32 => self.execute_equality_generic(u32::ge),
            BitSize::U64 => self.execute_equality_generic(u64::ge),
            BitSize::F32 => self.execute_equality_generic(f32::ge),
            BitSize::F64 => self.execute_equality_generic(f64::ge),
        }
        self.controller.advance();
    }

    fn execute_equality_generic<T, F>(&mut self, f: F)
    where
        F: FnOnce(&T, &T) -> bool,
        T: IntoValueType + Copy,
    {
        let b: T = self.runtime_stack.pop_value();
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(&a, &b));
    }

    fn execute_bit_not(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_unary_op_generic(i8::not),
            BitSize::I16 => self.execute_unary_op_generic(i16::not),
            BitSize::I32 => self.execute_unary_op_generic(i32::not),
            BitSize::I64 => self.execute_unary_op_generic(i64::not),
            BitSize::U8 => self.execute_unary_op_generic(u8::not),
            BitSize::U16 => self.execute_unary_op_generic(u16::not),
            BitSize::U32 => self.execute_unary_op_generic(u32::not),
            BitSize::U64 => self.execute_unary_op_generic(u64::not),
            BitSize::F32 => todo!("cannot bitnot float data"),
            BitSize::F64 => todo!("cannot bitnot float data"),
        }
        self.controller.advance();
    }

    fn execute_unary_op_generic<T, F>(&mut self, f: F)
    where
        T: IntoValueType + Copy,
        F: FnOnce(T) -> T,
    {
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(a));
    }

    fn execute_bit_and(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::bitand),
            BitSize::I16 => self.execute_binop_generic(i16::bitand),
            BitSize::I32 => self.execute_binop_generic(i32::bitand),
            BitSize::I64 => self.execute_binop_generic(i64::bitand),
            BitSize::U8 => self.execute_binop_generic(u8::bitand),
            BitSize::U16 => self.execute_binop_generic(u16::bitand),
            BitSize::U32 => self.execute_binop_generic(u32::bitand),
            BitSize::U64 => self.execute_binop_generic(u64::bitand),
            BitSize::F32 => todo!("cannot bitand float data"),
            BitSize::F64 => todo!("cannot bitand float data"),
        }
        self.controller.advance();
    }

    fn execute_bit_or(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::bitor),
            BitSize::I16 => self.execute_binop_generic(i16::bitor),
            BitSize::I32 => self.execute_binop_generic(i32::bitor),
            BitSize::I64 => self.execute_binop_generic(i64::bitor),
            BitSize::U8 => self.execute_binop_generic(u8::bitor),
            BitSize::U16 => self.execute_binop_generic(u16::bitor),
            BitSize::U32 => self.execute_binop_generic(u32::bitor),
            BitSize::U64 => self.execute_binop_generic(u64::bitor),
            BitSize::F32 => todo!("cannot bitor float data"),
            BitSize::F64 => todo!("cannot bitor float data"),
        }
        self.controller.advance();
    }

    fn execute_bit_xor(&mut self, variant: BitSize) {
        match variant {
            BitSize::I8 => self.execute_binop_generic(i8::bitxor),
            BitSize::I16 => self.execute_binop_generic(i16::bitxor),
            BitSize::I32 => self.execute_binop_generic(i32::bitxor),
            BitSize::I64 => self.execute_binop_generic(i64::bitxor),
            BitSize::U8 => self.execute_binop_generic(u8::bitxor),
            BitSize::U16 => self.execute_binop_generic(u16::bitxor),
            BitSize::U32 => self.execute_binop_generic(u32::bitxor),
            BitSize::U64 => self.execute_binop_generic(u64::bitxor),
            BitSize::F32 => todo!("cannot xor float data"),
            BitSize::F64 => todo!("cannot xor float data"),
        }
        self.controller.advance();
    }

    fn execute_jump_if_false(&mut self, offset: isize) {
        let v: bool = self.runtime_stack.pop_value();
        if !v {
            self.controller.jump(offset);
        } else {
            self.controller.advance();
        }
    }

    fn execute_jump_if_true(&mut self, offset: isize) {
        let v: bool = self.runtime_stack.pop_value();
        if v {
            self.controller.jump(offset);
        } else {
            self.controller.advance();
        }
    }

    fn execute_jump(&mut self, offset: isize) {
        self.controller.jump(offset);
    }

    fn execute_set_local(&mut self, offset: isize) {
        self.runtime_stack
            .pop_and_set(self.controller.current_base() as isize + offset);
        self.controller.advance();
    }

    fn execute_pop(&mut self, count: usize) {
        self.runtime_stack.pop(count);
        self.controller.advance();
    }

    fn execute_call(&mut self) {
        let fn_id: usize = self.runtime_stack.pop_value();
        self.controller.call(fn_id, self.runtime_stack.top_index());
    }

    fn execute_ret(&mut self) {
        self.controller.ret();
    }

    fn execute_call_native(&mut self, native_func: &String) {
        self.native_executor
            .execute_native(native_func.as_str(), &self.runtime_stack);
        self.controller.advance();
    }
}
