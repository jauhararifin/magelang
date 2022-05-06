use std::alloc::{Layout, alloc};
use std::cmp::PartialEq;
use std::ops::{Add, Div, Mul, Rem, Shl, Shr, Sub, BitAnd, BitOr, BitXor, Not};

use crate::bytecode::{self, Variant, Instruction, Program};

use super::value::ValueType;
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
            //     self.controller..get(func_id).unwrap().name,
            //     func_id,
            //     instruction_index
            // );
            // println!("executing: {:?}", instruction);

            match instruction {
                Instruction::Nop => self.controller.advance(),
                Instruction::Constant(val) => self.execute_constant(val),

                Instruction::Add(variant) => self.execute_add(variant),
                Instruction::Sub(variant) => self.execute_sub(variant),
                Instruction::Mul(variant) => self.execute_mul(variant),
                Instruction::Div(variant) => self.execute_div(variant),
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

                Instruction::Alloc => self.execute_alloc(),
                Instruction::GetHeap(variant) => self.execute_get_heap(variant),
                Instruction::SetHeap => self.execute_set_heap(),

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
            bytecode::Value::Ptr(val) => self.runtime_stack.push_value(ValueType::Ptr, val),
        }
        self.controller.advance();
    }

    fn execute_get_local(&mut self, offset: isize) {
        self.runtime_stack
            .get_and_push(self.controller.current_base() as isize + offset);
        self.controller.advance();
    }

    fn execute_add(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::add),
            Variant::I16 => self.execute_binop_generic(i16::add),
            Variant::I32 => self.execute_binop_generic(i32::add),
            Variant::I64 => self.execute_binop_generic(i64::add),
            Variant::U8 => self.execute_binop_generic(u8::add),
            Variant::U16 => self.execute_binop_generic(u16::add),
            Variant::U32 => self.execute_binop_generic(u32::add),
            Variant::U64 => self.execute_binop_generic(u64::add),
            Variant::F32 => self.execute_binop_generic(f32::add),
            Variant::F64 => self.execute_binop_generic(f64::add),
        }
        self.controller.advance();
    }

    fn execute_sub(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::sub),
            Variant::I16 => self.execute_binop_generic(i16::sub),
            Variant::I32 => self.execute_binop_generic(i32::sub),
            Variant::I64 => self.execute_binop_generic(i64::sub),
            Variant::U8 => self.execute_binop_generic(u8::sub),
            Variant::U16 => self.execute_binop_generic(u16::sub),
            Variant::U32 => self.execute_binop_generic(u32::sub),
            Variant::U64 => self.execute_binop_generic(u64::sub),
            Variant::F32 => self.execute_binop_generic(f32::sub),
            Variant::F64 => self.execute_binop_generic(f64::sub),
        }
        self.controller.advance();
    }

    fn execute_mul(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::mul),
            Variant::I16 => self.execute_binop_generic(i16::mul),
            Variant::I32 => self.execute_binop_generic(i32::mul),
            Variant::I64 => self.execute_binop_generic(i64::mul),
            Variant::U8 => self.execute_binop_generic(u8::mul),
            Variant::U16 => self.execute_binop_generic(u16::mul),
            Variant::U32 => self.execute_binop_generic(u32::mul),
            Variant::U64 => self.execute_binop_generic(u64::mul),
            Variant::F32 => self.execute_binop_generic(f32::mul),
            Variant::F64 => self.execute_binop_generic(f64::mul),
        }
        self.controller.advance();
    }

    fn execute_div(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::div),
            Variant::I16 => self.execute_binop_generic(i16::div),
            Variant::I32 => self.execute_binop_generic(i32::div),
            Variant::I64 => self.execute_binop_generic(i64::div),
            Variant::U8 => self.execute_binop_generic(u8::div),
            Variant::U16 => self.execute_binop_generic(u16::div),
            Variant::U32 => self.execute_binop_generic(u32::div),
            Variant::U64 => self.execute_binop_generic(u64::div),
            Variant::F32 => self.execute_binop_generic(f32::div),
            Variant::F64 => self.execute_binop_generic(f64::div),
        }
        self.controller.advance();
    }

    fn execute_mod(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::rem),
            Variant::I16 => self.execute_binop_generic(i16::rem),
            Variant::I32 => self.execute_binop_generic(i32::rem),
            Variant::I64 => self.execute_binop_generic(i64::rem),
            Variant::U8 => self.execute_binop_generic(u8::rem),
            Variant::U16 => self.execute_binop_generic(u16::rem),
            Variant::U32 => self.execute_binop_generic(u32::rem),
            Variant::U64 => self.execute_binop_generic(u64::rem),
            Variant::F32 => todo!("handle invalid f32 modulo"),
            Variant::F64 => todo!("handle invalid f64 modulo"),
        }
        self.controller.advance();
    }

    fn execute_shl(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::shl),
            Variant::I16 => self.execute_binop_generic(i16::shl),
            Variant::I32 => self.execute_binop_generic(i32::shl),
            Variant::I64 => self.execute_binop_generic(i64::shl),
            Variant::U8 => self.execute_binop_generic(u8::shl),
            Variant::U16 => self.execute_binop_generic(u16::shl),
            Variant::U32 => self.execute_binop_generic(u32::shl),
            Variant::U64 => self.execute_binop_generic(u64::shl),
            Variant::F32 => todo!("handle invalid f32 modulo"),
            Variant::F64 => todo!("handle invalid f64 modulo"),
        }
        self.controller.advance();
    }

    fn execute_shr(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::shr),
            Variant::I16 => self.execute_binop_generic(i16::shr),
            Variant::I32 => self.execute_binop_generic(i32::shr),
            Variant::I64 => self.execute_binop_generic(i64::shr),
            Variant::U8 => self.execute_binop_generic(u8::shr),
            Variant::U16 => self.execute_binop_generic(u16::shr),
            Variant::U32 => self.execute_binop_generic(u32::shr),
            Variant::U64 => self.execute_binop_generic(u64::shr),
            Variant::F32 => todo!("handle invalid f32 modulo"),
            Variant::F64 => todo!("handle invalid f64 modulo"),
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

    fn execute_eq(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::eq),
            Variant::I16 => self.execute_equality_generic(i16::eq),
            Variant::I32 => self.execute_equality_generic(i32::eq),
            Variant::I64 => self.execute_equality_generic(i64::eq),
            Variant::U8 => self.execute_equality_generic(i8::eq),
            Variant::U16 => self.execute_equality_generic(u16::eq),
            Variant::U32 => self.execute_equality_generic(u32::eq),
            Variant::U64 => self.execute_equality_generic(u64::eq),
            Variant::F32 => self.execute_equality_generic(f32::eq),
            Variant::F64 => self.execute_equality_generic(f64::eq),
        }
        self.controller.advance();
    }

    fn execute_neq(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::ne),
            Variant::I16 => self.execute_equality_generic(i16::ne),
            Variant::I32 => self.execute_equality_generic(i32::ne),
            Variant::I64 => self.execute_equality_generic(i64::ne),
            Variant::U8 => self.execute_equality_generic(i8::ne),
            Variant::U16 => self.execute_equality_generic(u16::ne),
            Variant::U32 => self.execute_equality_generic(u32::ne),
            Variant::U64 => self.execute_equality_generic(u64::ne),
            Variant::F32 => self.execute_equality_generic(f32::ne),
            Variant::F64 => self.execute_equality_generic(f64::ne),
        }
        self.controller.advance();
    }

    fn execute_lt(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::lt),
            Variant::I16 => self.execute_equality_generic(i16::lt),
            Variant::I32 => self.execute_equality_generic(i32::lt),
            Variant::I64 => self.execute_equality_generic(i64::lt),
            Variant::U8 => self.execute_equality_generic(i8::lt),
            Variant::U16 => self.execute_equality_generic(u16::lt),
            Variant::U32 => self.execute_equality_generic(u32::lt),
            Variant::U64 => self.execute_equality_generic(u64::lt),
            Variant::F32 => self.execute_equality_generic(f32::lt),
            Variant::F64 => self.execute_equality_generic(f64::lt),
        }
        self.controller.advance();
    }

    fn execute_lteq(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::le),
            Variant::I16 => self.execute_equality_generic(i16::le),
            Variant::I32 => self.execute_equality_generic(i32::le),
            Variant::I64 => self.execute_equality_generic(i64::le),
            Variant::U8 => self.execute_equality_generic(i8::le),
            Variant::U16 => self.execute_equality_generic(u16::le),
            Variant::U32 => self.execute_equality_generic(u32::le),
            Variant::U64 => self.execute_equality_generic(u64::le),
            Variant::F32 => self.execute_equality_generic(f32::le),
            Variant::F64 => self.execute_equality_generic(f64::le),
        }
        self.controller.advance();
    }

    fn execute_gt(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::gt),
            Variant::I16 => self.execute_equality_generic(i16::gt),
            Variant::I32 => self.execute_equality_generic(i32::gt),
            Variant::I64 => self.execute_equality_generic(i64::gt),
            Variant::U8 => self.execute_equality_generic(i8::gt),
            Variant::U16 => self.execute_equality_generic(u16::gt),
            Variant::U32 => self.execute_equality_generic(u32::gt),
            Variant::U64 => self.execute_equality_generic(u64::gt),
            Variant::F32 => self.execute_equality_generic(f32::gt),
            Variant::F64 => self.execute_equality_generic(f64::gt),
        }
        self.controller.advance();
    }

    fn execute_gteq(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_equality_generic(i8::ge),
            Variant::I16 => self.execute_equality_generic(i16::ge),
            Variant::I32 => self.execute_equality_generic(i32::ge),
            Variant::I64 => self.execute_equality_generic(i64::ge),
            Variant::U8 => self.execute_equality_generic(i8::ge),
            Variant::U16 => self.execute_equality_generic(u16::ge),
            Variant::U32 => self.execute_equality_generic(u32::ge),
            Variant::U64 => self.execute_equality_generic(u64::ge),
            Variant::F32 => self.execute_equality_generic(f32::ge),
            Variant::F64 => self.execute_equality_generic(f64::ge),
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

    fn execute_bit_not(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_unary_op_generic(i8::not),
            Variant::I16 => self.execute_unary_op_generic(i16::not),
            Variant::I32 => self.execute_unary_op_generic(i32::not),
            Variant::I64 => self.execute_unary_op_generic(i64::not),
            Variant::U8 => self.execute_unary_op_generic(u8::not),
            Variant::U16 => self.execute_unary_op_generic(u16::not),
            Variant::U32 => self.execute_unary_op_generic(u32::not),
            Variant::U64 => self.execute_unary_op_generic(u64::not),
            Variant::F32 => todo!("cannot bitnot float data"),
            Variant::F64 => todo!("cannot bitnot float data"),
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

    fn execute_bit_and(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::bitand),
            Variant::I16 => self.execute_binop_generic(i16::bitand),
            Variant::I32 => self.execute_binop_generic(i32::bitand),
            Variant::I64 => self.execute_binop_generic(i64::bitand),
            Variant::U8 => self.execute_binop_generic(u8::bitand),
            Variant::U16 => self.execute_binop_generic(u16::bitand),
            Variant::U32 => self.execute_binop_generic(u32::bitand),
            Variant::U64 => self.execute_binop_generic(u64::bitand),
            Variant::F32 => todo!("cannot bitand float data"),
            Variant::F64 => todo!("cannot bitand float data"),
        }
        self.controller.advance();
    }

    fn execute_bit_or(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::bitor),
            Variant::I16 => self.execute_binop_generic(i16::bitor),
            Variant::I32 => self.execute_binop_generic(i32::bitor),
            Variant::I64 => self.execute_binop_generic(i64::bitor),
            Variant::U8 => self.execute_binop_generic(u8::bitor),
            Variant::U16 => self.execute_binop_generic(u16::bitor),
            Variant::U32 => self.execute_binop_generic(u32::bitor),
            Variant::U64 => self.execute_binop_generic(u64::bitor),
            Variant::F32 => todo!("cannot bitor float data"),
            Variant::F64 => todo!("cannot bitor float data"),
        }
        self.controller.advance();
    }

    fn execute_bit_xor(&mut self, variant: Variant) {
        match variant {
            Variant::I8 => self.execute_binop_generic(i8::bitxor),
            Variant::I16 => self.execute_binop_generic(i16::bitxor),
            Variant::I32 => self.execute_binop_generic(i32::bitxor),
            Variant::I64 => self.execute_binop_generic(i64::bitxor),
            Variant::U8 => self.execute_binop_generic(u8::bitxor),
            Variant::U16 => self.execute_binop_generic(u16::bitxor),
            Variant::U32 => self.execute_binop_generic(u32::bitxor),
            Variant::U64 => self.execute_binop_generic(u64::bitxor),
            Variant::F32 => todo!("cannot xor float data"),
            Variant::F64 => todo!("cannot xor float data"),
        }
        self.controller.advance();
    }

    fn execute_alloc(&mut self) {
        let size: usize = self.runtime_stack.pop_value();

        let layout = Layout::array::<u8>(size).unwrap();
        let data = unsafe { alloc(layout) } as usize;

        self.runtime_stack.push_value(ValueType::Ptr, data);
        self.controller.advance();
    }   

    fn execute_get_heap(&mut self, variant: Variant) {
        let ptr: usize = self.runtime_stack.pop_value();

        let value_type = match variant {
            Variant::I8 => ValueType::I8,
            Variant::I16 => ValueType::I16,
            Variant::I32 => ValueType::I32,
            Variant::I64 => ValueType::I64,
            Variant::U8 => ValueType::U8,
            Variant::U16 => ValueType::U16,
            Variant::U32 => ValueType::U32,
            Variant::U64 => ValueType::U64,
            Variant::F32 => ValueType::F32,
            Variant::F64 => ValueType::F64,
        };
        self.runtime_stack.push_from_ptr(value_type, ptr);

        self.controller.advance();
    }   

    fn execute_set_heap(&mut self) {
        let ptr: usize = self.runtime_stack.pop_value();
        let value = self.runtime_stack.pop_value_detail();

        unsafe {
            std::ptr::copy::<u8>(value.data as *const u8, ptr as *mut u8, value.typ.size());
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
