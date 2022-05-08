use std::cmp::PartialEq;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};

use crate::bytecode::{Instruction, Program};

use super::mem::{IMemoryManager, MemoryManager};
use super::value::IntoType;
use super::{
    control::{Controller, IController},
    errors::Error,
    native::{INativeExecutor, NativeExecutor},
    stack::RuntimeStack,
};

pub struct Executor {
    runtime_stack: RuntimeStack,

    controller: Box<dyn IController>,
    native_executor: Box<dyn INativeExecutor>,
    memory_manager: Box<dyn IMemoryManager>,
}

enum Variant {
    Int,
    Uint,
    Float,
    Double,
}

impl Executor {
    pub fn load(program: Program) -> Result<Self, Error> {
        let controller = Box::new(Controller::load(program));
        let native_executor = Box::new(NativeExecutor::new());
        let memory_manager = Box::new(MemoryManager::new());
        Ok(Self {
            runtime_stack: RuntimeStack::allocate(16 * 1024)?,
            controller,
            native_executor,
            memory_manager,
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

                Instruction::ConstI64(val) => self.execute_constant(val),
                Instruction::ConstF32(val) => self.execute_constant(val),
                Instruction::ConstF64(val) => self.execute_constant(val),

                Instruction::AddI64 => self.execute_add(Variant::Uint),
                Instruction::AddF32 => self.execute_add(Variant::Float),
                Instruction::AddF64 => self.execute_add(Variant::Double),

                Instruction::SubI64 => self.execute_sub(Variant::Uint),
                Instruction::SubF32 => self.execute_sub(Variant::Float),
                Instruction::SubF64 => self.execute_sub(Variant::Double),

                Instruction::SMulI64 => self.execute_mul(Variant::Int),
                Instruction::MulI64 => self.execute_mul(Variant::Uint),
                Instruction::MulF32 => self.execute_mul(Variant::Float),
                Instruction::MulF64 => self.execute_mul(Variant::Double),

                Instruction::SDivI64 => self.execute_div(Variant::Int),
                Instruction::DivI64 => self.execute_div(Variant::Uint),
                Instruction::DivF32 => self.execute_div(Variant::Float),
                Instruction::DivF64 => self.execute_div(Variant::Double),

                Instruction::SModI64 => self.execute_mod(Variant::Int),
                Instruction::ModI64 => self.execute_mod(Variant::Uint),

                Instruction::ShlI64 => self.execute_shl(Variant::Uint),
                Instruction::ShrI64 => self.execute_shr(Variant::Uint),
                Instruction::SShrI64 => self.execute_shr(Variant::Int),

                Instruction::EqI64 => self.execute_eq(Variant::Uint),
                Instruction::EqF32 => self.execute_eq(Variant::Float),
                Instruction::EqF64 => self.execute_eq(Variant::Double),

                Instruction::NEqI64 => self.execute_neq(Variant::Uint),
                Instruction::NEqF32 => self.execute_neq(Variant::Float),
                Instruction::NEqF64 => self.execute_neq(Variant::Double),

                Instruction::GTI64 => self.execute_gt(Variant::Uint),
                Instruction::GTF32 => self.execute_gt(Variant::Float),
                Instruction::GTF64 => self.execute_gt(Variant::Double),

                Instruction::GTEqI64 => self.execute_gteq(Variant::Uint),
                Instruction::GTEqF32 => self.execute_gteq(Variant::Float),
                Instruction::GTEqF64 => self.execute_gteq(Variant::Double),

                Instruction::LTI64 => self.execute_lt(Variant::Uint),
                Instruction::LTF32 => self.execute_lt(Variant::Float),
                Instruction::LTF64 => self.execute_lt(Variant::Double),

                Instruction::LTEqI64 => self.execute_lteq(Variant::Uint),
                Instruction::LTEqF32 => self.execute_lteq(Variant::Float),
                Instruction::LTEqF64 => self.execute_lteq(Variant::Double),

                Instruction::NotI64 => self.execute_bit_not(),
                Instruction::AndI64 => self.execute_bit_and(),
                Instruction::OrI64 => self.execute_bit_or(),
                Instruction::XorI64 => self.execute_bit_xor(),

                Instruction::ConvertF32ToI64 => self.execute_cast(Variant::Float, Variant::Uint),
                Instruction::SConvertF32ToI64 => self.execute_cast(Variant::Float, Variant::Int),
                Instruction::ConvertF32ToF64 => self.execute_cast(Variant::Float, Variant::Double),
                Instruction::ConvertF64ToI64 => self.execute_cast(Variant::Double, Variant::Uint),
                Instruction::SConvertF64ToI64 => self.execute_cast(Variant::Double, Variant::Int),
                Instruction::ConvertF64ToF32 => self.execute_cast(Variant::Double, Variant::Float),
                Instruction::ConvertI64ToF32 => self.execute_cast(Variant::Uint, Variant::Float),
                Instruction::ConvertI64ToF64 => self.execute_cast(Variant::Uint, Variant::Double),
                Instruction::SConvertI64ToF32 => self.execute_cast(Variant::Int, Variant::Float),
                Instruction::SConvertI64ToF64 => self.execute_cast(Variant::Int, Variant::Double),

                Instruction::GetLocal(offset) => self.execute_get_local(offset),
                Instruction::SetLocal(offset) => self.execute_set_local(offset),

                Instruction::AllocArrayI64 => self.execute_array_alloc::<u64>(),
                Instruction::AllocArrayF32 => self.execute_array_alloc::<f32>(),
                Instruction::AllocArrayF64 => self.execute_array_alloc::<f64>(),

                Instruction::ArrayGetI64 => todo!(), // self.execute_get_heap(variant),
                Instruction::ArrayGetF32 => todo!(),
                Instruction::ArrayGetF64 => todo!(),

                Instruction::ArraySetI64 => self.execute_array_primitive_set::<u64>(), // self.execute_set_heap(variant),
                Instruction::ArraySetF32 => self.execute_array_primitive_set::<f32>(),
                Instruction::ArraySetF64 => self.execute_array_primitive_set::<f64>(),

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

    fn execute_constant<T: IntoType>(&mut self, val: T) {
        self.runtime_stack.push_primitive(val);
        self.controller.advance();
    }

    fn execute_add(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::add),
            Variant::Uint => self.execute_binop_generic(u64::add),
            Variant::Float => self.execute_binop_generic(f32::add),
            Variant::Double => self.execute_binop_generic(f64::add),
        }
        self.controller.advance();
    }

    fn execute_sub(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::sub),
            Variant::Uint => self.execute_binop_generic(u64::sub),
            Variant::Float => self.execute_binop_generic(f32::sub),
            Variant::Double => self.execute_binop_generic(f64::sub),
        }
        self.controller.advance();
    }

    fn execute_mul(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::mul),
            Variant::Uint => self.execute_binop_generic(u64::mul),
            Variant::Float => self.execute_binop_generic(f32::mul),
            Variant::Double => self.execute_binop_generic(f64::mul),
        }
        self.controller.advance();
    }

    fn execute_div(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::div),
            Variant::Uint => self.execute_binop_generic(u64::div),
            Variant::Float => self.execute_binop_generic(f32::div),
            Variant::Double => self.execute_binop_generic(f64::div),
        }
        self.controller.advance();
    }

    fn execute_mod(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::rem),
            Variant::Uint => self.execute_binop_generic(u64::rem),
            _ => unreachable!(),
        }
        self.controller.advance();
    }

    fn execute_shl(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::shl),
            Variant::Uint => self.execute_binop_generic(u64::shl),
            _ => unreachable!(),
        }
        self.controller.advance();
    }

    fn execute_shr(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_binop_generic(i64::shr),
            Variant::Uint => self.execute_binop_generic(u64::shr),
            _ => unreachable!(),
        }
        self.controller.advance();
    }

    fn execute_binop_generic<T, F>(&mut self, f: F)
    where
        T: IntoType + Copy,
        F: FnOnce(T, T) -> T,
    {
        let b: T = self.runtime_stack.pop_value();
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(a, b));
    }

    fn execute_eq(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::eq),
            Variant::Uint => self.execute_equality_generic(u64::eq),
            Variant::Float => self.execute_equality_generic(f32::eq),
            Variant::Double => self.execute_equality_generic(f64::eq),
        }
        self.controller.advance();
    }

    fn execute_neq(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::ne),
            Variant::Uint => self.execute_equality_generic(u64::ne),
            Variant::Float => self.execute_equality_generic(f32::ne),
            Variant::Double => self.execute_equality_generic(f64::ne),
        }
        self.controller.advance();
    }

    fn execute_lt(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::lt),
            Variant::Uint => self.execute_equality_generic(u64::lt),
            Variant::Float => self.execute_equality_generic(f32::lt),
            Variant::Double => self.execute_equality_generic(f64::lt),
        }
        self.controller.advance();
    }

    fn execute_lteq(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::le),
            Variant::Uint => self.execute_equality_generic(u64::le),
            Variant::Float => self.execute_equality_generic(f32::le),
            Variant::Double => self.execute_equality_generic(f64::le),
        }
        self.controller.advance();
    }

    fn execute_gt(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::gt),
            Variant::Uint => self.execute_equality_generic(u64::gt),
            Variant::Float => self.execute_equality_generic(f32::gt),
            Variant::Double => self.execute_equality_generic(f64::gt),
        }
        self.controller.advance();
    }

    fn execute_gteq(&mut self, variant: Variant) {
        match variant {
            Variant::Int => self.execute_equality_generic(i64::ge),
            Variant::Uint => self.execute_equality_generic(u64::ge),
            Variant::Float => self.execute_equality_generic(f32::ge),
            Variant::Double => self.execute_equality_generic(f64::ge),
        }
        self.controller.advance();
    }

    fn execute_equality_generic<T, F>(&mut self, f: F)
    where
        F: FnOnce(&T, &T) -> bool,
        T: IntoType + Copy,
    {
        let b: T = self.runtime_stack.pop_value();
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(&a, &b) as u64);
    }

    fn execute_bit_not(&mut self) {
        self.execute_unary_op_generic(u64::not);
        self.controller.advance();
    }

    fn execute_unary_op_generic<T, F>(&mut self, f: F)
    where
        T: IntoType + Copy,
        F: FnOnce(T) -> T,
    {
        let a: T = self.runtime_stack.pop_value();
        self.runtime_stack.push_primitive(f(a));
    }

    fn execute_bit_and(&mut self) {
        self.execute_binop_generic(u64::bitand);
        self.controller.advance();
    }

    fn execute_bit_or(&mut self) {
        self.execute_binop_generic(u64::bitor);
        self.controller.advance();
    }

    fn execute_bit_xor(&mut self) {
        self.execute_binop_generic(u64::bitxor);
        self.controller.advance();
    }

    fn execute_cast(&mut self, source: Variant, target: Variant) {
        match (source, target) {
            (Variant::Uint, Variant::Float) => self.execute_convert_generic(|f: i64| f as f32),
            (Variant::Uint, Variant::Double) => self.execute_convert_generic(|f: i64| f as f64),

            (Variant::Int, Variant::Float) => self.execute_convert_generic(|f: u64| f as f32),
            (Variant::Int, Variant::Double) => self.execute_convert_generic(|f: u64| f as f64),

            (Variant::Float, Variant::Uint) => self.execute_convert_generic(|f: f32| f as u64),
            (Variant::Float, Variant::Int) => self.execute_convert_generic(|f: f32| f as i64),
            (Variant::Float, Variant::Double) => self.execute_convert_generic(|f: f32| f as f64),

            (Variant::Double, Variant::Uint) => self.execute_convert_generic(|f: f64| f as u64),
            (Variant::Double, Variant::Int) => self.execute_convert_generic(|f: f64| f as i64),
            (Variant::Double, Variant::Float) => self.execute_convert_generic(|f: f64| f as f32),
            _ => unreachable!(),
        }

        self.controller.advance();
    }

    fn execute_convert_generic<T, U, F>(&mut self, f: F)
    where
        T: IntoType + Copy,
        U: IntoType + Copy,
        F: FnOnce(T) -> U,
    {
        let v: T = self.runtime_stack.pop_value();
        let v: U = f(v);
        self.runtime_stack.push_primitive(v);
    }

    fn execute_get_local(&mut self, offset: isize) {
        self.runtime_stack
            .get_and_push(self.controller.current_base() as isize + offset);
        self.controller.advance();
    }

    fn execute_set_local(&mut self, offset: isize) {
        self.runtime_stack
            .pop_and_set(self.controller.current_base() as isize + offset);
        self.controller.advance();
    }

    fn execute_array_alloc<T: IntoType>(&mut self) {
        let elem_type = T::into();
        let size: u64 = self.runtime_stack.pop_value();
        let local = self.memory_manager.alloc_array(elem_type, size);
        self.runtime_stack.push_value(local.typ, local.data as usize);

        self.controller.advance();
    }

    fn execute_array_primitive_set<T: Copy + IntoType>(&mut self) {
        // [i64][array][index] -> []
        let index: u64 = self.runtime_stack.pop_value();
        let array: u64 = self.runtime_stack.pop_value();
        let value: T = self.runtime_stack.pop_value();

        let elem_typ = T::into();
        let elem_ptr = (array + index * elem_typ.size()) as *mut T;
        unsafe { *elem_ptr = value; }

        self.controller.advance();
    }

    fn execute_jump_if_false(&mut self, offset: isize) {
        let v: u64 = self.runtime_stack.pop_value();
        if v == 0 {
            self.controller.jump(offset);
        } else {
            self.controller.advance();
        }
    }

    fn execute_jump_if_true(&mut self, offset: isize) {
        let v: u64 = self.runtime_stack.pop_value();
        if v != 0 {
            self.controller.jump(offset);
        } else {
            self.controller.advance();
        }
    }

    fn execute_jump(&mut self, offset: isize) {
        self.controller.jump(offset);
    }

    fn execute_pop(&mut self, count: usize) {
        self.runtime_stack.pop(count);
        self.controller.advance();
    }

    fn execute_call(&mut self) {
        let fn_id: u64 = self.runtime_stack.pop_value();
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
