use crate::layout::Scalar;
use std::fmt::Write;

#[derive(Default)]
pub(crate) struct Assembly(pub(crate) String);

impl Assembly {
    pub(crate) fn emit(&mut self, text: impl AsRef<str>) {
        writeln!(self.0, "{}", text.as_ref()).unwrap();
    }

    pub(crate) fn immediate(&mut self, register: &str, value: u64) {
        self.emit(format!("    movz {register}, #{}", value & 0xffff));
        for shift in [16, 32, 48] {
            let part = (value >> shift) & 0xffff;
            if part != 0 {
                self.emit(format!("    movk {register}, #{part}, lsl #{shift}"));
            }
        }
    }

    // x16 is always scratch, including when an offset exceeds ARM's immediate range.
    pub(crate) fn address(&mut self, dest: &str, base: &str, offset: isize) {
        self.immediate("x16", offset.unsigned_abs() as u64);
        let op = if offset < 0 { "sub" } else { "add" };
        self.emit(format!("    {op} {dest}, {base}, x16"));
    }

    pub(crate) fn symbol(&mut self, dest: &str, symbol: &str) {
        self.emit(format!("    adrp {dest}, {symbol}@PAGE\n    add {dest}, {dest}, {symbol}@PAGEOFF"));
    }

    pub(crate) fn adjust_stack(&mut self, bytes: isize) {
        if bytes != 0 {
            self.address("sp", "sp", bytes);
        }
    }

    pub(crate) fn push(&mut self, register: &str) {
        self.emit(format!("    str {register}, [sp, #-16]!"));
    }

    pub(crate) fn pop(&mut self, register: &str) {
        self.emit(format!("    ldr {register}, [sp], #16"));
    }

    pub(crate) fn normalize(&mut self, scalar: Scalar) {
        match scalar {
            Scalar::Int { signed: true, bits: 8 } => self.emit("    sxtb x9, w9"),
            Scalar::Int { signed: true, bits: 16 } => self.emit("    sxth x9, w9"),
            Scalar::Int { signed: true, bits: 32 } => self.emit("    sxtw x9, w9"),
            Scalar::Int { signed: false, bits: 8 } => self.emit("    and x9, x9, #0xff"),
            Scalar::Int { signed: false, bits: 16 } => self.emit("    and x9, x9, #0xffff"),
            Scalar::Int { signed: false, bits: 32 } | Scalar::F32 => self.emit("    mov w9, w9"),
            _ => {}
        }
    }

    pub(crate) fn load(&mut self, scalar: Scalar, base: &str, offset: isize) {
        self.address("x17", base, offset);
        let (op, reg) = match scalar {
            Scalar::Int { signed: true, bits: 8 } => ("ldrsb", "x9"),
            Scalar::Int { signed: true, bits: 16 } => ("ldrsh", "x9"),
            Scalar::Int { signed: true, bits: 32 } => ("ldrsw", "x9"),
            Scalar::Int { bits: 8, .. } => ("ldrb", "w9"),
            Scalar::Int { bits: 16, .. } => ("ldrh", "w9"),
            Scalar::Int { bits: 32, .. } | Scalar::F32 => ("ldr", "w9"),
            _ => ("ldr", "x9"),
        };
        self.emit(format!("    {op} {reg}, [x17]"));
    }

    pub(crate) fn store(&mut self, scalar: Scalar, base: &str, offset: isize) {
        self.address("x17", base, offset);
        let (op, reg) = match scalar.size() {
            1 => ("strb", "w9"),
            2 => ("strh", "w9"),
            4 => ("str", "w9"),
            _ => ("str", "x9"),
        };
        self.emit(format!("    {op} {reg}, [x17]"));
    }
}
