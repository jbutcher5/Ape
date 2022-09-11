use crate::asm;

use asm::{Instr, Instr::*, Operand::*, Register, Register::*};
use std::collections::HashMap;

#[inline]
fn next_aligned_stack(bytes: u64) -> u64 {
    match bytes {
        0 => 16,
        start => (start as f64 / 16.0).ceil() as u64 * 16,
    }
}

#[inline]
fn push_type(t: Type) -> Vec<Instr> {
    match t {
        Int(val) => vec![Mov(RAX, Value(val)), Push(Reg(RAX))],
        Bool(val) => vec![Mov(RAX, Value(val as i64)), Push(Reg(RAX))], // TODO: Implement 8 bit registers and use AL instead of RAX
    }
}

#[inline]
fn push_reg(r: Register) -> Vec<Instr> {
    vec![Mov(RAX, Reg(r)), Push(Reg(RAX))]
}

#[inline]
fn mov_reg(to: Register, from: Register) -> Instr {
    Mov(to, Reg(from))
}

#[inline]
fn mov_type(r: Register, t: Type) -> Instr {
    match t {
        Int(val) => Mov(r, Value(val)),
        Bool(val) => Mov(r, Value(val as i64)), // TODO: Implement 8 bit registers and use AL instead of RAX
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Int(i64),
    Bool(bool),
}

impl Type {
    pub fn byte_size(&self) -> u64 {
        use Type::*;

        match self {
            Int(_) => 8,
            Bool(_) => 8,
        }
    }
}

#[derive(Clone, Debug)]
pub enum InterRep {
    Define(String, Type),
    CCall(String, Vec<Register>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum StackDirective {
    Variable(String, Type),
    TempLiteral(Type),
    TempReg(Register),
    Empty(u64),
    BasePointer,
}

use InterRep::*;
use StackDirective::*;
use Type::*;

impl StackDirective {
    pub fn byte_size(&self) -> u64 {
        use StackDirective::*;

        match self {
            Variable(_, t) | TempLiteral(t) => t.byte_size(),
            TempReg(_) => 8,
            BasePointer => 8,
            Empty(n) => *n,
        }
    }
}

pub struct Generator {
    stack: Vec<StackDirective>,
    bp: u64,
    functions: HashMap<String, Vec<Instr>>,
    data: HashMap<String, String>,
}

impl Generator {
    pub fn new() -> Self {
        use StackDirective::*;

        let mut initial = HashMap::new();

        initial.insert("main".to_string(), vec![Push(Reg(RBP)), Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![BasePointer],
            bp: 0,
            functions: initial.clone(),
            data: HashMap::new(),
        }
    }

    pub fn get_variable(&self, name: String) -> Register {
        let mut acc: i64 = 0;

        for x in &self.stack[0..] {
            if let Variable(var, t) = x {
                acc -= x.byte_size() as i64;
                if var.clone() == name {
                    break;
                }
            } else {
                acc -= x.byte_size() as i64;
            }
        }

        Stack(acc)
    }

    pub fn apply(&mut self, ir: Vec<InterRep>) {
        for instruction in ir {
            self.handle_ir(instruction);
        }
    }

    fn handle_ir(&mut self, instr: InterRep) {
        match instr {
            Define(name, value) => self.push("main", Variable(name, value)),
            CCall(name, parameters) => self.c_call("main", name, parameters),
        }
    }

    fn push(&mut self, function: &str, directive: StackDirective) {
        self.stack.push(directive.clone());

        self.functions
            .get_mut(&function.to_string())
            .unwrap()
            .extend(match directive {
                BasePointer => vec![Push(Reg(RBP))],
                Variable(_, t) | TempLiteral(t) => push_type(t),
                TempReg(r) => push_reg(r),
                Empty(n) => vec![Sub(RSP, Value(n as i64))],
            })
    }

    fn stack_size_from_rbp(&self) -> u64 {
        let mut acc = 0;

        for x in self.stack.iter().rev() {
            if *x == BasePointer {
                break;
            }
            acc += x.byte_size();
        }

        acc
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<Register>) {
        const REGSITERS: [Register; 6] = [RDI, RSI, RCX, RDX, R(8), R(9)];

        let parameter_stack_size = if parameters.len() > 6 {
            parameters[5..].iter().map(|x| 8).sum()
        } else {
            0
        };

        let new_stack_size = parameter_stack_size + self.stack_size_from_rbp();
        let next_multiple_of_16 = next_aligned_stack(new_stack_size);

        if new_stack_size != next_multiple_of_16 {
            self.push("main", Empty(next_multiple_of_16 - new_stack_size))
        }

        for (i, r) in parameters.iter().enumerate().rev() {
            if i > 5 {
                self.push(function, TempReg(r.clone()));
            } else {
                self.functions
                    .get_mut(&function.to_string())
                    .unwrap()
                    .push(mov_reg(REGSITERS[i].clone(), r.clone()))
            }
        }

        self.functions
            .get_mut(&function.to_string())
            .unwrap()
            .push(Call(c_func));
    }

    fn write_exit(&mut self) {
        self.functions
            .get_mut(&"main".to_string())
            .unwrap()
            .extend(vec![Mov(RAX, Value(60)), Mov(RDI, Value(0)), Syscall]);
    }

    pub fn export(&mut self) -> Vec<u8> {
        self.write_exit();

        let mut buffer: Vec<u8> = include_bytes!("header.asm").to_vec();
        for (key, value) in &self.functions {
            buffer.extend(format!("\n{}:\n", key).as_bytes());
            for instr in value {
                buffer.extend(format!("    {}\n", instr.to_string()).as_bytes())
            }
        }

        buffer.extend(b"\nsection .data\n");
        for (key, value) in &self.data {
            buffer.extend(format!("{}: db {}", key, value).as_bytes());
        }

        buffer
    }

    pub fn define_bytes(&mut self, name: String, value: String) {
        self.data.insert(name, value);
    }
}
