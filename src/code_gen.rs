use crate::asm;
use crate::parser;

use asm::{Instr, Instr::*, Operand::*, Register, Register::*};
use std::collections::HashMap;

fn push_type(t: Type) -> Vec<Instr> {
    match t {
        Int(val) => vec![Mov(RAX, Value(val)), Push(Reg(RAX))],
        Bool(val) => vec![Mov(RAX, Value(val as i64)), Push(Reg(RAX))], // TODO: Implement 8 bit registers and use AL instead of RAX
    }
}

fn mov_type(r: Register, t: Type) -> Instr {
    match t {
        Int(val) => Mov(RAX, Value(val)),
        Bool(val) => Mov(RAX, Value(val as i64)), // TODO: Implement 8 bit registers and use AL instead of RAX
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
            Bool(_) => 1,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum InterRep {
    Define(String, Type),
    CCall(String, Vec<Type>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum StackDirective {
    Variable(String, Type),
    Temp(Type),
    BasePointer,
}

use InterRep::*;
use StackDirective::*;
use Type::*;

impl StackDirective {
    pub fn byte_size(&self) -> u64 {
        use StackDirective::*;

        match self {
            Variable(_, t) => t.byte_size(),
            Temp(t) => t.byte_size(),
            BasePointer => 8,
        }
    }
}

pub struct Generator {
    stack: Vec<StackDirective>,
    bp: u64,
    functions: HashMap<String, Vec<Instr>>,
}

impl Generator {
    pub fn new() -> Self {
        use StackDirective::*;

        let mut initial = HashMap::new();

        initial.insert("main".to_string(), vec![Push(Reg(RBP)), Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![BasePointer],
            functions: initial.clone(),
            bp: 0,
        }
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
                Variable(_, t) | Temp(t) => push_type(t),
            })
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<Type>) {
        let registers = vec![R(9), R(8), RCX, RDX, RSI, RDI];

        for (i, t) in parameters.iter().enumerate().rev() {
            if i > registers.len() - 1 {
                self.push(function, Temp(t.clone()));
            } else {
                self.functions
                    .get_mut(&function.to_string())
                    .unwrap()
                    .push(mov_type(registers[i].clone(), t.clone()))
            }
        }

        self.functions.get_mut(&function.to_string()).unwrap()
    }
}
