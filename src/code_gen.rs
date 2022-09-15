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
fn push_type(t: Type, stack_size: u64) -> Vec<Instr> {
    match t {
        Int(val) => vec![Mov(RAX, Value(val)), Push(Reg(RAX))],
        Bool(val) => vec![
            Sub(RSP, Value(1)),
            Mov(Stack(-(stack_size as i64), 1), Value(val as i64)),
        ],
    }
}

#[inline]
fn push_reg(r: Register, size: u64, stack_size: u64) -> Vec<Instr> {
    match size {
        1 => vec![
            Mov(AL, Reg(r)),
            Sub(RSP, Value(1)),
            Mov(Stack(-(stack_size as i64), 1), Reg(AL)),
        ],
        2 => vec![Mov(AX, Reg(r)), Push(Reg(AX))],
        4 => vec![Mov(EAX, Reg(r)), Push(Reg(EAX))],
        _ => vec![Mov(RAX, Reg(r)), Push(Reg(RAX))],
    }
}

#[inline]
fn mov_reg(to: Register, from: Register) -> Instr {
    match (to.byte_size(), from.byte_size()) {
        (8, 1) => Movzx(to, from),
        _ => Mov(to, Reg(from)),
    }
}

#[inline]
fn mov_type(r: Register, t: Type) -> Instr {
    match t {
        Int(val) => Mov(r, Value(val)),
        Bool(val) => Mov(r, Value(val as i64)),
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Int(i64),
    Bool(bool),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Node {
    Literal(Type),
    Ident(String),
    Str(String),
}

#[derive(Clone, PartialEq, Debug)]
enum NodeDefined {
    Literal(Type),
    Var(Register),
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

#[derive(Clone, Debug)]
pub enum InterRep {
    Define(String, Node),
    CCall(String, Vec<Node>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum StackDirective {
    Variable(String, Type),
    Str(String, String),
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
            TempReg(r) => r.byte_size(),
            BasePointer => 8,
            Empty(n) => *n,
            Str(..) => 0,
        }
    }
}

pub struct Generator {
    stack: Vec<StackDirective>,
    bp: usize,
    functions: HashMap<String, Vec<Instr>>,
    data: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        let mut initial = HashMap::new();

        initial.insert("main".to_string(), vec![Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![],
            bp: 0,
            functions: initial.clone(),
            data: vec![],
        }
    }

    pub fn get_variable(&self, name: String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut t: Option<Type> = None;

        for x in &self.stack[self.bp..] {
            if let Str(var, ident) = x {
                if var.clone() == name {
                    return Some(Data(ident.to_string()));
                }
            }
            if let Variable(var, t2) = x {
                acc -= x.byte_size() as i64;
                if var.clone() == name {
                    t = Some(t2.clone());
                    break;
                }
            } else {
                acc -= x.byte_size() as i64;
            }
        }

        Some(Stack(acc, t?.byte_size()))
    }

    pub fn apply(&mut self, ir: Vec<InterRep>) {
        for instruction in ir {
            self.handle_ir(instruction);
        }
    }

    fn handle_ir(&mut self, instr: InterRep) {
        match instr {
            Define(name, value) => {
                let (ident, s_val) = self.define_node(&value);

                if let Some(string_value) = s_val {
                    self.data.push(string_value);
                }

                self.define("main", name, ident)
            }
            CCall(name, parameters) => {
                let mut defined_nodes = vec![];

                for x in &parameters {
                    let (ident, s_val) = self.define_node(&x);

                    if let Some(string_value) = s_val {
                        self.data.push(string_value)
                    }

                    defined_nodes.push(ident);
                }

                self.c_call("main", name, defined_nodes)
            }
        }
    }

    fn push(&mut self, function: &str, directive: StackDirective) {
        self.stack.push(directive.clone());
        let scope_size = self.stack_size_from_rbp();

        self.functions
            .get_mut(&function.to_string())
            .unwrap()
            .extend(match directive {
                BasePointer => vec![Push(Reg(RBP))],
                Variable(_, t) | TempLiteral(t) => push_type(t, scope_size),
                TempReg(r) => push_reg(r.clone(), r.byte_size(), scope_size),
                Empty(n) => vec![Sub(RSP, Value(n as i64))],
                _ => vec![],
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

    fn define_node(&mut self, node: &Node) -> (NodeDefined, Option<String>) {
        match node {
            Node::Literal(t) => (NodeDefined::Literal(t.clone()), None),
            Node::Ident(ident) => (
                NodeDefined::Var(self.get_variable(ident.to_owned()).unwrap()),
                None,
            ),
            Node::Str(s) => match self.data.iter().position(|r| r == s) {
                Some(n) => (NodeDefined::Var(Data(format!("s{}", n))), None),
                None => (
                    NodeDefined::Var(Data(format!("s{}", self.data.len()))),
                    Some(s.to_owned()),
                ),
            },
        }
    }

    fn define(&mut self, function: &str, name: String, value: NodeDefined) {
        use NodeDefined::*;

        let directive = match value {
            Var(Register::Data(ident)) => Str(name.to_owned(), ident),
            Literal(t) => Variable(name, t),
            _ => panic!(),
        };

        self.push(function, directive);
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<NodeDefined>) {
        const REGSITERS64: [Register; 6] = [RDI, RSI, RCX, RDX, R(8), R(9)];

        let parameter_stack_size = if parameters.len() > 6 {
            parameters[5..]
                .iter()
                .map(|x| match x {
                    NodeDefined::Literal(t) => t.byte_size(),
                    NodeDefined::Var(r) => r.byte_size(),
                })
                .sum::<u64>()
                + 8
        } else {
            8
        };

        let new_stack_size = parameter_stack_size + self.stack_size_from_rbp();
        let next_multiple_of_16 = next_aligned_stack(new_stack_size);

        if new_stack_size != next_multiple_of_16 {
            self.push("main", Empty(next_multiple_of_16 - new_stack_size))
        }

        for (i, x) in parameters.iter().enumerate().rev() {
            if i > 5 {
                self.push(
                    function,
                    match x {
                        NodeDefined::Var(r) => TempReg(r.to_owned()),
                        NodeDefined::Literal(t) => TempLiteral(t.clone()),
                    },
                );
            } else {
                self.functions
                    .get_mut(&function.to_string())
                    .unwrap()
                    .extend(vec![match x {
                        NodeDefined::Var(v) => mov_reg(REGSITERS64[i].clone(), v.to_owned()),
                        NodeDefined::Literal(t) => mov_type(REGSITERS64[i].clone(), t.clone()),
                    }])
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
        for (i, v) in self.data.iter().enumerate() {
            buffer.extend(format!("    s{}: db {}\n", i, v).as_bytes());
        }

        buffer
    }
}
