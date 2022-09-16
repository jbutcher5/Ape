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

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Int(i64),
    Bool(bool),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Node {
    Literal(Type),
    Ident(String),
    Str(String),
}

#[derive(Clone, Eq, PartialEq, Debug)]
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
pub enum IR {
    Define(String, Node),
    CCall(String, Vec<Node>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum StackDirective {
    Variable(String, Type),
    Str(String, usize),
    TempLiteral(Type),
    TempReg(Register),
    Empty(u64),
    BasePointer,
}

use StackDirective::*;
use Type::*;
use IR::*;

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

#[derive(Clone)]
pub struct Generator {
    stack: Vec<StackDirective>,
    bp: usize,
    functions: HashMap<String, Vec<Instr>>,
    data: Vec<String>,
}

impl Default for Generator {
    fn default() -> Self {
        let mut initial = HashMap::new();
        initial.insert("main".to_string(), vec![Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![],
            bp: 0,
            functions: initial.clone(),
            data: vec![],
        }
    }
}

impl Generator {
    pub fn get_variable(&self, name: String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut t: Option<Type> = None;

        for x in &self.stack[self.bp..] {
            if let Str(var, ident) = x {
                if var.clone() == name {
                    return Some(Data(*ident));
                }
            } else if let Variable(var, t2) = x {
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

    pub fn get_variable_clone(&self, name: String) -> Option<Node> {
        for x in &self.stack[self.bp..] {
            if let Str(var, ident) = x {
                if var.clone() == name {
                    return Some(Node::Str(self.data[*ident].clone()));
                }
            } else if let Variable(var, t) = x {
                if var.clone() == name {
                    return Some(Node::Literal(t.clone()));
                }
            }
        }

        None
    }

    pub fn apply(&mut self, ir: Vec<IR>) {
        for instruction in ir {
            self.handle_ir(instruction);
        }
    }

    fn handle_ir(&mut self, instr: IR) {
        match instr {
            Define(name, value) => self.define("main", name, value),
            CCall(name, parameters) => self.c_call("main", name, parameters),
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

    fn define_node(&mut self, node: &Node) -> Node {
        match node {
            Node::Ident(ident) => self.get_variable_clone(ident.to_owned()).unwrap(),
            Node::Str(s) => {
                self.get_str_index(s.to_string());
                node.clone()
            }
            _ => node.clone(),
        }
    }

    fn get_str_index_mut(&mut self, string: String) -> usize {
        match self.data.iter().position(|r| r.clone() == string) {
            Some(n) => n,
            None => {
                self.data.push(string);
                self.data.len() - 1
            }
        }
    }

    fn get_str_index(&self, string: String) -> Option<usize> {
        self.data.iter().position(|r| r.clone() == string)
    }

    fn node_stack_directive_rec(&mut self, name: String, node: Node) -> StackDirective {
        match node {
            Node::Str(string) => Str(name, self.get_str_index_mut(string)),
            Node::Literal(t) => Variable(name, t),
            Node::Ident(ident) => {
                self.node_stack_directive_rec(name, self.get_variable_clone(ident).unwrap())
            }
        }
    }

    fn define(&mut self, function: &str, name: String, node: Node) {
        let directive = self.node_stack_directive_rec(name, node);

        self.push(function, directive);
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<Node>) {
        const REGSITERS64: [Register; 6] = [RDI, RSI, RCX, RDX, R(8), R(9)];
        let mut cloned_params = vec![];

        for x in &parameters {
            cloned_params.push(match x {
                Node::Ident(ident) => self.get_variable_clone(ident.to_string()).unwrap(),
                _ => x.clone(),
            });
        }

        let parameter_stack_size = if cloned_params.len() > 6 {
            cloned_params[5..]
                .iter()
                .map(|x| match x {
                    Node::Literal(t) => t.byte_size(),
                    Node::Str(_) => 8,
                    _ => panic!(),
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
                        Node::Ident(ident) => {
                            TempReg(self.get_variable(ident.to_string()).unwrap())
                        }
                        Node::Str(string) => {
                            TempReg(Data(self.get_str_index(string.to_string()).unwrap()))
                        }
                        Node::Literal(t) => TempLiteral(t.clone()),
                    },
                );
            } else {
                let set_reg = vec![match x {
                    Node::Ident(ident) => mov_reg(
                        REGSITERS64[i].clone(),
                        self.get_variable(ident.to_string()).unwrap(),
                    ),
                    Node::Literal(t) => mov_type(REGSITERS64[i].clone(), t.clone()),
                    Node::Str(string) => mov_reg(
                        REGSITERS64[i].clone(),
                        Data(self.get_str_index(string.to_string()).unwrap()),
                    ),
                }];
                self.functions
                    .get_mut(&function.to_string())
                    .unwrap()
                    .extend(set_reg)
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
