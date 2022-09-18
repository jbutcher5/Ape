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
fn push_reg(r: Register, stack_size: u64) -> Vec<Instr> {
    let temp_reg = match r.byte_size() {
        1 => AL,
        2 => AX,
        4 => EAX,
        8 => RAX,
        _ => panic!("Unknown register for byte size"),
    };

    vec![
        Sub(RSP, Value(r.byte_size().to_string())),
        Mov(temp_reg.clone(), Reg(r.clone())),
        Mov(Stack(-(stack_size as i64), r.byte_size()), Reg(temp_reg)),
    ]
}

#[inline]
fn mov_reg(to: Register, from: Register) -> Instr {
    match (to.byte_size(), from.byte_size()) {
        (8, 1) => Movzx(to, from),
        _ => Mov(to, Reg(from)),
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Int(i64),
    Bool(bool),
    Str(String),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Node {
    Literal(Type),
    Ident(String),
}

impl Type {
    pub fn byte_size(&self) -> u64 {
        match self {
            Int(_) => 8,
            Bool(_) => 1,
            Str(_) => 8,
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
    Variable(String, Node),
    TempLiteral(Type),
    TempReg(Register),
    Empty(u64),
    BasePointer,
}

use Type::*;
use IR::*;

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
    #[inline]
    fn push_type(&mut self, t: Type, stack_size: u64) -> Vec<Instr> {
        match t {
            Int(val) => vec![Mov(RAX, Value(val.to_string())), Push(Reg(RAX))],
            Bool(val) => vec![
                Sub(RSP, Value(1.to_string())),
                Mov(
                    Stack(-(stack_size as i64), 1),
                    Value((val as i64).to_string()),
                ),
            ],
            Str(val) => {
                let index = self.get_str_index_mut(val);
                vec![
                    Sub(RSP, Value(8.to_string())),
                    Mov(Stack(-(stack_size as i64), 8), Reg(Data(index))),
                ]
            }
        }
    }

    #[inline]
    fn mov_type(&mut self, r: Register, t: Type) -> Instr {
        match t {
            Int(val) => Mov(r, Value(val.to_string())),
            Bool(val) => Mov(r, Value((val as i64).to_string())),
            Str(val) => {
                let index = self.get_str_index_mut(val);
                Mov(r, Reg(Data(index)))
            }
        }
    }

    pub fn get_variable(&self, name: String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut t: Option<u64> = None;

        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, t2) = x {
                acc -= self.directive_byte_size(x) as i64;
                if var.clone() == name {
                    t = Some(self.node_byte_size(t2));
                    break;
                }
            } else {
                acc -= self.directive_byte_size(x) as i64;
            }
        }

        Some(Stack(acc, t?))
    }

    pub fn get_variable_clone(&self, name: String) -> Option<Node> {
        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, node) = x {
                if var.clone() == name {
                    return Some(match node {
                        Node::Literal(_) => node.clone(),
                        Node::Ident(ident) => self.get_variable_clone(ident.to_string())?,
                    });
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
        let asm = match directive {
            StackDirective::BasePointer => vec![Push(Reg(RBP))],
            StackDirective::TempLiteral(t) => self.push_type(t, scope_size),
            StackDirective::Variable(_, node) => match node {
                Node::Literal(t) => self.push_type(t, scope_size),
                Node::Ident(ident) => {
                    let reference = self.get_variable(ident).unwrap();
                    push_reg(reference, scope_size) // TODO: Check this out later
                }
            },
            StackDirective::TempReg(r) => push_reg(r.clone(), scope_size),
            StackDirective::Empty(n) => vec![Sub(RSP, Value((n as i64).to_string()))],
            _ => vec![],
        };

        self.functions
            .get_mut(&function.to_string())
            .unwrap()
            .extend(asm)
    }

    fn stack_size_from_rbp(&self) -> u64 {
        let mut acc = 0;

        for x in self.stack.iter().rev() {
            if *x == StackDirective::BasePointer {
                break;
            }
            acc += self.directive_byte_size(x);
        }

        acc
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

    #[inline]
    fn node_byte_size(&self, node: &Node) -> u64 {
        match node {
            Node::Literal(t) => t.byte_size(),
            Node::Ident(ident) => {
                self.node_byte_size(&self.get_variable_clone(ident.to_string()).unwrap())
            }
        }
    }

    #[inline]
    fn directive_byte_size(&self, directive: &StackDirective) -> u64 {
        match directive {
            StackDirective::TempLiteral(t) => t.byte_size(),
            StackDirective::Variable(_, node) => self.node_byte_size(node),
            StackDirective::TempReg(r) => r.byte_size(),
            StackDirective::BasePointer => 8,
            StackDirective::Empty(n) => *n,
        }
    }

    fn define(&mut self, function: &str, name: String, node: Node) {
        self.push(function, StackDirective::Variable(name, node));
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
            self.push(
                "main",
                StackDirective::Empty(next_multiple_of_16 - new_stack_size),
            )
        }

        for (i, x) in parameters.iter().enumerate().rev() {
            if i > 5 {
                self.push(
                    function,
                    match x {
                        Node::Ident(ident) => {
                            StackDirective::TempReg(self.get_variable(ident.to_string()).unwrap())
                        }
                        Node::Literal(t) => StackDirective::TempLiteral(t.clone()),
                    },
                );
            } else {
                let set_reg = vec![match x {
                    Node::Ident(ident) => mov_reg(
                        REGSITERS64[i].clone(),
                        self.get_variable(ident.to_string()).unwrap(),
                    ),
                    Node::Literal(t) => self.mov_type(REGSITERS64[i].clone(), t.clone()),
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
            .extend(vec![
                Mov(RAX, Value(60.to_string())),
                Mov(RDI, Value(0.to_string())),
                Syscall,
            ]);
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
            buffer.extend(format!("    s{}: db `{}`\n", i, v).as_bytes());
        }

        buffer
    }
}
