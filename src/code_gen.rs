use crate::asm;

use asm::{Instr::*, Operand::*, Register::*, *};
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Int(i64),
    Bool(bool),
    Str(String),
    Array(Vec<Type>),
    Pointer(Register, ReferenceType),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ReferenceType {
    Int,
    Bool,
    Str,
    Array(usize, Box<ReferenceType>),
    Pointer(Box<ReferenceType>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Node {
    Literal(Type),
    Ident(String),
    Ref(String),
    Define(String, Box<Node>),
    CCall(String, Vec<Node>),
}

pub trait ByteSize {
    fn byte_size(&self) -> u64;
}

impl Type {
    pub fn get_ref_type(&self) -> ReferenceType {
        match self {
            Type::Int(_) => Int,
            Type::Bool(_) => Bool,
            Type::Str(_) => Str,
            Type::Array(arr) => Array(arr.len(), Box::new(arr[0].get_ref_type())),
            Type::Pointer(_, t) => t.clone(),
        }
    }
}

impl ByteSize for ReferenceType {
    #[inline]
    fn byte_size(&self) -> u64 {
        match self {
            Int => 8,
            Bool => 1,
            Str => 8,
            Array(length, t) => (*length as u64) * t.byte_size(),
            Pointer(t) => t.byte_size(),
        }
    }
}

impl ByteSize for Type {
    #[inline]
    fn byte_size(&self) -> u64 {
        self.get_ref_type().byte_size()
    }
}

impl ByteSize for VariableContent {
    #[inline]
    fn byte_size(&self) -> u64 {
        self.type_ref().byte_size()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VariableContent {
    Ident(String, ReferenceType),
    Literal(ReferenceType),
}

impl VariableContent {
    #[inline]
    fn type_ref(&self) -> &ReferenceType {
        match self {
            Self::Literal(t) => t,
            Self::Ident(_, t) => t,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum StackDirective {
    Variable(String, VariableContent),
    TempLiteral(VariableContent),
    TempReg(Register),
    Empty(u64),
    BasePointer,
}

use Node::*;
use ReferenceType::*;

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
    fn push_type(&mut self, t: &Type, stack_offset: u64) -> Vec<Instr> {
        let stack_size: u64 = self.stack_size_from_rbp() + stack_offset;
        match t {
            Type::Int(val) => vec![
                Sub(RSP, Value(8.to_string())),
                Mov(Stack(-(stack_size as i64), 8), Value(val.to_string())),
            ],
            Type::Bool(val) => vec![
                Sub(RSP, Value(1.to_string())),
                Mov(
                    Stack(-(stack_size as i64), 1),
                    Value((val.clone() as i64).to_string()),
                ),
            ],
            Type::Str(val) => {
                let index = self.get_str_index_mut(val);
                vec![
                    Sub(RSP, Value(8.to_string())),
                    Mov(Stack(-(stack_size as i64), 8), Reg(Data(index))),
                ]
            }
            Type::Array(array) => array
                .iter()
                .enumerate()
                .map(|(i, element)| self.push_type(element, (i as u64) * element.byte_size()))
                .collect::<Vec<Vec<Instr>>>()
                .concat(),
            Type::Pointer(r, _) => vec![
                vec![Sub(RSP, Value(8.to_string()))],
                reference_reg(Stack(-(stack_size as i64), 8), r.clone()),
            ]
            .concat(),
        }
    }

    #[inline]
    fn mov_type(&mut self, r: Register, t: Type) -> Instr {
        match t {
            Type::Int(val) => Mov(r, Value(val.to_string())),
            Type::Bool(val) => Mov(r, Value((val as i64).to_string())),
            Type::Str(val) => {
                let index = self.get_str_index_mut(&val);
                Mov(r, Reg(Data(index)))
            }
            _ => panic!("The type {:?} can not be copied.", t),
        }
    }

    pub fn get_address(&self, name: String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut size: Option<u64> = None;

        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, var_type) = x {
                if var.clone() == name {
                    size = Some(var_type.byte_size());
                    break;
                } else {
                    acc -= self.directive_byte_size(x) as i64;
                }
            } else {
                acc -= self.directive_byte_size(x) as i64;
            }
        }

        Some(Stack(acc, size?))
    }

    pub fn get_address_reference(&self, name: String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut size: Option<u64> = None;

        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, var_type) = x {
                if var.clone() == name {
                    size = Some(match var_type {
                        VariableContent::Literal(Array(_, _)) => 8,
                        _ => var_type.byte_size(),
                    });
                    break;
                } else {
                    acc -= self.directive_byte_size(x) as i64;
                }
            } else {
                acc -= self.directive_byte_size(x) as i64;
            }
        }

        Some(Stack(acc, size?))
    }

    pub fn get_reference_type(&self, name: &String) -> Option<ReferenceType> {
        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(ident, node) = x {
                if ident == name {
                    return Some(node.type_ref().clone());
                }
            }
        }

        None
    }

    fn node_as_var_content(&self, node: &Node) -> Option<VariableContent> {
        Some(match node {
            Node::Literal(t) => VariableContent::Literal(t.get_ref_type().clone()),
            Node::Ident(ident) => {
                VariableContent::Ident(ident.to_string(), self.get_reference_type(ident)?)
            }
            Node::Ref(ident) => {
                VariableContent::Literal(Pointer(Box::new(self.get_reference_type(ident)?)))
            }
            _ => panic!(), // TODO: Handle Define and CCall here
        })
    }

    pub fn apply(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.handle_node(node);
        }
    }

    fn handle_node(&mut self, node: Node) {
        if let Node::Define(name, value) = node {
            self.define("main", name, *value);
        } else if let Node::CCall(name, parameters) = node {
            self.c_call("main", name, parameters);
        }
    }

    fn stack_size_from_rbp(&self) -> u64 {
        let mut acc = 0;

        for x in self.stack.iter().rev() {
            if *x == StackDirective::BasePointer {
                break;
            } else if let StackDirective::Variable(_, VariableContent::Literal(Array(len, t))) = x {
                acc += t.byte_size() * *len as u64;
            } else {
                acc += self.directive_byte_size(x);
            }
        }

        acc
    }

    fn get_str_index_mut(&mut self, string: &String) -> usize {
        match self.data.iter().position(|r| r == string) {
            Some(n) => n,
            None => {
                self.data.push(string.to_string());
                self.data.len() - 1
            }
        }
    }

    #[inline]
    fn node_byte_size(&self, node: &Node) -> u64 {
        self.node_as_var_content(node).unwrap().byte_size()
    }

    #[inline]
    fn directive_byte_size(&self, directive: &StackDirective) -> u64 {
        match directive {
            StackDirective::TempLiteral(t) => t.byte_size(),
            StackDirective::Variable(_, content) => content.byte_size(),
            StackDirective::TempReg(r) => r.byte_size(),
            StackDirective::BasePointer => 8,
            StackDirective::Empty(n) => *n,
        }
    }

    #[inline]
    fn define(&mut self, function: &str, name: String, node: Node) {
        let asm: (Vec<Instr>, StackDirective) = match node {
            Literal(ref t) => (
                self.push_type(&t, 0),
                StackDirective::Variable(name, self.node_as_var_content(&node).unwrap()),
            ),
            Ident(ref ident) => {
                let reg = self.get_address_reference(ident.to_string()).unwrap();
                (
                    push_reg(reg, self.stack_size_from_rbp()),
                    StackDirective::Variable(
                        name,
                        match self.get_reference_type(&ident).unwrap() {
                            Array(_, t) => VariableContent::Literal(Pointer(t)),
                            t => VariableContent::Ident(ident.to_string(), t),
                        },
                    ),
                )
            }
            Ref(ref ident) => {
                let var_content = self.node_as_var_content(&node).unwrap();
                (
                    self.push_type(
                        &Type::Pointer(
                            self.get_address(ident.to_string()).unwrap(),
                            var_content.type_ref().clone(),
                        ),
                        0,
                    ),
                    StackDirective::Variable(name, var_content),
                )
            }
            CCall(c_func, parameters) => {
                self.c_call(function, c_func, parameters);
                (
                    push_reg(RAX, self.stack_size_from_rbp()),
                    StackDirective::Variable(name, VariableContent::Literal(Int)),
                ) // TODO: Determine return type of each C function
            }
            _ => panic!("Node can not be used in a define."),
        };

        self.stack.push(asm.1);
        self.functions.get_mut(function).unwrap().extend(asm.0);
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<Node>) {
        const REGSITERS64: [Register; 6] = [RDI, RSI, RCX, RDX, R(8), R(9)];
        let mut cloned_params_size = vec![];

        for x in &parameters {
            cloned_params_size.push(self.node_byte_size(x)); //TODO: C calls will error here as their result type size is unknown
        }

        let parameter_stack_size = if cloned_params_size.len() > 6 {
            cloned_params_size[5..].iter().sum::<u64>()
        } else {
            8
        };

        let new_stack_size = parameter_stack_size + self.stack_size_from_rbp();
        let next_multiple_of_16 = next_aligned_stack(new_stack_size);

        if new_stack_size != next_multiple_of_16 {
            let n = next_multiple_of_16 - new_stack_size;

            self.stack.push(StackDirective::Empty(n));

            self.functions
                .get_mut(function)
                .unwrap()
                .push(Sub(RSP, Value(n.to_string())))
        }

        for (i, x) in parameters.iter().enumerate().rev() {
            if i > 5 {
                let asm: (Vec<Instr>, StackDirective) = match x {
                    Node::Literal(t) => (
                        self.push_type(t, 0),
                        StackDirective::TempLiteral(self.node_as_var_content(x).unwrap()),
                    ),
                    Node::Ident(ident) => {
                        let reg = self.get_address(ident.to_string()).unwrap();
                        (
                            push_reg(reg.clone(), self.stack_size_from_rbp()),
                            StackDirective::TempReg(reg),
                        )
                    }
                    Node::Ref(ident) => {
                        let var_content = self.node_as_var_content(x).unwrap();
                        (
                            self.push_type(
                                &Type::Pointer(
                                    self.get_address(ident.to_string()).unwrap(),
                                    var_content.type_ref().clone(),
                                ),
                                0,
                            ),
                            StackDirective::TempLiteral(var_content),
                        )
                    }
                    CCall(c_func, parameters) => {
                        self.c_call(function, c_func.to_string(), parameters.to_vec());
                        (
                            push_reg(RAX, self.stack_size_from_rbp()),
                            StackDirective::TempLiteral(VariableContent::Literal(Int)),
                        ) // TODO: Determine return type of each C function
                    }
                    _ => panic!("Node can not be used as a parameter."),
                };

                self.stack.push(asm.1);
                self.functions.get_mut(function).unwrap().extend(asm.0);
            } else {
                let set_reg = match x {
                    Node::Ident(ident) => match self.get_reference_type(&ident).unwrap() {
                        Array(..) => reference_reg(
                            REGSITERS64[i].clone(),
                            self.get_address_reference(ident.to_string()).unwrap(),
                        ),
                        _ => vec![mov_reg(
                            REGSITERS64[i].clone(),
                            self.get_address_reference(ident.to_string()).unwrap(),
                        )],
                    },
                    Node::Literal(t) => vec![self.mov_type(REGSITERS64[i].clone(), t.clone())],
                    Node::Ref(ident) => reference_reg(
                        REGSITERS64[i].clone(),
                        self.get_address(ident.to_string()).unwrap(),
                    ),
                    CCall(c_func, parameters) => {
                        self.c_call(function, c_func.to_string(), parameters.to_vec());
                        vec![mov_reg(REGSITERS64[i].clone(), RAX)] // TODO: Determine return type of each C function
                    }
                    _ => panic!("Node can not be used as a parameter."),
                };
                self.functions
                    .get_mut(&function.to_string())
                    .unwrap()
                    .extend(set_reg)
            }
        }

        self.functions
            .get_mut(&function.to_string())
            .unwrap()
            .extend(vec![Mov(RAX, Value(0.to_string())), Call(c_func)]);
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
                buffer.extend(
                    match instr {
                        DefineLabel(_) => format!("{}\n", instr.to_string()),
                        _ => format!("    {}\n", instr.to_string()),
                    }
                    .as_bytes(),
                )
            }
        }

        buffer.extend(b"\nsection .data\n");
        for (i, v) in self.data.iter().enumerate() {
            buffer.extend(format!("    s{}: db `{}`\n", i, v).as_bytes());
        }

        buffer
    }
}
