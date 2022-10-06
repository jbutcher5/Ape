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
pub enum IRNode {
    Literal(Type),
    Ident(String),
    Ref(String),
    Define(String, Box<IRNode>),
    CCall(String, Vec<IRNode>),
    Extern(String, Vec<ReferenceType>),
    If(Box<IRNode>, Vec<IRNode>),
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

use IRNode::*;
use ReferenceType::*;

#[derive(Clone)]
pub struct Generator {
    stack: Vec<StackDirective>,
    bp: usize,
    externs: HashMap<String, Vec<ReferenceType>>,
    functions: HashMap<String, Vec<Instr>>,
    data: Vec<String>,
    next_branch: u64,
}

impl Default for Generator {
    fn default() -> Self {
        let mut initial = HashMap::new();
        initial.insert("main".to_string(), vec![Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![],
            bp: 0,
            externs: HashMap::new(),
            functions: initial.clone(),
            data: vec![],
            next_branch: 0,
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
                    Value((*val as i64).to_string()),
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
            Type::Pointer(addr, _) => Lea(r, addr),
            _ => panic!("The type {:?} can not be copied.", t),
        }
    }

    pub fn get_address(&self, name: &String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut size: Option<u64> = None;

        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, var_type) = x {
                if var == name {
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

    pub fn get_address_reference(&self, name: &String) -> Option<Register> {
        let mut acc: i64 = 0;
        let mut size: Option<u64> = None;

        for x in &self.stack[self.bp..] {
            if let StackDirective::Variable(var, var_type) = x {
                if var == name {
                    size = Some(match var_type {
                        VariableContent::Literal(Array(_, t))
                        | VariableContent::Ident(_, Array(_, t)) => t.byte_size(),
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

    fn node_as_var_content(&self, node: &IRNode) -> Option<VariableContent> {
        Some(match node {
            Literal(t) => VariableContent::Literal(t.get_ref_type()),
            Ident(ident) => {
                VariableContent::Ident(ident.to_string(), self.get_reference_type(ident)?)
            }
            Ref(ident) => {
                VariableContent::Literal(Pointer(Box::new(self.get_reference_type(ident)?)))
            }
            _ => panic!("Impossible to get var content from a {:?}", node),
        })
    }

    pub fn apply(&mut self, nodes: Vec<IRNode>) {
        for node in nodes {
            self.handle_node(node);
        }
    }

    fn handle_node(&mut self, node: IRNode) {
        if let Define(name, value) = node {
            self.define("main", name, *value);
        } else if let CCall(name, parameters) = node {
            self.c_call("main", name, parameters);
        } else if let Extern(name, param_types) = node {
            self.externs.insert(name, param_types);
        } else if let If(precondition, nodes) = node {
            self.if_statement("main", *precondition, nodes);
        }
    }

    fn eval_node(&self, node: &IRNode) -> IRNode {
        match node {
            Ref(ident) => Literal(Type::Pointer(
                self.get_address_reference(ident).unwrap(),
                self.get_reference_type(ident).unwrap(),
            )),
            _ => node.clone(),
        }
    }

    fn node_type(&self, node: &IRNode) -> Option<ReferenceType> {
        Some(match node {
            Literal(t) => t.get_ref_type(),
            Ident(ident) => self.get_reference_type(ident)?,
            Ref(t) => Pointer(Box::new(match self.get_reference_type(t)? {
                Array(_, t) => *t,
                t => t,
            })),
            CCall(ident, _) => self.externs.get(ident)?.get(0)?.clone(),
            _ => return None,
        })
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
    fn node_byte_size(&self, node: &IRNode) -> u64 {
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

    fn if_statement(&mut self, function: &str, precondition: IRNode, nodes: Vec<IRNode>) {
        let (asm, label): (Vec<Instr>, Label) = match precondition {
            Literal(Type::Bool(value)) => (
                vec![
                    Cmp(Value((value as i64).to_string()), Value(0.to_string())),
                    Je(Label::Numbered(self.next_branch)),
                ],
                Label::Numbered(self.next_branch),
            ),
            Ident(ident) => match self.get_reference_type(&ident) {
                Some(Bool | Int) => {
                    let address = self.get_address(&ident).unwrap();
                    let label = Label::Numbered(self.next_branch);

                    (
                        vec![Cmp(Reg(address), Value(0.to_string())), Je(label.clone())],
                        label,
                    )
                }
                Some(_) => panic!("Invalid type to be used in an if statement"),
                None => panic!("Unknown identifier {ident}"),
            },
            _ => panic!("Invalid node to be used as a precondition."),
        };
        self.next_branch += 1;

        self.functions.get_mut(function).unwrap().extend(asm);
        self.apply(nodes);
        self.functions
            .get_mut(function)
            .unwrap()
            .push(DefineLabel(label));
    }

    fn define(&mut self, function: &str, name: String, mut node: IRNode) {
        node = self.eval_node(&node);

        let (asm, directive): (Vec<Instr>, StackDirective) = match node {
            Literal(ref t) => (
                self.push_type(t, 0),
                StackDirective::Variable(name, self.node_as_var_content(&node).unwrap()),
            ),
            Ident(ref ident) => (
                match self.get_reference_type(ident).unwrap() {
                    Array(len, t) => {
                        let bytesize = t.byte_size();
                        let position = if let Some(Stack(x, _)) = self.get_address(ident) {
                            x
                        } else {
                            panic!()
                        };
                        let mut asm_buffer: Vec<Instr> = vec![];

                        for i in 0..len {
                            asm_buffer.extend(push_reg(
                                Stack(position - bytesize as i64 * i as i64, bytesize),
                                self.stack_size_from_rbp() + bytesize * i as u64,
                            ));
                        }

                        asm_buffer
                    }
                    _ => {
                        let reg = self.get_address(ident).unwrap();
                        push_reg(reg, self.stack_size_from_rbp())
                    }
                },
                StackDirective::Variable(
                    name,
                    VariableContent::Ident(
                        ident.to_string(),
                        self.get_reference_type(ident).unwrap(),
                    ),
                ),
            ),
            CCall(c_func, parameters) => {
                self.c_call(function, c_func, parameters);
                (
                    push_reg(RAX, self.stack_size_from_rbp()),
                    StackDirective::Variable(name, VariableContent::Literal(Int)),
                ) // TODO: Determine return type of each C function
            }
            _ => {
                self.handle_node(node.clone());
                (
                    self.push_type(&Type::Bool(true), 0),
                    StackDirective::Variable(
                        name,
                        self.node_as_var_content(&Literal(Type::Bool(true)))
                            .unwrap(),
                    ),
                )
            }
        };

        self.stack.push(directive);
        self.functions.get_mut(function).unwrap().extend(asm);
    }

    fn c_call(&mut self, function: &str, c_func: String, parameters: Vec<IRNode>) {
        const REGSITERS64: [Register; 6] = [RDI, RSI, RCX, RDX, R(8), R(9)];
        let mut cloned_params_size = vec![];

        let parameter_types: Vec<ReferenceType> = match self.externs.get(&c_func) {
            Some(types) => types.clone()[1..].to_vec(),
            None => panic!("No function named {c_func}"),
        };

        for x in &parameters {
            cloned_params_size.push(self.node_byte_size(x)); // TODO: C calls will error here as their result type size is unknown
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
            if let Some(expected_type) = parameter_types.get(i) {
                if expected_type != &self.node_type(x).unwrap() {
                    panic!("Incorrect type given to {c_func}");
                }
            } else {
                panic!("Too many parameters given to {c_func}.");
            }

            let node = self.eval_node(x);
            if i > 5 {
                let asm: (Vec<Instr>, StackDirective) = match node {
                    Literal(ref t) => (
                        self.push_type(t, 0),
                        StackDirective::TempLiteral(self.node_as_var_content(&node).unwrap()),
                    ),
                    Ident(ident) => {
                        let reg = self.get_address(&ident).unwrap();
                        (
                            push_reg(reg.clone(), self.stack_size_from_rbp()),
                            StackDirective::TempReg(reg),
                        )
                    }
                    CCall(c_func_call, parameters) => {
                        self.c_call(function, c_func_call.to_string(), parameters.to_vec());
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
                let set_reg = match node {
                    Ident(ident) => match self.get_reference_type(&ident).unwrap() {
                        Array(_, _) => reference_reg(
                            REGSITERS64[i].clone(),
                            self.get_address_reference(&ident).unwrap(),
                        ),
                        _ => {
                            vec![mov_reg(
                                REGSITERS64[i].clone(),
                                self.get_address_reference(&ident).unwrap(),
                            )]
                        }
                    },
                    Literal(t) => {
                        vec![self.mov_type(REGSITERS64[i].clone(), t.clone())]
                    }
                    CCall(c_func_call, parameters) => {
                        self.c_call(function, c_func_call.to_string(), parameters.to_vec());
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

        for name in self.externs.keys() {
            buffer.extend(format!("extern {name}\n").as_bytes());
        }

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
