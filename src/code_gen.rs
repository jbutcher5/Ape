use crate::asm;

use asm::{Instr::*, Operand::*, Register::*, *};
use std::collections::HashMap;
use Type::*;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Str(String),
    Array(Vec<Literal>, Type),
    Pointer(Register, Type),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Int,
    Bool,
    Str,
    Void,
    Array(usize, Box<Type>),
    Pointer(Box<Type>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Node {
    Literal(Literal),
    Ident(String),
    Ref(Box<Node>),
    Deref(Box<Node>),
    Define(Box<Node>, Box<Node>),
    Call(Box<Node>, Vec<Node>),
    Extern(String, Vec<Type>),
    If(Box<Node>, Vec<Node>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Stack {
    Variable(String, Type),
    Empty(u64),
    Allocation(u64),
    BasePointer,
}

impl From<&Literal> for Type {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Int(_) => Int,
            Literal::Bool(_) => Bool,
            Literal::Str(_) => Str,
            Literal::Array(arr, t) => Array(arr.len(), Box::new(t.clone())),
            Literal::Pointer(_, t) => t.clone(),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(ident: &str) -> Result<Type, ()> {
        Ok(match ident {
            "Int" => Self::Int,
            "Bool" => Self::Bool,
            "Str" => Self::Str,
            "Void" => Self::Void,
            _ => {
                if ident.get(0..1).ok_or(())? == "["
                    && ident.get(ident.len() - 1..ident.len()).ok_or(())? == "]"
                {
                    let split = ident.find(';').ok_or(())?;

                    let len: usize = ident
                        .get(split..(ident.len()) - 1)
                        .ok_or(())?
                        .parse()
                        .unwrap();
                    let t: Type = Self::try_from(ident.get(1..split).ok_or(())?)?;

                    Self::Array(len, Box::new(t))
                } else if ident.get(0..1).ok_or(())? == "&" {
                    let t: Type = Self::try_from(ident.get(1..).ok_or(())?)?;

                    Self::Pointer(Box::new(t))
                } else {
                    return Err(());
                }
            }
        })
    }
}

pub trait ByteSize {
    fn byte_size(&self) -> u64;
}

impl ByteSize for Type {
    fn byte_size(&self) -> u64 {
        match self {
            Int | Void => 8,
            Bool => 1,
            Str => 8,
            Array(length, t) => (*length as u64) * t.byte_size(),
            Pointer(_) => 8,
        }
    }
}

impl ByteSize for Literal {
    fn byte_size(&self) -> u64 {
        Type::from(self).byte_size()
    }
}

impl ByteSize for Stack {
    fn byte_size(&self) -> u64 {
        match self {
            Self::BasePointer => 8,
            Self::Empty(n) => *n,
            Self::Variable(_, t) => t.byte_size(),
            Self::Allocation(n) => *n,
        }
    }
}

#[derive(Debug)]
pub struct Generator {
    stack: Vec<Stack>,
    externs: HashMap<String, Vec<Type>>,
    functions: HashMap<String, Vec<Instr>>,
    data: Vec<String>,
}

impl Default for Generator {
    fn default() -> Self {
        let mut initial = HashMap::new();
        initial.insert("main".to_string(), vec![Mov(RBP, Reg(RSP))]);

        Self {
            stack: vec![],
            externs: HashMap::new(),
            functions: initial.clone(),
            data: vec![],
        }
    }
}

impl Generator {
    fn scope_size(&self) -> u64 {
        let mut acc = 0;

        for x in self.stack.iter().rev() {
            match x {
                &Stack::BasePointer => break,
                _ => acc += x.byte_size(),
            }
        }

        acc
    }

    fn base_pointer(&self) -> Option<usize> {
        if self.stack.is_empty() {
            Some(
                self.stack
                    .iter()
                    .enumerate()
                    .rev()
                    .find(|&x| matches!(x, (_, &Stack::BasePointer)))?
                    .0,
            )
        } else {
            Some(0)
        }
    }

    fn get_string(&mut self, string: String) -> usize {
        if let Some(i) = self.data.iter().position(|x| x == &string) {
            i
        } else {
            self.data.push(string);
            self.data.len() - 1
        }
    }

    fn get_variable(&self, ident: &String) -> Option<(Register, Type)> {
        let mut acc: i64 = 0;

        for x in &self.stack[self.base_pointer()?..] {
            if let Stack::Variable(name, t) = x {
                if ident == name {
                    return Some((Stack(acc, t.byte_size()), t.clone()));
                } else {
                    acc -= x.byte_size() as i64;
                }
            } else {
                acc -= x.byte_size() as i64;
            }
        }

        None
    }

    fn move_literal(
        &mut self,
        register: Register,
        literal: &Literal,
    ) -> Result<Vec<Instr>, String> {
        use Literal::*;

        match literal {
            Int(x) => Ok(vec![Mov(register, Value(x.to_string()))]),
            Bool(x) => Ok(vec![Mov(register, Value((*x as i32).to_string()))]),
            Str(string) => {
                let index = self.get_string(string.clone());

                Ok(vec![Mov(register, Reg(Data(index)))])
            }
            Array(array, t) => {
                let mut instructions = vec![];

                self.stack
                    .push(Stack::Allocation(array.len() as u64 * t.byte_size()));

                let mut bp_offset = self.scope_size();
                let first = bp_offset;
                for literal in array {
                    if Type::from(literal) != *t {
                        return Err(format!(
                            "Array has type of {:?} but a literal has a type of {:?}",
                            t,
                            Type::from(literal)
                        ));
                    }

                    instructions.extend(
                        self.move_literal(Stack(bp_offset as i64, t.byte_size()), literal)?,
                    );
                    bp_offset += t.byte_size();
                }

                instructions.push(Lea(RAX, Stack(first as i64, t.byte_size())));
                Ok(instructions)
            }
            _ => todo!(),
        }
    }

    fn handle_ident(&mut self, ident: &String, function: &String) -> Result<Type, String> {
        let (address, t) = self
            .get_variable(ident)
            .ok_or(format!("Unkown identifier `{ident}`"))?;

        let instructions = match t {
            Int => mov_reg(
                match t.byte_size() {
                    1 => AL,
                    2 => AX,
                    4 => EAX,
                    8 => RAX,
                    _ => {
                        return Err(format!(
                            "Unknown register for byte size `{}`",
                            t.byte_size()
                        ))
                    }
                },
                address,
            ),
            _ => todo!(),
        };

        self.functions
            .get_mut(function)
            .ok_or(format!("Unknown function called `{function}`"))?
            .push(instructions);

        Ok(t) // TODO: If t is an array this should become a pointer
    }

    fn handle_define(
        &mut self,
        function: &String,
        ident: String,
        node: Node,
    ) -> Result<Type, String> {
        use Type::*;

        let t = self.consume_node(function, node)?;

        let instructions = match t {
            Int | Bool | Pointer(_) => vec![Mov(
                Stack(-(self.scope_size() as i64), t.byte_size()),
                Reg(match t.byte_size() {
                    1 => AL,
                    2 => AX,
                    4 => EAX,
                    8 => RAX,
                    _ => {
                        return Err(format!(
                            "Unknown register for byte size `{}`",
                            t.byte_size()
                        ))
                    }
                }),
            )],
            _ => todo!(),
        };

        self.functions
            .get_mut(function)
            .ok_or(format!("Unknown function called `{function}`"))?
            .extend(instructions);
        self.stack.push(Stack::Variable(ident, t));
        Ok(Void)
    }

    fn consume_node(&mut self, function: &String, node: Node) -> Result<Type, String> {
        match node {
            Node::Literal(literal) => {
                let instructions = self.move_literal(RAX, &literal)?;

                self.functions
                    .get_mut(function)
                    .ok_or(format!("Unknown function called `{function}`"))?
                    .extend(instructions);
                Ok(Type::from(&literal))
            }
            Node::Ident(ident) => self.handle_ident(&ident, function),
            Node::Define(ident_node, node) => {
                if let Node::Ident(ident) = *ident_node {
                    self.handle_define(function, ident, *node)
                } else {
                    Err("Define's can only assign to identifiers".to_string())
                }
            }
            Node::Ref(node) => match *node {
                Node::Ident(ident) => match self.get_variable(&ident) {
                    Some((address, t)) => {
                        self.functions
                            .get_mut(function)
                            .ok_or(format!("Unknown function called `{function}`"))?
                            .push(Lea(RAX, address));
                        Ok(Type::Pointer(Box::new(t)))
                    }
                    None => Err(format!("Unkown identifier `{ident}`")),
                },
                _ => Err("Can only take the reference of an identifier".to_string()),
            },
            _ => todo!(),
        }
    }

    pub fn apply(&mut self, nodes: Vec<Node>) -> Result<(), String> {
        for node in nodes {
            self.consume_node(&"main".to_string(), node)?;
        }

        Ok(())
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
