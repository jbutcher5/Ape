pub mod asm;
pub mod code_gen;
pub mod lexer;
pub mod parser;

use std::collections::HashMap;

use asm::{Register::*, *};
use Type::*;

const REGSITERS64: [Register; 6] = [RDI, RSI, RDX, RCX, R(8), R(9)];
const REGSITERS32: [Register; 6] = [EDI, ESI, EDX, ECX, RD(8), RD(9)];
const REGSITERS16: [Register; 6] = [DI, SI, DX, CX, RW(8), RW(9)];
const REGSITERS8: [Register; 6] = [DIL, SIL, DL, CL, RB(8), RB(9)];

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Str(String),
    Array(Vec<Node>, Option<Type>),
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
    TypedIdent(String, Type),
    Ident(String),
    Bracket(Vec<Node>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Stack {
    Variable(String, Type),
    Empty(u64),
    Allocation(u64),
    BasePointer,
}

#[derive(Debug, Clone)]
pub struct Function {
    signature: FuncSignature,
    body: Vec<Instr>,
}

impl Function {
    fn new(types: Vec<Type>, var_args: bool) -> Self {
        Self {
            signature: FuncSignature::new(types, var_args),
            body: vec![],
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FuncSignature {
    types: Vec<Type>,
    var_args: bool,
}

impl FuncSignature {
    fn new(types: Vec<Type>, var_args: bool) -> Self {
        Self { types, var_args }
    }
}

#[derive(Debug, Clone)]
pub struct Macro {
    pattern: Node,
    body: Vec<Node>,
}

impl From<&Literal> for Type {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Int(_) => Int,
            Literal::Bool(_) => Bool,
            Literal::Str(_) => Str,
            Literal::Array(arr, t) => Array(
                arr.len(),
                Box::new(match t {
                    Some(t) => t.clone(),
                    None => panic!("Array has an unkown type"),
                }),
            ),
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

pub(crate) trait ByteSize {
    fn byte_size(&self) -> u64;
}

impl ByteSize for Type {
    fn byte_size(&self) -> u64 {
        match self {
            Void => 0,
            Int => 4,
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

pub fn func_param_stack_alloc(parameters: &Vec<Type>) -> u64 {
    if parameters.len() < 7 {
        0
    } else {
        let stack_params = &parameters[7..];
        stack_params.iter().map(Type::byte_size).sum()
    }
}

pub fn match_macro_pattern(pattern: &Node, node: &Node) -> Option<HashMap<String, Node>> {
    let mut ident_to_node: HashMap<String, Node> = HashMap::new();

    match (pattern, node) {
        (Node::Literal(_), Node::Literal(_)) => Some(ident_to_node),
        (Node::Bracket(pattern_inner), Node::Bracket(node_inner)) => {
            for (pattern, node) in pattern_inner.into_iter().zip(node_inner) {
                ident_to_node.extend(match_macro_pattern(pattern, node)?);
            }

            Some(ident_to_node)
        }
        (Node::Ident(x), y) => {
            if let Node::Ident(y) = y {
                if !x.ends_with("!") {
                    ident_to_node.insert(x.to_string(), Node::Ident(y.to_string()));
                    return Some(ident_to_node);
                } else if x == y {
                    ident_to_node.insert(x.to_string(), Node::Ident(y.to_string()));
                    return Some(ident_to_node);
                } else {
                    return None;
                }
            }

            ident_to_node.insert(x.to_string(), y.clone());
            Some(ident_to_node)
        }
        _ => None,
    }
}

pub fn replace_ident(ident_map: &HashMap<String, Node>, nodes: Vec<Node>) -> Vec<Node> {
    let mut result = vec![];

    for node in nodes {
        result.push(match node {
            Node::Ident(ref ident) => {
                if let Some(node) = ident_map.get(ident) {
                    node.clone()
                } else {
                    node.clone()
                }
            }
            Node::TypedIdent(..) | Node::Literal(_) => node,
            Node::Bracket(nodes) => Node::Bracket(replace_ident(ident_map, nodes)),
        })
    }

    result
}
