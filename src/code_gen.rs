use crate::asm;

use asm::{Instr::*, Operand::*, Register::*, *};
use std::collections::HashMap;
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

pub trait ByteSize {
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

fn func_param_stack_alloc(parameters: &Vec<Type>) -> u64 {
    if parameters.len() < 7 {
        0
    } else {
        let stack_params = &parameters[7..];
        stack_params.iter().map(Type::byte_size).sum()
    }
}

#[derive(Debug, Clone)]
pub struct Generator {
    stack: Vec<Stack>,
    externs: Vec<String>,
    functions: HashMap<String, Function>,
    data: Vec<String>,
}

impl Default for Generator {
    fn default() -> Self {
        let mut main = Function::new(vec![Void], false);
        main.body = vec![Push(Reg(RSP)), Mov(RBP, Reg(RSP))];

        let mut init_func = HashMap::new();
        init_func.insert("main".to_string(), main);

        Self {
            stack: vec![],
            externs: vec![],
            functions: init_func,
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

    fn extend_fn<T>(&mut self, function: &String, iter: T) -> Result<(), String>
    where
        T: IntoIterator<Item = Instr>,
    {
        self.functions
            .get_mut(function)
            .ok_or("Unknown function called `{function}`")?
            .body
            .extend(iter);

        Ok(())
    }

    fn push_fn(&mut self, function: &String, item: Instr) -> Result<(), String> {
        self.functions
            .get_mut(function)
            .ok_or("Unknown function called `{function}`")?
            .body
            .push(item);

        Ok(())
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

    fn deallocate_scope(&mut self) -> Box<[Instr; 3]> {
        let mut acc: u64 = 0;

        while let Some(stack_item) = self.stack.pop() {
            if stack_item != Stack::BasePointer {
                acc += stack_item.byte_size();
            } else {
                break;
            }
        }

        Box::new([Add(RSP, Value(acc.to_string())), Pop(RBP), Return])
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

        for x in &self.stack[self.base_pointer()? + 1..] {
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
        function: &String,
        register: Register,
        literal: &Literal,
    ) -> Result<Type, String> {
        use Literal::*;

        match literal {
            Int(x) => {
                self.push_fn(function, Mov(register, Value(x.to_string())))?;
                Ok(Type::from(literal))
            }
            Bool(x) => {
                self.push_fn(function, Mov(register, Value((*x as i32).to_string())))?;
                Ok(Type::from(literal))
            }
            Str(string) => {
                let index = self.get_string(string.clone());
                self.push_fn(function, Mov(register, Reg(Data(index))))?;
                Ok(Type::from(literal))
            }
            Array(array, array_type) => {
                let mut bp_offset = self.scope_size();
                let mut t: Option<Type> = array_type.clone();
                let first = bp_offset;

                for node in array.iter().rev() {
                    let node_type = self.consume_node(function, node.clone())?;

                    if t.as_ref().is_none() {
                        t = Some(node_type.clone());
                        self.stack.push(Stack::Allocation(
                            array.len() as u64 * node_type.byte_size(),
                        ));
                    }

                    if node_type != t.clone().ok_or("Array has an unknown type".to_string())? {
                        return Err(format!(
                            "Array has type of {:?} but an item has a type of {:?}",
                            &t, node_type
                        ));
                    }

                    self.push_fn(
                        function,
                        mov_reg(
                            Stack(
                                -(bp_offset as i64),
                                t.as_ref().ok_or("Array has an unknown type")?.byte_size(),
                            ),
                            match t.as_ref().ok_or("Array has an unknown type")?.byte_size() {
                                1 => AL,
                                2 => AX,
                                4 => EAX,
                                8 => RAX,
                                n => return Err(format!("Unknown register for byte size `{}`", n)),
                            },
                        ),
                    )?;

                    bp_offset += t.as_ref().ok_or("Array has an unknown type")?.byte_size();
                }

                self.extend_fn(
                    function,
                    [
                        Sub(RSP, Value((bp_offset - first).to_string())),
                        Lea(
                            RAX,
                            Stack(
                                -((bp_offset
                                    - t.as_ref().ok_or("Array has an unknown type")?.byte_size())
                                    as i64),
                                t.as_ref().ok_or("Array has an unknown type")?.byte_size(),
                            ),
                        ),
                    ],
                )?;

                Ok(Type::Pointer(Box::new(
                    t.ok_or("Array has an unknown type")?,
                )))
            }
        }
    }

    fn handle_ident(&mut self, ident: &String, function: &String) -> Result<Type, String> {
        let (address, t) = self
            .get_variable(ident)
            .ok_or(format!("Unkown identifier `{ident}`"))?;

        let instructions = match t {
            Int | Bool | Pointer(_) => mov_reg(
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

        self.push_fn(function, instructions)?;
        Ok(t) // TODO: If t is an array this should become a pointer
    }

    fn handle_define(
        &mut self,
        function: &String,
        ident: String,
        expected_type: Option<Type>,
        node: Node,
    ) -> Result<Type, String> {
        use Type::*;

        let t = self.consume_node(function, node)?;

        if let Some(expected) = expected_type {
            if t != expected {
                return Err(format!(
                    "Define expected a type of {:?} but got {:?}",
                    expected, t
                ));
            }
        }

        let instructions = match t {
            Int | Bool | Pointer(_) => vec![
                Mov(
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
                ),
                Sub(RSP, Value(t.byte_size().to_string())),
            ],
            _ => todo!(),
        };

        self.extend_fn(function, instructions)?;
        self.stack.push(Stack::Variable(ident, t));

        Ok(Void)
    }

    fn handle_call(&mut self, function: &String, nodes: Vec<Node>) -> Result<Type, String> {
        // Get function infomation

        let mut parameter_address = vec![];
        let func = if let Some(Node::Ident(func)) = nodes.get(0) {
            func
        } else {
            return Err("Could not find a function to call".to_string());
        };

        let func_signature = self
            .functions
            .get(func)
            .ok_or(format!("Function `{func}` has no type signature"))?
            .signature
            .clone();

        let mut node_types = vec![];
        for node in nodes[1..].iter() {
            node_types.push(self.node_type(node)?);
        }

        // Validate node types

        for (index, node_type) in node_types.iter().enumerate() {
            if index > func_signature.types.len() && !func_signature.var_args {
                return Err(format!("Too many arguments passed to function `{func}`"));
            } else if !func_signature.var_args {
                let expected_type = &func_signature.types[index + 1];

                if node_type != expected_type {
                    return Err(format!(
                        "In a call to {func} expected a {:?} but got a {:?}",
                        expected_type, node_type
                    ));
                }
            } else {
                break;
            }
        }

        // Calculate required C calling convention stack offset
        // the stack must be aligned to a multiple of 16

        let mut stack_offset =
            next_aligned_stack(self.scope_size() + func_param_stack_alloc(&node_types) + 8);

        if node_types.len() > 6 {
            for node in &node_types[6..] {
                stack_offset += node.byte_size();
            }
        }

        if stack_offset > 0 {
            self.push_fn(function, Sub(RSP, Value(stack_offset.to_string())))?;
            self.stack.push(Stack::Empty(stack_offset));
        }

        for node in nodes[1..].iter().rev() {
            // Evaluate each parameter and push it onto the stack

            let t = self.consume_node(function, node.clone())?;
            let address = Stack(-(self.scope_size() as i64), t.byte_size());

            self.push_fn(
                function,
                mov_reg(
                    address.clone(),
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
                ),
            )?;

            // Update the virtual stack within the compiler

            parameter_address.push((address, t.byte_size()));
            self.stack.push(Stack::Allocation(t.byte_size()));
        }

        // Move the first six parameters off the stack and into registers

        for i in 0..parameter_address.len().min(6) {
            let (address, byte_size) = parameter_address[parameter_address.len() - (i + 1)].clone();
            self.push_fn(
                function,
                mov_reg(
                    match byte_size {
                        1 => REGSITERS8[i].clone(),
                        2 => REGSITERS16[i].clone(),
                        4 => REGSITERS32[i].clone(),
                        _ => REGSITERS64[i].clone(),
                    },
                    address,
                ),
            )?;
        }

        // Pop all of the parameters off the stack

        for _ in parameter_address {
            self.stack
                .pop()
                .ok_or("Parameter address and scope stack out of sync".to_string())?;
        }

        let mut call_func = vec![NullReg(RAX), Call(func.to_string())];

        if let Some(Stack::Allocation(n)) = self.stack.last() {
            call_func.push(Add(RSP, Value(n.to_string())));
            self.stack
                .pop()
                .ok_or("Parameter address and scope stack out of sync".to_string())?;
        }

        self.extend_fn(function, call_func)?;

        self.functions
            .get(func)
            .ok_or("No function called `{func}`".to_string())?
            .signature
            .types
            .get(0)
            .cloned()
            .ok_or("Function `{func}` has no return type".to_string())
    }

    fn node_type(&self, node: &Node) -> Result<Type, String> {
        match node {
            Node::Literal(literal) => Ok(Type::from(literal)),
            Node::TypedIdent(_, t) => Ok(t.clone()),
            Node::Ident(ident) => self
                .get_variable(ident)
                .ok_or(format!("Unkown identifier `{ident}`"))
                .map(|(_, t)| t),
            Node::Bracket(nodes) => match nodes.get(0).ok_or("Cannot have empty brackets")? {
                Node::Ident(ident) => match ident.as_str() {
                    "ref" => match nodes.get(1).ok_or("ref must have 1 parameter")? {
                        Node::Ident(ident) => {
                            let t = self
                                .get_variable(ident)
                                .ok_or(format!("Unkown identifier `{ident}`"))?
                                .1;

                            Ok(Pointer(Box::new(t)))
                        }
                        _ => todo!(),
                    },
                    "extern" | "define" => Ok(Void),
                    _ => self
                        .functions
                        .get(ident)
                        .ok_or(format!("Unknown function called `{ident}`"))?
                        .signature
                        .types
                        .get(0)
                        .ok_or(format!("Function `{ident}` specifies no return type"))
                        .map(|x| x.clone()),
                },
                _ => todo!(),
            },
        }
    }

    fn handle_ref(&mut self, function: &String, node: &Node) -> Result<Type, String> {
        match node {
            Node::Ident(ident) => match self.get_variable(ident) {
                Some((address, t)) => {
                    self.push_fn(function, Lea(RAX, address))?;
                    Ok(Type::Pointer(Box::new(t)))
                }
                None => Err(format!("Unkown identifier `{ident}`")),
            },
            _ => Err("Can only take the reference of an identifier".to_string()),
        }
    }

    fn handle_extern(&mut self, nodes: Vec<Node>) -> Result<Type, String> {
        if let Node::Ident(ident) = nodes
            .get(1)
            .ok_or("Extern expects 2 parameters".to_string())?
        {
            let mut buffer = vec![];

            for node in &nodes[2..] {
                if let Node::Ident(ident) = node {
                    buffer.push(ident);
                } else {
                    return Err("Externs parameters must be idents".to_string());
                }
            }

            let (mut types, mut var_args) = (vec![], false);

            if let Some("...") = buffer.last().map(|x| x.as_str()) {
                buffer.pop();
                var_args = true;
            }

            for ident in buffer {
                types.push(Type::try_from(ident.as_str()).unwrap());
            }

            self.externs.push(ident.to_string());

            self.functions
                .insert(ident.to_string(), Function::new(types, var_args));

            Ok(Void)
        } else {
            Err("The first parameter of an extern must be an identifier".to_string())
        }
    }

    fn handle_function(&mut self, function: &String, nodes: Vec<Node>) -> Result<Type, String> {
        if let Some(Node::Ident(fn_name)) = nodes.get(1) {
            self.functions
                .insert(fn_name.to_string(), Function::new(vec![Void], false));
            let (mut signature_nodes, arguments) = if let Some(Node::Bracket(nodes)) = nodes.get(2)
            {
                nodes
                    .iter()
                    .fold(Ok((FuncSignature::new(vec![], false), vec![])), |y, x| {
                        let mut acc = y?;

                        let (ident, t) = if let Node::TypedIdent(ident, t) = x.clone() {
                            (ident, t)
                        } else {
                            return Err("Failed to parse function argument types".to_string());
                        };

                        acc.0.types.push(t.clone());
                        acc.1.push((ident, t));
                        Ok(acc)
                    })?
            } else {
                return Err("Missing function parameter".to_string());
            };

            signature_nodes.types.insert(
                0,
                if let Some(Node::Ident(ident)) = nodes.get(3) {
                    Type::try_from(ident.as_str()).map_err(|_| "Could parse type".to_string())?
                } else {
                    return Err("Return type must be an Type".to_string());
                },
            );

            self.functions
                .get_mut(fn_name)
                .ok_or(format!("Unkown function called `{function}`"))?
                .signature = signature_nodes;

            self.stack.push(Stack::BasePointer);

            let mut var_buffer: Vec<Stack> = vec![];
            let mut arg_stack_size: u64 = arguments
                .iter()
                .enumerate()
                .map(|(i, (_, t))| t.byte_size() * (i > 6) as u64)
                .sum();

            for (i, (ident, t)) in arguments.into_iter().enumerate().rev() {
                self.push_fn(
                    function,
                    Mov(
                        Stack(
                            -((self.scope_size() + arg_stack_size) as i64),
                            t.byte_size(),
                        ),
                        if i > 6 {
                            arg_stack_size -= t.byte_size();
                            Reg(Stack(arg_stack_size as i64, t.byte_size()))
                        } else {
                            Reg(match t.byte_size() {
                                8 => REGSITERS64[i].clone(),
                                4 => REGSITERS32[i].clone(),
                                2 => REGSITERS16[i].clone(),
                                _ => REGSITERS8[i].clone(),
                            })
                        },
                    ),
                )?;

                var_buffer.insert(0, Stack::Variable(ident.to_string(), t));
            }

            self.stack.extend(var_buffer);

            self.extend_fn(function, [Push(Reg(RBP)), Mov(RBP, Reg(RSP))])?;

            for node in nodes[4..].iter().cloned() {
                self.consume_node(fn_name, node)?;
            }

            let dealloc_instr = self.deallocate_scope();
            self.extend_fn(function, *dealloc_instr)?;

            Ok(Void)
        } else {
            Err("No function name provided to fn".to_string())
        }
    }

    fn consume_node(&mut self, function: &String, node: Node) -> Result<Type, String> {
        match node {
            Node::Literal(literal) => self.move_literal(function, RAX, &literal),
            Node::Ident(ident) | Node::TypedIdent(ident, _) => self.handle_ident(&ident, function),
            Node::Bracket(nodes) => match nodes.get(0).ok_or("Cannot have empty brackets")? {
                Node::Ident(ident) => match ident.as_str() {
                    "ref" => {
                        self.handle_ref(function, nodes.get(1).ok_or("ref must have 1 parameter")?)
                    }
                    "define" => {
                        let (ident, expected_type): (String, Option<Type>) = match nodes
                            .get(1)
                            .ok_or("Define expects 2 parameters".to_string())?
                            .clone()
                        {
                            Node::TypedIdent(ident, t) => (ident, Some(t)),
                            Node::Ident(ident) => (ident, None),
                            _ => return Err("Define's can only assign to identifiers".to_string()),
                        };

                        self.handle_define(
                            function,
                            ident,
                            expected_type,
                            nodes
                                .get(2)
                                .ok_or("Define expects 2 parameters".to_string())?
                                .clone(),
                        )
                    }
                    "extern" => self.handle_extern(nodes),
                    "fn" => self.handle_function(function, nodes),
                    _ => self.handle_call(function, nodes),
                },
                _ => todo!(),
            },
        }
    }

    pub fn apply(&mut self, nodes: Vec<Node>) -> Result<(), String> {
        for node in nodes {
            self.consume_node(&"main".to_string(), node)?;
        }

        Ok(())
    }

    pub fn export(&mut self) -> Vec<u8> {
        let mut buffer: Vec<u8> = include_bytes!("header.asm").to_vec();

        for name in self.externs.iter() {
            buffer.extend(format!("extern {name}\n").as_bytes());
        }

        let main_instrs = self.functions.remove("main").unwrap();
        buffer.extend(export_function("main", &main_instrs.body));

        for (key, value) in &self.functions {
            if !self.externs.contains(key) {
                buffer.extend(export_function(key, &value.body));
            }
        }

        buffer.extend(b"\nsection .data\n");
        for (i, v) in self.data.iter().enumerate() {
            buffer.extend(format!("    s{}: db `{}`, 0\n", i, v).as_bytes());
        }

        buffer
    }
}

fn export_function(fn_name: &str, instrs: &[Instr]) -> Vec<u8> {
    let mut buffer = vec![];

    buffer.extend(format!("\n{}:\n", fn_name).as_bytes());
    for instr in instrs {
        buffer.extend(
            match instr {
                DefineLabel(_) => format!("{}\n", instr.to_string()),
                _ => format!("    {}\n", instr.to_string()),
            }
            .as_bytes(),
        )
    }

    buffer
}
