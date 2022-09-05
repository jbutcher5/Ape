use crate::parser::Node;
use std::collections::HashMap;

use std::fs::File;
use std::io::Write;

#[inline]
fn next_multiple(start: u64, n: u64) -> u64 {
    (start as f64 / n as f64).ceil() as u64 * n
}

#[inline]
fn stack_alignment(bytes: u64) -> i64 {
    match bytes {
        0 => 16,
        start => next_multiple(start, 16) as i64,
    }
}

#[derive(Debug, Clone)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R(u8),
    Stack(i64),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Reg(Register),
    Value(i64),
}

#[derive(Debug, Clone)]
pub enum Instr {
    Raw(String),
    Push(Operand),
    Pop(Register),
    Mov(Register, Operand),
    Call(String),
    NullReg(Register),
    Add(Register, Operand),
    Sub(Register, Operand),
    Return,
    Syscall,
    Scoped(Scope),
}

#[derive(Debug, Clone)]
pub struct DefineBytes {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub name: Option<String>,
    pub used_bytes: u64,
    pub instructions: Vec<Instr>,
    pub vars: HashMap<String, i64>,
    pub parent: Option<Box<Scope>>,
}

#[derive(Debug)]
pub struct Program {
    file: File,
    global_scope: Scope,
    data: Vec<DefineBytes>,
}

impl ToString for Register {
    fn to_string(&self) -> String {
        use Register::*;

        match self {
            RAX => "rax".to_string(),
            RBX => "rbx".to_string(),
            RCX => "rcx".to_string(),
            RDX => "rdx".to_string(),
            RSI => "rsi".to_string(),
            RDI => "rdi".to_string(),
            RSP => "rsp".to_string(),
            RBP => "rbp".to_string(),
            R(n) => format!("r{n}"),
            Stack(i) => {
                if i >= &0 {
                    format!("[rbp+{}]", i)
                } else {
                    format!("[rbp{}]", i)
                }
            }
            Data(name) => name.to_owned(),
        }
    }
}

impl ToString for Operand {
    fn to_string(&self) -> String {
        use Operand::*;

        match self {
            Reg(reg) => reg.to_string(),
            Value(val) => val.to_string(),
        }
    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        use Instr::*;

        match self {
            Push(reg) => format!("push {}", reg.to_string()),
            Pop(reg) => format!("pop {}", reg.to_string()),
            Mov(reg, op) => format!("mov {}, {}", reg.to_string(), op.to_string()),
            Call(name) => format!("call {name}"),
            NullReg(reg) => format!("xor {}, {}", reg.to_string(), reg.to_string()),
            Add(reg1, op) => format!("add {}, {}", reg1.to_string(), op.to_string()),
            Sub(reg1, reg2) => format!("sub {}, {}", reg1.to_string(), reg2.to_string()),
            Return => "ret".to_string(),
            Syscall => "syscall".to_string(),
            Scoped(scope) => scope.to_string(),
            Raw(string) => string.to_string(),
        }
    }
}

impl ToString for DefineBytes {
    fn to_string(&self) -> String {
        format!("{}:  db {}", self.name, self.value)
    }
}

impl ToString for Scope {
    fn to_string(&self) -> String {
        use Instr::*;
        use Operand::*;
        use Register::*;

        let header = vec![
            Push(Reg(RBP)),
            Mov(RBP, Reg(RSP)),
            Sub(RSP, Value(stack_alignment(self.used_bytes))),
        ];

        let mut footer = vec![Add(RSP, Value(stack_alignment(self.used_bytes))), Pop(RBP)];

        if self.name == Some("main".to_string()) {
            footer.extend(vec![Mov(RAX, Value(60)), NullReg(RDI), Syscall, Return]);
        } else if self.name.is_some() {
            footer.push(Return);
        }

        let body: Vec<Instr> = vec![header, self.instructions.clone(), footer].concat();

        let result = [
            if let Some(name) = self.name.clone() {
                format!("{name}:\n")
            } else {
                String::new()
            },
            {
                let mut body_string = body
                    .iter()
                    .map(|instr| format!("    {}\n", instr.to_string()))
                    .collect::<String>();

                if self.name != Some("main".to_string()) {
                    body_string = body_string.trim_start().to_string();
                }

                body_string
            },
        ]
        .concat();
        result
    }
}

impl Scope {
    pub fn new(name: Option<String>, parent: Option<&Scope>) -> Self {
        let carried_vars = if let Some(scope) = parent {
            let mut vars = scope.vars.clone();

            for (_, val) in vars.iter_mut() {
                *val += stack_alignment(scope.used_bytes) + 8;
            }

            vars
        } else {
            HashMap::new()
        };

        Self {
            name,
            used_bytes: 0,
            instructions: vec![],
            vars: carried_vars,
            parent: match parent {
                Some(scope) => Some(Box::new(scope.clone())),
                None => None,
            },
        }
    }

    pub fn append(&mut self, instructions: Vec<Instr>) {
        self.instructions.extend(instructions);
    }

    pub fn new_var(&mut self, name: String, bytes: u64) {
        self.used_bytes += bytes;
        self.vars.insert(name, -(self.used_bytes as i64));
    }

    pub fn get_var(&self, name: &String) -> Option<Register> {
        Some(Register::Stack(*self.vars.get(name)?))
    }

    pub fn res_bytes(&mut self, bytes: u64) {
        self.used_bytes += bytes;
    }

    pub fn res_bytes_loc(&mut self, bytes: u64) -> u64 {
        self.res_bytes(bytes);
        self.used_bytes
    }

    pub fn generate_node(&mut self, node: Node) {
        use Instr::*;
        use Operand::*;
        use Register::*;

        match node {
            Node::Int(x) => {
                self.append(vec![Push(Value(x))]);
            }
            Node::Call(call) => {
                let ident = call.first().expect("Empty call");

                if let Node::Ident(name) = ident {
                    match name.as_str() {
                        "+" => {
                            for x in &call[1..call.len()] {
                                self.generate_node(x.clone());
                            }

                            self.append(vec![Pop(RAX), Pop(RBX), Add(RAX, Reg(RBX)), Push(Reg(RAX))]);
                        }
                        "define" => {
                            let symbol = match call.get(1) {
                                Some(Node::Ident(s)) => s,
                                Some(x) => panic!("{:?} is not a valid symbol", x),
                                _ => panic!("define requires 2 parameters"),
                            };
                            if !self.vars.contains_key(symbol) {
                                self.new_var(symbol.to_string(), 8);
                            }

                            if let Some(x) = call.get(2) {
                                self.generate_node(x.clone());
                            } else {
                                panic!("define requires 2 parameters");
                            }

                            self.append(vec![Pop(RAX), Mov(self.get_var(symbol).unwrap(), Reg(RAX))]);
                        }
                        "display" => {
                            for x in &call[1..call.len()] {
                                self.generate_node(x.clone());
                            }

                            self.append(vec![Pop(RDI), Instr::Call("print_value".to_string())]);
                        }
                        "scope" => {
                            let mut new_scope = Scope::new(None, Some(self));

                            for x in &call[1..call.len()] {
                                new_scope.generate_node(x.clone());
                            }

                            self.append(vec![Scoped(new_scope)]);
                        }
                        x => println!("Unknown symbol {}", x),
                    }
                } else {
                    panic!("The first item in the call is not a function")
                }
            }
            Node::Ident(ident) => {
                self.append(vec![Mov(RAX, Reg(self.get_var(&ident).unwrap())), Push(Reg(RAX))]);
            }
        }
    }
}

impl Program {
    pub fn new(file_path: &str) -> std::io::Result<Self> {
        Ok(Program {
            file: File::create(file_path)?,
            global_scope: Scope::new(Some("main".to_string()), None),
            data: vec![DefineBytes {
                name: "as_int".to_string(),
                value: "`%d\\n`".to_string(),
            }],
        })
    }

    fn encode(&self) -> Vec<u8> {
        [
            include_bytes!("header.asm"),
            self.global_scope.to_string().as_bytes(),
            format!(
                "section .data\n{}",
                self.data
                    .iter()
                    .map(|db| format!("    {}\n", db.to_string()))
                    .collect::<String>()
            )
            .as_bytes(),
        ]
        .concat()
    }

    pub fn generate(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.global_scope.generate_node(node);
        }
    }

    pub fn write(&mut self) -> std::io::Result<()> {
        self.file.write_all(&self.encode())
    }
}
