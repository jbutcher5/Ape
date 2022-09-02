use crate::parser::Node;
use std::collections::HashMap;

use std::fs::File;
use std::io::Write;

#[inline]
fn next_multiple(start: u64, n: u64) -> u64 {
    (start as f64 / n as f64).ceil() as u64 * n
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
    Stack(u64),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Instr {
    Pushl(i64),
    Push(Register),
    Pop(Register),
    Movl(Register, i64),
    Mov(Register, Register),
    Call(String),
    NullReg(Register),
    Add(Register, Register),
    Addl(i64, i64),
    Sub(Register, Register),
    Subl(i64, i64),
    Return,
    Syscall,
}

#[derive(Debug, Clone)]
pub struct DefineBytes {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub used_bytes: u64,
    pub instructions: Vec<Instr>,
    pub functions: Vec<Function>,
    pub vars: HashMap<String, u64>,
    pub is_main: bool,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub body: Scope,
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
            Stack(i) => format!("[rbp-{i}]"),
            Data(name) => name.to_owned(),
        }
    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        use Instr::*;
        use Register::*;

        match self {
            Pushl(n) => [Movl(RAX, *n), Push(RAX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Push(reg) => format!("push {}", reg.to_string()),
            Pop(reg) => format!("pop {}", reg.to_string()),
            Movl(reg, n) => format!("mov {}, {n}", reg.to_string()),
            Mov(reg1, reg2) => format!("mov {}, {}", reg1.to_string(), reg2.to_string()),
            Call(name) => format!("call {name}"),
            NullReg(reg) => Movl(reg.to_owned(), 0).to_string(),
            Add(reg1, reg2) => format!("add {}, {}", reg1.to_string(), reg2.to_string()),
            Addl(l1, l2) => [Movl(RAX, *l1), Movl(RBX, *l2), Add(RAX, RBX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Sub(reg1, reg2) => format!("sub {}, {}", reg1.to_string(), reg2.to_string()),
            Subl(l1, l2) => [Movl(RAX, *l1), Movl(RBX, *l2), Sub(RAX, RBX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Return => "ret".to_string(),
            Syscall => "syscall".to_string(),
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
        use Register::*;

        let stack_alignment = match self.used_bytes {
            0 => 16,
            start => next_multiple(start, 16),
        };

        let header = vec![
            Push(RBP),
            Mov(RBP, RSP),
            Movl(RAX, stack_alignment as i64),
            Sub(RSP, RAX),
        ];

        let footer = if self.is_main {
            vec![Pop(RBP), Movl(RAX, 60), NullReg(RDI), Syscall, Return]
        } else {
            vec![Pop(RBP), Return]
        };

        let body: Vec<Instr> = vec![header, self.instructions.clone(), footer].concat();

        [
            if self.is_main {
                "main:\n".to_string()
            } else {
                String::new()
            },
            body.iter()
                .map(|instr| format!("    {}\n", instr.to_string()))
                .collect::<String>(),
        ]
        .concat()
    }
}

impl Scope {
    pub fn new(is_main: bool) -> Self {
        Self {
            used_bytes: 0,
            instructions: vec![],
            functions: vec![],
            vars: HashMap::new(),
            is_main,
        }
    }

    pub fn append(&mut self, instructions: Vec<Instr>) {
        self.instructions.extend(instructions);
    }

    pub fn new_var(&mut self, name: String, bytes: u64) {
        self.used_bytes += bytes;
        self.vars.insert(name, self.used_bytes);
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
}

impl Program {
    pub fn new(file_path: &str) -> std::io::Result<Self> {
        Ok(Program {
            file: File::create(file_path)?,
            global_scope: Scope::new(true),
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

    fn generate_node(&mut self, node: Node) {
        use Instr::*;
        use Register::*;

        match node {
            Node::Int(x) => {
                self.append(vec![Pushl(x)]);
            }
            Node::Call(call) => {
                let ident = call.first().expect("Empty call");

                if let Node::Ident(name) = ident {
                    match name.as_str() {
                        "+" => {
                            for x in &call[1..call.len()] {
                                self.generate_node(x.clone());
                            }

                            self.append(vec![Pop(RAX), Pop(RBX), Add(RAX, RBX), Push(RAX)]);
                        }
                        "define" => {
                            let symbol = match call.get(1) {
                                Some(Node::Ident(s)) => s,
                                Some(x) => panic!("{:?} is not a valid symbol", x),
                                _ => panic!("define requires 2 parameters"),
                            };
                            if let Some(x) = self.global_scope.vars.get(symbol) {
                                panic!("{symbol} is already defined at rbp-{x}");
                            } else {
                                self.global_scope.new_var(symbol.to_string(), 8);
                            }

                            if let Some(x) = call.get(2) {
                                self.generate_node(x.clone());
                            } else {
                                panic!("define requires 2 parameters");
                            }

                            self.append(vec![
                                Pop(RAX),
                                Mov(self.global_scope.get_var(symbol).unwrap(), RAX),
                            ]);
                        }
                        "display" => {
                            for x in &call[1..call.len()] {
                                self.generate_node(x.clone());
                            }

                            self.append(vec![Pop(RDI), Instr::Call("print_value".to_string())]);
                        }
                        x => println!("Unknown symbol {}", x),
                    }
                } else {
                    panic!("The first item in the call is not a function")
                }
            }
            Node::Ident(ident) => {
                self.append(vec![
                    Mov(RAX, self.global_scope.get_var(&ident).unwrap()),
                    Push(RAX),
                ]);
            }
            _ => panic!("Lmao wtf bro your code is shit"),
        }
    }

    fn append(&mut self, instructions: Vec<Instr>) {
        self.global_scope.append(instructions)
    }

    pub fn generate(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.generate_node(node);
        }
    }

    pub fn write(&mut self) -> std::io::Result<()> {
        self.file.write_all(&self.encode())
    }
}
