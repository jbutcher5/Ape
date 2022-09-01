#![feature(iter_intersperse)]

mod asm;

use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Write};

use asm::{DefineBytes, Instr, Instr::*, Program, Register::*, Scope};

#[derive(Debug, Clone)]
enum Node {
    Int(i64),
    Call(Vec<Node>),
    Ident(String),
}

#[derive(Debug)]
struct Parser {
    data: Vec<u8>,
    index: usize,
}

impl Parser {
    fn parse(&mut self) -> Option<Node> {
        let mut result: Option<Node> = None;

        self.skip_whitespace();

        match self.get_index() {
            Some(b'0'..=b'9') => result = self.parse_int(),
            Some(b'(') => result = self.parse_call(),
            Some(_) => result = self.parse_ident(),
            None => println!("Unexpected EOF"),
        }

        result
    }

    fn parse_toplevel(&mut self) -> Option<Vec<Node>> {
        let mut result: Vec<Node> = vec![];

        while self.index < self.data.len() {
            let node = self.parse()?;
            self.skip_whitespace();

            result.push(node);
        }

        Some(result)
    }

    fn skip_whitespace(&mut self) {
        while let Some(b'\n' | b' ') = self.get_index() {
            self.inc();
        }
    }

    fn get_index(&self) -> Option<&u8> {
        self.data.get(self.index)
    }

    fn inc(&mut self) {
        self.index += 1;
    }

    fn parse_int(&mut self) -> Option<Node> {
        let mut acc: i64 = 0;

        while let Some(c) = self.get_index() {
            if c.is_ascii_digit() {
                acc *= 10;
                acc += *c as i64 - 48;
                self.inc();
            } else {
                break;
            }
        }

        Some(Node::Int(acc))
    }

    fn parse_ident(&mut self) -> Option<Node> {
        let start = self.index;

        while let Some(c) = self.get_index() {
            if c.is_ascii_whitespace() || *c == b'(' || *c == b')' {
                break;
            } else {
                self.inc();
            }
        }

        Some(Node::Ident(
            String::from_utf8(self.data[start..self.index].to_vec()).unwrap(),
        ))
    }

    fn parse_call(&mut self) -> Option<Node> {
        let mut acc: Vec<Node> = vec![];
        self.inc();
        self.skip_whitespace();

        while let Some(c) = self.get_index() {
            if *c == b')' {
                self.inc();
                break;
            }

            acc.push(self.parse()?);
            self.skip_whitespace();
        }

        Some(Node::Call(acc))
    }
}

struct Generator {
    file: File,
    variables: HashMap<String, u64>,
    scopes: Vec<Scope>,
    data: Vec<DefineBytes>,
    i: usize,
}

impl Generator {
    fn new(file_path: &str) -> io::Result<Self> {
        Ok(Generator {
            file: File::create(file_path)?,
            variables: HashMap::new(),
            scopes: vec![Scope {
                name: "main".to_string(),
                used_bytes: 0,
                instructions: vec![],
            }],
            data: vec![],
            i: 0,
        })
    }

    fn scope(&mut self) -> &mut Scope {
        &mut self.scopes[self.i]
    }

    fn generate(&mut self, node: Node) {
        use Node::*;

        match node {
            Int(x) => {
                self.append(vec![Pushl(x)]);
            }
            Call(call) => {
                let ident = call.first().expect("Empty call");

                if let Ident(name) = ident {
                    match name.as_str() {
                        "+" => {
                            for x in &call[1..call.len()] {
                                self.generate(x.clone());
                            }

                            self.append(vec![Pop(RAX), Pop(RBX), Add(RAX, RBX), Push(RAX)]);
                        }
                        "define" => {
                            let location = self.scope().res_bytes_loc(4);

                            let symbol = match call.get(1) {
                                Some(Ident(s)) => s,
                                Some(x) => panic!("{:?} is not a valid symbol", x),
                                _ => panic!("define requires 2 parameters"),
                            };
                            if let Some(x) = self.variables.get(symbol) {
                                panic!("{symbol} is already defined at rbp-{x}");
                            } else {
                                self.variables.insert(symbol.to_string(), location);
                            }

                            if let Some(x) = call.get(2) {
                                self.generate(x.clone());
                            } else {
                                panic!("define requires 2 parameters");
                            }

                            self.append(vec![Pop(RAX), Mov(Stack(location), RAX)]);
                        }
                        x => println!("Unknown symbol {}", x),
                    }
                } else {
                    panic!("The first item in the call is not a function")
                }
            }
            Ident(ident) => {
                self.append(vec![
                    Mov(RAX, Stack(*self.variables.get(&ident).unwrap())),
                    Push(RAX),
                ]);
            }
            _ => panic!("Lmao wtf bro your code is shit"),
        }
    }

    fn open_scope(&mut self, name: &str) {
        self.i += 1;
        self.scopes.push(Scope {
            name: name.to_string(),
            used_bytes: 0,
            instructions: vec![],
        });
    }

    fn append(&mut self, instructions: Vec<Instr>) {
        self.scopes[self.i].append(instructions)
    }

    fn generate_toplevel(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.generate(node);
        }
    }

    fn write(&mut self) -> io::Result<()> {
        let program = Program {
            text: self.scopes.clone(),
            data: self.data.clone(),
        };

        self.file.write_all(&program.encode())
    }
}

fn main() -> io::Result<()> {
    let mut parser = Parser {
        data: fs::read("test.lp")?,
        index: 0,
    };

    let result = parser.parse_toplevel();
    let mut generator = Generator::new("out.asm")?;

    generator.generate_toplevel(result.unwrap());
    generator.write()?;

    Ok(())
}
