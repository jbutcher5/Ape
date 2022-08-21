use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Write};

macro_rules! appendln {
    ($self:ident, $($arg:tt)*) => {{
        $self.appendln(format!($($arg)*).as_bytes())
    }}
}

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
    rbp_offset: u32,
    variables: HashMap<String, u32>,
    assembly: Vec<u8>,
}

impl Generator {
    fn new(file_path: &str) -> io::Result<Self> {
        Ok(Generator {
            file: File::create(file_path)?,
            rbp_offset: 0,
            variables: HashMap::new(),
            assembly: vec![],
        })
    }

    fn header(&mut self) {
        self.appendln(include_bytes!("header.asm"))
    }

    fn footer(&mut self) {
        appendln!(self, "mov rax, 60");
        appendln!(self, "pop rdi");
        appendln!(self, "syscall");
    }

    fn generate(&mut self, node: Node) {
        use Node::*;

        match node {
            Int(x) => {
                appendln!(self, "mov rax, {}", x);
                appendln!(self, "push rax");
            }
            Call(call) => {
                let ident = call.first().expect("Empty call");

                if let Ident(name) = ident {
                    match name.as_str() {
                        "display" => {
                            for x in &call[1..call.len()] {
                                self.generate(x.clone());
                                appendln!(self, "pop rdi");
                                appendln!(self, "call func_print");
                            }
                        }
                        "+" => {
                            for x in &call[1..call.len()] {
                                self.generate(x.clone());
                            }

                            appendln!(self, "pop rax");
                            appendln!(self, "pop rbx");
                            appendln!(self, "add rax, rbx");
                            appendln!(self, "push rax");
                        }
                        "define" => {
                            self.rbp_offset += 8;
                            let symbol = match call.get(1) {
                                Some(Ident(s)) => s,
                                Some(x) => panic!("{:?} is not a valid symbol", x),
                                _ => panic!("define requires 2 parameters"),
                            };
                            if let Some(x) = self.variables.get(symbol) {
                                panic!("{symbol} is already defined at rbp-{x}");
                            } else {
                                self.variables.insert(symbol.to_string(), self.rbp_offset);
                            }

                            if let Some(x) = call.get(2) {
                                self.generate(x.clone());
                            } else {
                                panic!("define requires 2 parameters");
                            }

                            appendln!(self, "pop rax");
                            appendln!(self, "mov [rbp-{}], rax", self.rbp_offset);
                        }
                        x => println!("Unknown symbol {}", x),
                    }
                } else {
                    panic!("The first item in the call is not a function")
                }
            }
            Ident(ident) => {
                appendln!(
                    self,
                    "mov rax, [rbp-{}]",
                    self.variables.get(&ident).unwrap()
                );
                appendln!(self, "push rax");
            }
            _ => panic!("Lmao wtf bro your code is shit"),
        }
    }

    fn generate_toplevel(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.generate(node);
            appendln!(self, "\n");
        }
    }

    fn appendln(&mut self, code: &[u8]) {
        self.assembly.push(b'\n');
        self.assembly.extend(code);
    }

    fn write(&mut self) -> io::Result<()> {
        self.file.write_all(&self.assembly)
    }
}

fn main() -> io::Result<()> {
    let mut parser = Parser {
        data: fs::read("test.lp")?,
        index: 0,
    };

    let result = parser.parse_toplevel();
    let mut generator = Generator::new("out.asm")?;

    generator.header();
    generator.generate_toplevel(result.unwrap());
    generator.footer();
    generator.write()?;

    Ok(())
}
