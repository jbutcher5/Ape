use std::fs::{self, File};
use std::io::{self, Write};

macro_rules! emit {
    ($self:ident, $($arg:tt)*) => {{
        $self.file.write_all(format!($($arg)*).as_bytes())?
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

    fn parse_toplevel(&mut self) -> Option<Node> {
        let result = self.parse();
        self.skip_whitespace();

        if self.index < self.data.len() {
            println!("Expected EOF after top level");
            return None;
        }

        result
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
            if c.is_ascii_whitespace() {
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

            if let Some(x) = self.parse() {
                acc.push(x);
            } else {
                return None;
            }

            self.skip_whitespace();
        }

        Some(Node::Call(acc))
    }
}

struct Generator {
    file: File,
}

impl Generator {
    fn new(file_path: &str) -> io::Result<Self> {
        Ok(Generator {
            file: File::create(file_path)?,
        })
    }

    fn header(&mut self) -> io::Result<()> {
        emit!(self, "section .text\n");
        emit!(self, "global _start\n");
        emit!(self, "_start:\n");
        Ok(())
    }

    fn footer(&mut self) -> io::Result<()> {
        emit!(self, "mov rax, 60\n");
        emit!(self, "pop rdi\n");
        emit!(self, "syscall\n");
        Ok(())
    }

    fn generate(&mut self, node: Node) -> io::Result<()> {
        use Node::*;

        match node {
            Int(x) => {
                emit!(self, "mov rax, {}\npush rax\n", x);
                Ok(())
            }
            Call(call) => {
                let ident = call.first().unwrap();

                for x in &call[1..call.len()] {
                    self.generate(x.clone())?;
                }

                if let Node::Ident(name) = ident {
                    match name.as_str() {
                        "+" => {
                            emit!(self, "pop rax\n");
                            emit!(self, "pop rbx\n");
                            emit!(self, "add rax, rbx\n");
                            emit!(self, "push rax\n");
                        }
                        x => println!("Unknown symbol {}", x),
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

fn main() -> io::Result<()> {
    let mut parser = Parser {
        data: fs::read("test.lp")?,
        index: 0,
    };

    let result = parser.parse_toplevel();
    let mut generator = Generator::new("out.asm")?;

    generator.header()?;
    generator.generate(result.unwrap())?;
    generator.footer()?;

    Ok(())
}
