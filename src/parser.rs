use crate::code_gen::{IRNode, Type};

#[derive(Debug, Clone)]
pub enum ParserNode {
    Int(i64),
    Call(Vec<ParserNode>),
    Ident(String),
}

impl TryFrom<&ParserNode> for IRNode {
    type Error = &'static str;

    fn try_from(node: &ParserNode) -> Result<Self, Self::Error> {
        let _define = ParserNode::Ident("define".to_string());

        fn convert_vec(nodes: &[ParserNode]) -> Result<Vec<IRNode>, &'static str> {
            let mut acc = vec![];

            for node in nodes {
                acc.push(IRNode::try_from(node)?);
            }

            Ok(acc)
        }

        Ok(match node {
            ParserNode::Call(array) => match array.as_slice() {
                [_define, ParserNode::Ident(ident), value] => {
                    IRNode::Define(ident.to_string(), Box::new(IRNode::try_from(value)?))
                }
                [ParserNode::Ident(ident), tail @ ..] => {
                    IRNode::CCall(ident.to_string(), convert_vec(tail)?)
                }
                _ => return Err("Call's must begin with an ident"),
            },
            ParserNode::Int(value) => IRNode::Literal(Type::Int(*value)),
            ParserNode::Ident(ident) => IRNode::Ident(ident.to_string()),
        })
    }
}

#[derive(Debug)]
pub struct Parser {
    data: Vec<u8>,
    index: usize,
}

impl Parser {
    pub fn new(file_path: &str) -> std::io::Result<Self> {
        Ok(Self {
            data: std::fs::read(file_path)?,
            index: 0,
        })
    }

    fn parse_node(&mut self) -> Result<ParserNode, &'static str> {
        self.skip_whitespace();

        match self.get_index() {
            Some(b'0'..=b'9') => Ok(self.parse_int()),
            Some(b'(') => self.parse_call(),
            Some(_) => Ok(self.parse_ident()),
            None => Err("Unexpected EOF"),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ParserNode>, &'static str> {
        let mut result: Vec<ParserNode> = vec![];

        while self.index < self.data.len() {
            let node = self.parse_node()?;
            self.skip_whitespace();

            result.push(node);
        }

        Ok(result)
    }

    pub fn parse_to_ir(&mut self) -> Result<Vec<IRNode>, &'static str> {
        self.parse()?.iter().map(IRNode::try_from).collect()
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

    fn parse_int(&mut self) -> ParserNode {
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

        ParserNode::Int(acc)
    }

    fn parse_ident(&mut self) -> ParserNode {
        let start = self.index;

        while let Some(c) = self.get_index() {
            if c.is_ascii_whitespace() || *c == b'(' || *c == b')' {
                break;
            } else {
                self.inc();
            }
        }

        ParserNode::Ident(String::from_utf8(self.data[start..self.index].to_vec()).unwrap())
        // TODO: Remove unwrap
    }

    fn parse_call(&mut self) -> Result<ParserNode, &'static str> {
        let mut acc: Vec<ParserNode> = vec![];
        self.inc();
        self.skip_whitespace();

        while let Some(c) = self.get_index() {
            if *c == b')' {
                self.inc();
                break;
            }

            acc.push(self.parse_node()?);
            self.skip_whitespace();
        }

        Ok(ParserNode::Call(acc))
    }
}
