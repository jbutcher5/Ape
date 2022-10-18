use crate::code_gen::{Literal, Node, Type};
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParserNode {
    Int(i64),
    Call(Vec<ParserNode>),
    Ident(String),
}

impl TryFrom<&ParserNode> for Node {
    type Error = &'static str;

    fn try_from(node: &ParserNode) -> Result<Self, Self::Error> {
        let _define = ParserNode::Ident("define".to_string());
        let _extern = ParserNode::Ident("extern".to_string());
        let _reference = ParserNode::Ident("ref".to_string());

        fn convert_vec(nodes: &[ParserNode]) -> Result<Vec<Node>, &'static str> {
            let mut acc = vec![];

            for node in nodes {
                acc.push(Node::try_from(node)?);
            }

            Ok(acc)
        }

        Ok(match node {
            ParserNode::Call(array) => {
                if let [head, tail @ ..] = array.as_slice() {
                    if head == &_define {
                        if let (Some(left), Some(right)) = (tail.get(0), tail.get(1)) {
                            Node::Define(
                                Box::new(Node::try_from(left)?),
                                Box::new(Node::try_from(right)?),
                            )
                        } else {
                            return Err("Define requires 2 parameters");
                        }
                    } else if head == &_extern {
                        let mut types = vec![];

                        if tail.len() > 1 {
                            return Err("An external function must specify a type");
                        }

                        for t in &tail[1..] {
                            match t {
                                ParserNode::Ident(t) => match Type::try_from(t.as_str()) {
                                    Ok(t) => types.push(t),
                                    _ => {
                                        return Err("Type in an extern must reference a valid type")
                                    }
                                },
                                _ => return Err("Type cannot be a literal value."),
                            }
                        }

                        if let ParserNode::Ident(ident) = &tail[0] {
                            Node::Extern(ident.to_string(), types)
                        } else {
                            return Err("First argument of an extern must be an identifier");
                        }
                    } else if head == &_reference {
                        Node::Ref(Box::new(Node::try_from(
                            tail.get(0).ok_or("Reference requires 1 parameter")?,
                        )?))
                    } else {
                        Node::Call(Box::new(Node::try_from(head)?), convert_vec(tail)?)
                    }
                } else {
                    return Err("A call requires a length of at least 1");
                }
            }
            ParserNode::Int(value) => Node::Literal(Literal::Int(*value)),
            ParserNode::Ident(ident) => Node::Ident(ident.to_string()),
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

    pub fn parse_to_ir(&mut self) -> Result<Vec<Node>, &'static str> {
        self.parse()?.iter().map(Node::try_from).collect()
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
