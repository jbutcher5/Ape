#[derive(Debug, Clone)]
pub enum Node {
    Int(i64),
    Call(Vec<Node>),
    Ident(String),
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

    fn parse_node(&mut self) -> Option<Node> {
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

    pub fn parse(&mut self) -> Option<Vec<Node>> {
        let mut result: Vec<Node> = vec![];

        while self.index < self.data.len() {
            let node = self.parse_node()?;
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

            acc.push(self.parse_node()?);
            self.skip_whitespace();
        }

        Some(Node::Call(acc))
    }
}
