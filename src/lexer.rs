use std::path::Path;

#[derive(Debug, Clone)]
pub enum Token {
    OpenParens,
    CloseParens,
    Number(i64),
    String(String),
    Ident(String),
}

pub struct Lexer {
    source: Vec<u8>,
    index: usize,
}

impl TryFrom<&Path> for Lexer {
    type Error = std::io::Error;

    fn try_from(path: &Path) -> Result<Lexer, Self::Error> {
        Ok(Lexer {
            source: std::fs::read(path)?,
            index: 0,
        })
    }
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.as_bytes().to_vec(),
            index: 0,
        }
    }

    fn curr(&self) -> Option<u8> {
        self.source.get(self.index).map(|x| *x)
    }

    fn inc(&mut self) {
        self.index += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(b'\n' | b' ') = self.curr() {
            self.inc();
        }
    }

    pub fn tokenise(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        self.skip_whitespace();

        while let Some(token) = self.curr() {
            tokens.push(match token {
                b'(' => Token::OpenParens,
                b')' => Token::CloseParens,
                b'"' => self.handle_string(),
                b'0'..=b'9' => self.handle_number(),
                _ => self.handle_ident(),
            });
            self.inc();
            self.skip_whitespace();
        }

        tokens
    }

    fn handle_string(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            self.inc();

            if c == b'"' {
                break;
            } else {
                acc.push(c);
            }
        }

        Token::String(String::from_utf8(acc).unwrap())
    }

    fn handle_number(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            self.inc();

            if c.is_ascii_digit() || c == b'.' {
                acc.push(c);
            } else {
                break;
            }
        }

        Token::Number(String::from_utf8(acc).unwrap().parse::<i64>().unwrap())
    }

    fn handle_ident(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            self.inc();

            if c != b'(' && c != b')' && c != b'"' && c != b' ' && c != b'\n' {
                acc.push(c);
            } else {
                break;
            }
        }

        Token::Ident(String::from_utf8(acc).unwrap())
    }
}

impl TryFrom<Vec<Token>> for Vec<Node> {
    type Error = String;

    fn try_from(tokens: Vec<Token>) -> Result<Vec<Node>, Self::Error> {}
}
