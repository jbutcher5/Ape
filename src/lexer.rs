use std::path::Path;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
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
                b'(' => {
                    self.inc();
                    Token::OpenBracket
                }
                b')' => {
                    self.inc();
                    Token::CloseBracket
                }
                b'"' => self.handle_string(),
                b'0'..=b'9' => self.handle_number(),
                _ => self.handle_ident(),
            });
            self.skip_whitespace();
        }

        tokens
    }

    fn handle_string(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            if c != b'"' {
                acc.push(c);
                self.inc();
            } else {
                break;
            }
        }

        Token::String(String::from_utf8(acc).unwrap())
    }

    fn handle_number(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            if c.is_ascii_digit() {
                acc.push(c);
                self.inc();
            } else {
                break;
            }
        }

        println!("{:?}", self.curr().map(|x| x as char));

        Token::Number(String::from_utf8(acc).unwrap().parse::<i64>().unwrap())
    }

    fn handle_ident(&mut self) -> Token {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            if c != b'(' && c != b')' && c != b'"' && c != b' ' && c != b'\n' {
                acc.push(c);
                self.inc();
            } else {
                break;
            }
        }

        Token::Ident(String::from_utf8(acc).unwrap())
    }
}
