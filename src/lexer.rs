use std::path::Path;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
    Octothorpe,
    Number(i64),
    Boolean(bool),
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

    fn dec(&mut self) {
        self.index -= 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(b'\n' | b' ') = self.curr() {
            self.inc();
        }
    }

    pub fn tokenise(&mut self) -> Result<Vec<Token>, String> {
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
                b'#' => self.handle_octothorpe()?,
                b'"' => self.handle_string()?,
                b'0'..=b'9' => self.handle_number()?,
                _ => self.handle_ident(),
            });
            self.skip_whitespace();
        }

        Ok(tokens)
    }

    fn handle_octothorpe(&mut self) -> Result<Token, String> {
        self.index += 2;

        self.curr()
            .map(|c| match c {
                b't' => Ok(Token::Boolean(true)),
                b'f' => Ok(Token::Boolean(false)),
                _ => {
                    self.dec();
                    Ok(Token::Octothorpe)
                }
            })
            .ok_or("A second character must follow a #".to_string())?
    }

    fn handle_string(&mut self) -> Result<Token, String> {
        let mut acc = vec![];

        loop {
            self.inc();

            if let Some(b'"') = self.curr() {
                self.inc();

                return Ok(Token::String(String::from_utf8(acc).map_err(|_| {
                    "Error converting utf8 bytes to String".to_string()
                })?));
            } else if let Some(c) = self.curr() {
                acc.push(c);
            } else {
                return Err("FOO".to_string());
            }
        }
    }

    fn handle_number(&mut self) -> Result<Token, String> {
        let mut acc = vec![];

        while let Some(c) = self.curr() {
            if c.is_ascii_digit() {
                acc.push(c);
                self.inc();
            } else {
                break;
            }
        }

        Ok(Token::Number(
            String::from_utf8(acc)
                .map_err(|_| "Error converting utf8 bytes to String".to_string())?
                .parse::<i64>()
                .map_err(|_| "Error converting String to signed 64-bit number")?,
        ))
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
