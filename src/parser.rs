use crate::code_gen::{Literal, Node, Type};
use crate::lexer::Token;

pub fn parse(mut tokens: Vec<Token>) -> Result<Vec<Node>, String> {
    let mut result: Vec<Node> = vec![];

    while !tokens.is_empty() {
        let token = tokens.remove(0);

        result.push(match token {
            Token::OpenBracket => {
                let mut bracket_depth = 1;
                let mut buffer: Vec<Token> = vec![];

                while !tokens.is_empty() {
                    let token = tokens.remove(0);

                    if Token::OpenBracket == token {
                        bracket_depth += 1;
                    } else if Token::CloseBracket == token {
                        bracket_depth -= 1;
                    }

                    if bracket_depth == 0 {
                        break;
                    } else {
                        buffer.push(token);
                    }
                }

                if bracket_depth == 0 {
                    Node::Bracket(parse(buffer)?)
                } else {
                    return Err("Bracket is never closed".to_string());
                }
            }
            Token::Octothorpe => {
                if tokens.len() < 2 {
                    return Err("Invalid array".to_string());
                } else {
                    let mut t = None;

                    if let Some(Token::Ident(type_hint)) = tokens.get(0) {
                        t = Some(
                            Type::try_from(type_hint.as_str())
                                .map_err(|_| format!("{type_hint} is not a valid type"))?,
                        );

                        tokens.remove(0);
                    }

                    if let Token::OpenBracket = tokens.remove(0) {
                        let mut bracket_depth = 1;
                        let mut buffer: Vec<Token> = vec![];

                        while !tokens.is_empty() {
                            let token = tokens.remove(0);

                            if Token::OpenBracket == token {
                                bracket_depth += 1;
                            } else if Token::CloseBracket == token {
                                bracket_depth -= 1;
                            }

                            if bracket_depth == 0 {
                                break;
                            } else {
                                buffer.push(token);
                            }
                        }

                        if bracket_depth == 0 {
                            Node::Literal(Literal::Array(parse(buffer)?, t))
                        } else {
                            return Err("Bracket is never closed".to_string());
                        }
                    } else {
                        return Err("Expected an open bracket after a #".to_string());
                    }
                }
            }
            Token::Ident(ident) => Node::Ident(ident),
            Token::Number(n) => Node::Literal(Literal::Int(n)),
            Token::String(str) => Node::Literal(Literal::Str(str)),
            Token::Boolean(b) => Node::Literal(Literal::Bool(b)),
            Token::CloseBracket => return Err("Out of place close brackets".to_string()),
        });
    }

    Ok(result)
}
