use crate::code_gen::{Literal, Node};
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
            Token::Ident(ident) => Node::Ident(ident),
            Token::Number(n) => Node::Literal(Literal::Int(n)),
            Token::String(str) => Node::Literal(Literal::Str(str)),
            Token::Boolean(b) => Node::Literal(Literal::Bool(b)),
            Token::CloseBracket => return Err("Out of place close brackets".to_string()),
        });
    }

    Ok(result)
}
