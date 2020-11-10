use std::io::{self, Read};

#[derive(Debug, Clone)]
pub enum TT {
    Ident(String),
    String(String),
    Float(f64),
    Int(i64),
    Nat(i64),
    Char(char),
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
}

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("{0}")]
    IO(#[from] io::Error),
    #[error("Invalid escape sequence '\\{0}'")]
    InvalidEscapeSequence(char),
    #[error("Invalid character '{0}'")]
    InvalidCharacter(char),
    #[error("{0}")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("{0}")]
    ParseFloat(#[from] std::num::ParseFloatError),
    #[error("Expected {expected:?} {}", found_char(*found))]
    Expected { expected: char, found: Option<char> },
    #[error("Expected character")]
    ExpectedCharacter,
    #[error("Invalid bracket")]
    InvalidBracket(char),
    #[error("Unmatched bracket")]
    UnmatchedBracket(char),
}

fn found_char(found: Option<char>) -> String {
    if let Some(c) = found {
        format!("found {:?}", c)
    } else {
        String::new()
    }
}

pub fn lex<R>(input: R) -> Result<Vec<TT>, LexError>
where
    R: Read,
{
    let mut tokens = Vec::new();
    let mut chars = itertools::put_back(unicode_reader::CodePoints::from(input.bytes()));
    let mut brackets = Vec::new();
    while let Some(c) = chars.next() {
        let token = match c? {
            '[' => {
                brackets.push('[');
                TT::OpenBracket
            }
            ']' => {
                if let Some(brack) = brackets.pop() {
                    if brack != '[' {
                        return Err(LexError::InvalidBracket(']'));
                    }
                } else {
                    return Err(LexError::UnmatchedBracket(']'));
                };
                TT::CloseBracket
            }
            '{' => {
                brackets.push('{');
                TT::OpenCurly
            }
            '}' => {
                if let Some(brack) = brackets.pop() {
                    if brack != '{' {
                        return Err(LexError::InvalidBracket('}'));
                    }
                } else {
                    return Err(LexError::UnmatchedBracket('}'));
                };
                TT::CloseCurly
            }
            // String literals
            '"' => {
                let mut s = String::new();
                let mut closed = false;
                while let Some(c) = chars.next() {
                    match c? {
                        '"' => {
                            closed = true;
                            break;
                        }
                        '\\' => {
                            if let Some(c) = chars.next() {
                                s.push(escaped_char(c?)?);
                            }
                        }
                        c => s.push(c),
                    }
                }
                if !closed {
                    return Err(LexError::Expected {
                        expected: '"',
                        found: None,
                    });
                }
                TT::String(s)
            }
            // Character literals
            c if c == '\'' => {
                let mut c = chars.next().ok_or(LexError::ExpectedCharacter)??;
                if c == '\\' {
                    c = escaped_char(chars.next().ok_or(LexError::ExpectedCharacter)??)?;
                };
                let next = chars.next().ok_or(LexError::ExpectedCharacter)??;
                if next != '\'' {
                    return Err(LexError::Expected {
                        expected: '\'',
                        found: Some(next),
                    });
                }
                TT::Char(c)
            }
            // Num literals
            c if c.is_digit(10) || c == '-' => {
                let mut s: String = c.into();
                let mut period = false;
                while let Some(c) = chars.next() {
                    let c = c?;
                    if c.is_digit(10) || c == '.' && !period {
                        if c == '.' {
                            period = true;
                        }
                        s.push(c);
                    } else {
                        chars.put_back(Ok(c));
                        break;
                    }
                }
                if period {
                    TT::Float(s.parse()?)
                } else if s.starts_with('-') {
                    TT::Int(s.parse()?)
                } else {
                    TT::Nat(s.parse()?)
                }
            }
            // Idents
            c if ident_start_char(c) => {
                let mut s: String = c.into();
                while let Some(c) = chars.next() {
                    let c = c?;
                    if ident_body_char(c) {
                        s.push(c);
                    } else {
                        chars.put_back(Ok(c));
                        break;
                    }
                }
                TT::Ident(s)
            }
            c if c.is_whitespace() => continue,
            c => return Err(LexError::InvalidCharacter(c)),
        };
        tokens.push(token);
    }
    if let Some(brack) = brackets.pop() {
        return Err(LexError::UnmatchedBracket(brack));
    }
    Ok(tokens)
}

fn escaped_char(c: char) -> Result<char, LexError> {
    Ok(match c {
        '"' | '\\' => c,
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        c => return Err(LexError::InvalidEscapeSequence(c)),
    })
}

fn ident_start_char(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c as u32 > 127
}

fn ident_body_char(c: char) -> bool {
    ident_start_char(c) || c.is_digit(10)
}
