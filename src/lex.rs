use std::{
    cell::Cell,
    fmt,
    io::{self, Read},
};

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
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tt: TT,
    pub start: Loc,
    pub end: Loc,
}

#[derive(Debug, thiserror::Error)]
pub enum LexErrorKind {
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

impl LexErrorKind {
    pub fn span(self, start: Loc, end: Loc) -> LexError {
        LexError {
            kind: self,
            start,
            end,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{kind} {end}")]
pub struct LexError {
    pub kind: LexErrorKind,
    pub start: Loc,
    pub end: Loc,
}

fn found_char(found: Option<char>) -> String {
    if let Some(c) = found {
        format!("found {:?}", c)
    } else {
        String::new()
    }
}

pub fn lex<R>(input: R) -> Result<Vec<Token>, LexError>
where
    R: Read,
{
    let mut tokens = Vec::new();
    let col = Cell::new(0);
    let line = Cell::new(1);
    let mut chars = itertools::put_back(unicode_reader::CodePoints::from(input.bytes()).inspect(
        |c| {
            if let Ok(c) = c {
                match c {
                    '\n' => {
                        col.set(0);
                        line.set(line.get() + 1);
                    }
                    '\r' => col.set(0),
                    _ => col.set(col.get() + 1),
                }
            }
        },
    ));
    let mut brackets = Vec::new();
    macro_rules! loc {
        () => {
            Loc {
                line: line.get(),
                col: col.get(),
            }
        };
    }
    while let Some(c) = chars.next() {
        let start = loc!();
        macro_rules! ok {
            ($res:expr) => {
                $res.map_err(|e| LexErrorKind::from(e).span(start.clone(), loc!()))?
            };
        }
        let tt = match ok!(c) {
            // String literals
            '"' => {
                let mut s = String::new();
                let mut closed = false;
                while let Some(c) = chars.next() {
                    match ok!(c) {
                        '"' => {
                            closed = true;
                            break;
                        }
                        '\\' => {
                            if let Some(c) = chars.next() {
                                s.push(ok!(escaped_char(ok!(c))));
                            }
                        }
                        c => s.push(c),
                    }
                }
                if !closed {
                    return Err(LexErrorKind::Expected {
                        expected: '"',
                        found: None,
                    }
                    .span(start, loc!()));
                }
                TT::String(s)
            }
            // Character literals
            c if c == '\'' => {
                let mut c = ok!(chars
                    .next()
                    .ok_or_else(|| LexErrorKind::ExpectedCharacter.span(start.clone(), loc!()))?);
                if c == '\\' {
                    c = ok!(escaped_char(ok!(chars.next().ok_or_else(|| {
                        LexErrorKind::ExpectedCharacter.span(start.clone(), loc!())
                    })?)));
                };
                let next = ok!(chars
                    .next()
                    .ok_or_else(|| LexErrorKind::ExpectedCharacter.span(start.clone(), loc!()))?);
                if next != '\'' {
                    return Err(LexErrorKind::Expected {
                        expected: '\'',
                        found: Some(next),
                    }
                    .span(start, loc!()));
                }
                TT::Char(c)
            }
            // Num literals
            c if c.is_digit(10) || c == '-' => {
                let mut s: String = c.into();
                let mut period = false;
                while let Some(c) = chars.next() {
                    let c = ok!(c);
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
                    TT::Float(ok!(s.parse()))
                } else if s.starts_with('-') {
                    TT::Int(ok!(s.parse()))
                } else {
                    TT::Nat(ok!(s.parse()))
                }
            }
            // Brackets
            '[' => {
                brackets.push('[');
                TT::OpenBracket
            }
            ']' => {
                if let Some(brack) = brackets.pop() {
                    if brack != '[' {
                        return Err(LexErrorKind::InvalidBracket(']').span(start, loc!()));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket(']').span(start, loc!()));
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
                        return Err(LexErrorKind::InvalidBracket('}').span(start, loc!()));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket('}').span(start, loc!()));
                };
                TT::CloseCurly
            }
            '(' => {
                brackets.push('(');
                TT::OpenParen
            }
            ')' => {
                if let Some(brack) = brackets.pop() {
                    if brack != '(' {
                        return Err(LexErrorKind::InvalidBracket(')').span(start, loc!()));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket(')').span(start, loc!()));
                };
                TT::CloseParen
            }
            c if c.is_whitespace() => continue,
            // Idents
            c if ident_char(c) => {
                let mut s: String = c.into();
                while let Some(c) = chars.next() {
                    let c = ok!(c);
                    if ident_char(c) {
                        s.push(c);
                    } else {
                        chars.put_back(Ok(c));
                        break;
                    }
                }
                TT::Ident(s)
            }
            c => return Err(LexErrorKind::InvalidCharacter(c).span(start, loc!())),
        };
        tokens.push(Token {
            tt,
            start,
            end: Loc {
                line: line.get(),
                col: col.get(),
            },
        });
    }
    if let Some(brack) = brackets.pop() {
        return Err(LexErrorKind::UnmatchedBracket(brack).span(loc!(), loc!()));
    }
    Ok(tokens)
}

fn escaped_char(c: char) -> Result<char, LexErrorKind> {
    Ok(match c {
        '"' | '\\' => c,
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        c => return Err(LexErrorKind::InvalidEscapeSequence(c)),
    })
}

fn ident_char(c: char) -> bool {
    c > ' ' && c as u32 != 127
}
