use std::{
    cell::Cell,
    fmt,
    io::{self, Read},
};

use crate::{ast::*, span::*};

#[derive(Debug, Clone, PartialEq)]
pub enum TT {
    Ident(String),
    Text(String),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Float(f64),
    Char(char),
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    DoubleDash,
    Colon,
    SemiColon,
    Tilde,
    Period,
}

impl TT {
    pub fn text(self) -> Option<String> {
        if let TT::Text(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub fn ident(self) -> Option<String> {
        if let TT::Ident(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub fn node(self) -> Option<UnresolvedNode> {
        Some(match self {
            TT::Text(s) => UnresolvedNode::Literal(Literal::Text(s)),
            TT::Bool(b) => UnresolvedNode::Literal(Literal::Bool(b)),
            TT::Nat(n) => UnresolvedNode::Literal(Literal::Nat(n)),
            TT::Int(n) => UnresolvedNode::Literal(Literal::Int(n)),
            TT::Float(n) => UnresolvedNode::Literal(Literal::Float(n)),
            TT::Char(c) => UnresolvedNode::Literal(Literal::Char(c)),
            _ => return None,
        })
    }
}

impl fmt::Display for TT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TT::Ident(s) => s.fmt(f),
            TT::Text(s) => write!(f, "{:?}", s),
            TT::Bool(b) => b.fmt(f),
            TT::Nat(n) => n.fmt(f),
            TT::Int(n) => n.fmt(f),
            TT::Float(n) => n.fmt(f),
            TT::Char(c) => c.fmt(f),
            TT::OpenBracket => "[".fmt(f),
            TT::CloseBracket => "]".fmt(f),
            TT::OpenCurly => "{".fmt(f),
            TT::CloseCurly => "}".fmt(f),
            TT::OpenParen => "(".fmt(f),
            TT::CloseParen => ")".fmt(f),
            TT::DoubleDash => "--".fmt(f),
            TT::Colon => ":".fmt(f),
            TT::SemiColon => ";".fmt(f),
            TT::Tilde => "~".fmt(f),
            TT::Period => ".".fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tt: TT,
    pub span: Span,
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
            span: Span::new(start, end),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{kind} {}", span.end)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
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
    let mut put_back = None;
    let mut chars = unicode_reader::CodePoints::from(input.bytes());
    let mut brackets = Vec::new();
    macro_rules! loc {
        () => {
            Loc::new(line.get(), col.get())
        };
    }
    macro_rules! next {
        () => {
            put_back.take().or_else(|| chars.next()).map(|c| {
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
                c
            })
        };
    }
    macro_rules! put_back {
        ($c:expr) => {{
            put_back = Some($c);
            col.set(col.get().saturating_sub(1))
        }};
    }
    while let Some(c) = next!() {
        let start = loc!();
        macro_rules! ok {
            ($res:expr) => {
                $res.map_err(|e| LexErrorKind::from(e).span(start, loc!()))?
            };
        }
        let tt = match ok!(c) {
            // String literals
            '"' => {
                let mut s = String::new();
                let mut closed = false;
                while let Some(c) = next!() {
                    match ok!(c) {
                        '"' => {
                            closed = true;
                            break;
                        }
                        '\\' => {
                            if let Some(c) = next!() {
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
                TT::Text(s)
            }
            // Character literals
            c if c == '\'' => {
                let mut c = ok!(chars
                    .next()
                    .ok_or_else(|| LexErrorKind::ExpectedCharacter.span(start, loc!()))?);
                if c == '\\' {
                    c = ok!(escaped_char(ok!(next!().ok_or_else(|| {
                        LexErrorKind::ExpectedCharacter.span(start, loc!())
                    })?)));
                };
                let next = ok!(chars
                    .next()
                    .ok_or_else(|| LexErrorKind::ExpectedCharacter.span(start, loc!()))?);
                if next != '\'' {
                    return Err(LexErrorKind::Expected {
                        expected: '\'',
                        found: Some(next),
                    }
                    .span(start, loc!()));
                }
                TT::Char(c)
            }
            // Num literals or double dash
            c if c.is_digit(10) || c == '-' || c == '+' => {
                let mut non_number = None;
                if c == '-' {
                    if let Some(next) = next!() {
                        let next = ok!(next);
                        if next == '-' {
                            // Check for fold
                            if let Some(c) = next!() {
                                if let Ok('-') = c {
                                    for _ in chars.by_ref() {}
                                    continue;
                                } else {
                                    put_back!(c);
                                }
                            }
                            non_number = Some(TT::DoubleDash);
                        } else {
                            put_back!(Ok(next));
                        }
                    } else {
                        non_number = Some(TT::Ident("-".into()));
                    }
                }
                if let Some(tt) = non_number {
                    tt
                } else {
                    let mut s: String = c.into();
                    let mut period = false;
                    while let Some(c) = next!() {
                        let c = ok!(c);
                        if c.is_digit(10) || c == '.' && !period {
                            if c == '.' {
                                period = true;
                            }
                            s.push(c);
                        } else {
                            put_back!(Ok(c));
                            break;
                        }
                    }
                    if s == "-" || s == "+" {
                        TT::Ident(s)
                    } else if period {
                        TT::Float(ok!(s.parse()))
                    } else if s.starts_with('-') || s.starts_with('+') {
                        TT::Int(ok!(s.parse()))
                    } else {
                        TT::Nat(ok!(s.parse()))
                    }
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
            ':' => TT::Colon,
            ';' => TT::SemiColon,
            '~' => TT::Tilde,
            '.' => TT::Period,
            c if c.is_whitespace() => continue,
            // Idents and others
            c if ident_char(c) => {
                let mut s: String = c.into();
                while let Some(c) = next!() {
                    let c = ok!(c);
                    if ident_char(c) {
                        s.push(c);
                    } else {
                        put_back!(Ok(c));
                        break;
                    }
                }
                match s.as_str() {
                    "true" => TT::Bool(true),
                    "false" => TT::Bool(false),
                    _ => TT::Ident(s),
                }
            }
            c => return Err(LexErrorKind::InvalidCharacter(c).span(start, loc!())),
        };
        tokens.push(Token {
            tt,
            span: Span::new(start, Loc::new(line.get(), col.get())),
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
    c > ' ' && c as u32 != 127 && !".[]{}()".contains(c)
}
