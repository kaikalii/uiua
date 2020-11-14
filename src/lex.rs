use std::{
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
    Equals,
    Rule,
    Follow,
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
            TT::Equals => "=".fmt(f),
            TT::Rule => "rule".fmt(f),
            TT::Follow => "follow".fmt(f),
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

struct Lexer<R>
where
    R: Read,
{
    tokens: Vec<Token>,
    chars: unicode_reader::CodePoints<io::Bytes<R>>,
    brackets: Vec<char>,
    loc: Loc,
    start: Loc,
}

impl<R> Lexer<R>
where
    R: Read,
{
    fn next_char(&mut self) -> Result<char, LexError> {
        self.try_next_char()
            .unwrap_or_else(|| Err(LexErrorKind::ExpectedCharacter.span(self.start, self.loc)))
    }
    fn try_next_char(&mut self) -> Option<Result<char, LexError>> {
        self.chars.next().map(|res| {
            match res {
                Ok('\n') => {
                    self.loc.line += 1;
                    self.loc.col = 0;
                }
                Ok('\r') => {}
                Ok('\t') => self.loc.col += 4,
                Ok(_) => self.loc.col += 1,
                Err(_) => {}
            }
            self.span_res(res)
        })
    }
    fn handle_char(&mut self, c: Result<char, LexError>) -> Result<(), LexError> {
        self.start = self.loc;
        let c = c?;
        let tt = match c {
            // String literals
            '"' => {
                let mut s = String::new();
                let mut closed = false;
                while let Some(c) = self.try_next_char() {
                    match c? {
                        '"' => {
                            closed = true;
                            break;
                        }
                        '\\' => {
                            if let Some(c) = self.try_next_char() {
                                s.push(escaped_char(c?).map_err(|e| e.span(self.start, self.loc))?);
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
                    .span(self.start, self.loc));
                }
                TT::Text(s)
            }
            // Character literals
            c if c == '\'' => {
                let mut c = self.next_char()?;
                if c == '\\' {
                    c = self.next_char()?;
                };
                let next = self.next_char()?;
                if next != '\'' {
                    return Err(LexErrorKind::Expected {
                        expected: '\'',
                        found: Some(next),
                    }
                    .span(self.start, self.loc));
                }
                TT::Char(c)
            }
            // Num literals or double dash
            c if c.is_digit(10) || c == '-' || c == '+' => {
                let mut non_number = None;
                if c == '-' {
                    if let Some(next) = self.try_next_char() {
                        let next = next?;
                        if next == '-' {
                            // Check for fold
                            if let Some(c) = self.try_next_char() {
                                let c = c?;
                                if c == '-' {
                                    for _ in self.chars.by_ref() {}
                                    return Ok(());
                                } else {
                                    let tt = self.finish_number(c, Some(TT::DoubleDash))?;
                                    self.push_tt(tt);
                                    self.handle_char(Ok(c))?;
                                    return Ok(());
                                }
                            }
                        } else {
                            let tt = self.finish_number(c, non_number)?;
                            self.push_tt(tt);
                            self.handle_char(Ok(next))?;
                            return Ok(());
                        }
                    } else {
                        non_number = Some(TT::Ident("-".into()));
                    }
                }
                self.finish_number(c, non_number)?
            }
            // Brackets
            '[' => {
                self.brackets.push('[');
                TT::OpenBracket
            }
            ']' => {
                if let Some(brack) = self.brackets.pop() {
                    if brack != '[' {
                        return Err(LexErrorKind::InvalidBracket(']').span(self.start, self.loc));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket(']').span(self.start, self.loc));
                };
                TT::CloseBracket
            }
            '{' => {
                self.brackets.push('{');
                TT::OpenCurly
            }
            '}' => {
                if let Some(brack) = self.brackets.pop() {
                    if brack != '{' {
                        return Err(LexErrorKind::InvalidBracket('}').span(self.start, self.loc));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket('}').span(self.start, self.loc));
                };
                TT::CloseCurly
            }
            '(' => {
                self.brackets.push('(');
                TT::OpenParen
            }
            ')' => {
                if let Some(brack) = self.brackets.pop() {
                    if brack != '(' {
                        return Err(LexErrorKind::InvalidBracket(')').span(self.start, self.loc));
                    }
                } else {
                    return Err(LexErrorKind::UnmatchedBracket(')').span(self.start, self.loc));
                };
                TT::CloseParen
            }
            ':' => TT::Colon,
            ';' => TT::SemiColon,
            '~' => TT::Tilde,
            '.' => TT::Period,
            '=' => TT::Equals,
            c if c.is_whitespace() => return Ok(()),
            // Idents and others
            c if ident_char(c) => {
                let mut s: String = c.into();
                while let Some(c) = self.try_next_char() {
                    let c = c?;
                    if ident_char(c) {
                        s.push(c);
                    } else {
                        self.push_tt_drop_col(ident_str_as_tt(s));
                        self.handle_char(Ok(c))?;
                        return Ok(());
                    }
                }
                ident_str_as_tt(s)
            }
            c => return Err(LexErrorKind::InvalidCharacter(c).span(self.start, self.loc)),
        };
        self.push_tt(tt);
        Ok(())
    }
    fn push_tt_drop_col(&mut self, tt: TT) {
        let mut end = self.loc;
        end.col = end.col.saturating_sub(1);
        self.tokens.push(Token {
            tt,
            span: Span::new(self.start, end),
        })
    }
    fn push_tt(&mut self, tt: TT) {
        assert!(self.start <= self.loc);
        self.tokens.push(Token {
            tt,
            span: Span::new(self.start, self.loc),
        })
    }
    fn finish_number(&mut self, c: char, non_number: Option<TT>) -> Result<TT, LexError> {
        Ok(if let Some(tt) = non_number {
            tt
        } else {
            let mut s: String = c.into();
            let mut period = false;
            while let Some(c) = self.try_next_char() {
                let c = c?;
                if c.is_digit(10) || c == '.' && !period {
                    if c == '.' {
                        period = true;
                    }
                    s.push(c);
                } else {
                    self.handle_char(Ok(c))?;
                    break;
                }
            }
            if s == "-" || s == "+" {
                TT::Ident(s)
            } else if period {
                TT::Float(self.span_res(s.parse())?)
            } else if s.starts_with('-') || s.starts_with('+') {
                TT::Int(self.span_res(s.parse())?)
            } else {
                TT::Nat(self.span_res(s.parse())?)
            }
        })
    }
    fn span_res<T, E>(&self, res: Result<T, E>) -> Result<T, LexError>
    where
        E: Into<LexErrorKind>,
    {
        res.map_err(|e| e.into().span(self.start, self.loc))
    }
}

fn ident_str_as_tt(s: String) -> TT {
    match s.as_str() {
        "true" => TT::Bool(true),
        "false" => TT::Bool(false),
        "rule" => TT::Rule,
        "follow" => TT::Follow,
        _ => TT::Ident(s),
    }
}

pub fn lex<R>(input: R) -> Result<Vec<Token>, LexError>
where
    R: Read,
{
    let mut lexer = Lexer {
        loc: Loc::new(1, 0),
        start: Loc::new(1, 0),
        brackets: Vec::new(),
        chars: unicode_reader::CodePoints::from(input.bytes()),
        tokens: Vec::new(),
    };
    while let Some(c) = lexer.try_next_char() {
        lexer.handle_char(c)?;
    }
    if let Some(brack) = lexer.brackets.pop() {
        return Err(LexErrorKind::UnmatchedBracket(brack).span(lexer.start, lexer.loc));
    }
    Ok(lexer.tokens)
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
