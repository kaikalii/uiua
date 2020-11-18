use std::{
    collections::VecDeque,
    fmt,
    io::{self, Read},
    iter::Peekable,
};

use itertools::*;

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
    Period,
    Colon,
    WatchColon,
    TestColon,
    Equals,
    Backslash,
    QuestionMark,
    Type,
    Data,
    Unique,
    Use,
    DocComment(String),
    Comment(String),
    WhiteSpace(String),
}

impl TT {
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
    pub fn whitespace(self) -> Option<String> {
        if let TT::WhiteSpace(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub fn comment(self) -> Option<String> {
        if let TT::Comment(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub fn doc_comment(self) -> Option<String> {
        if let TT::DocComment(s) = self {
            Some(s)
        } else {
            None
        }
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
            TT::WatchColon => ":>".fmt(f),
            TT::TestColon => ":test>".fmt(f),
            TT::Equals => "=".fmt(f),
            TT::Backslash => "\\".fmt(f),
            TT::QuestionMark => "?".fmt(f),
            TT::Type => "type".fmt(f),
            TT::Data => "data".fmt(f),
            TT::Unique => "unique".fmt(f),
            TT::Use => "use".fmt(f),
            TT::Period => ".".fmt(f),
            TT::WhiteSpace(s) => s.fmt(f),
            TT::Comment(s) => write!(f, "` {}", s),
            TT::DocComment(s) => write!(f, "`` {}", s),
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

pub struct Lexer<R>
where
    R: Read,
{
    chars: Peekable<unicode_reader::CodePoints<io::Bytes<R>>>,
    put_back: VecDeque<io::Result<char>>,
    brackets: Vec<char>,
    loc: Loc,
    start: Loc,
}

impl<R> Lexer<R>
where
    R: Read,
{
    pub fn new(input: R) -> Self {
        Lexer {
            loc: Loc::new(1, 0),
            start: Loc::new(1, 0),
            brackets: Vec::new(),
            chars: unicode_reader::CodePoints::from(input.bytes()).peekable(),
            put_back: VecDeque::new(),
        }
    }
    fn next_char(&mut self) -> Result<char, LexError> {
        self.try_next_char()
            .unwrap_or_else(|| Err(LexErrorKind::ExpectedCharacter.span(self.start, self.loc)))
    }
    fn try_next_char(&mut self) -> Option<Result<char, LexError>> {
        if let Some(c) = self.put_back.pop_front().or_else(|| self.chars.next()) {
            match c {
                Ok('\n') => {
                    self.loc.line += 1;
                    self.loc.col = 0;
                }
                Ok('\t') => self.loc.col += 4,
                Ok('\u{0}') => return self.try_next_char(),
                Ok(_) => self.loc.col += 1,
                Err(_) => {}
            }
            Some(self.span_res(c))
        } else {
            self.loc.col += 1;
            None
        }
    }
    fn peek(&mut self) -> Option<&io::Result<char>> {
        let chars = &mut self.chars;
        self.put_back.front().or_else(move || chars.peek())
    }
    fn next_token(&mut self) -> Result<Option<Token>, LexError> {
        let c = if let Some(c) = self.try_next_char() {
            c?
        } else {
            return Ok(None);
        };
        self.start = self.loc;
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
            '=' => TT::Equals,
            '\\' => TT::Backslash,
            '`' => {
                let is_doc = if let Some(Ok('`')) = self.peek() {
                    self.next_char()?;
                    true
                } else {
                    false
                };
                let mut comment = String::new();
                let mut hit_non_whitespace = false;
                while let Some(Ok(c)) = self.peek() {
                    let c = *c;
                    if c == '\n' {
                        break;
                    }
                    self.next_char()?;
                    if c.is_whitespace() {
                        if !hit_non_whitespace {
                            continue;
                        }
                    } else {
                        hit_non_whitespace = true;
                    }
                    comment.push(c);
                }
                if is_doc {
                    TT::DocComment(comment)
                } else {
                    TT::Comment(comment)
                }
            }
            c if c.is_whitespace() => {
                let mut s = String::from(c);
                while let Some(Ok(c)) = self.peek() {
                    let c = *c;
                    if c.is_whitespace() {
                        self.next_char()?;
                        s.push(c);
                    } else {
                        break;
                    }
                }
                TT::WhiteSpace(s)
            }
            // Idents and others
            c if ident_char(c) => {
                let mut s: String = c.into();
                while let Some(Ok(c)) = self.peek() {
                    let c = *c;
                    if ident_char(c) {
                        self.next_char()?;
                        s.push(c);
                    } else {
                        break;
                    }
                }
                if let (true, Ok(i)) = (s.starts_with('-') || s.starts_with('+'), s.parse::<i64>())
                {
                    TT::Int(i)
                } else if let Ok(n) = s.parse::<u64>() {
                    TT::Nat(n)
                } else if let Ok(f) = s.parse::<f64>() {
                    TT::Float(f)
                } else {
                    match s.as_str() {
                        "?" => TT::QuestionMark,
                        ":>" => TT::WatchColon,
                        ":test" => TT::TestColon,
                        ":" => TT::Colon,
                        "." => TT::Period,
                        "---" => return Ok(None),
                        "--" => TT::DoubleDash,
                        s => {
                            if s.starts_with('?') {
                                self.put_back_str(s, "?", Some(2));
                                return self.next_token();
                            } else if let Some(sub) = [":>", ":", ".", "---", "--"]
                                .iter()
                                .find(|&&sub| s.contains(sub))
                            {
                                self.put_back_str(s, sub, None);
                                return self.next_token();
                            } else {
                                match s {
                                    "true" => TT::Bool(true),
                                    "false" => TT::Bool(false),
                                    "type" => TT::Type,
                                    "data" => TT::Data,
                                    "use" => TT::Use,
                                    "unique" => TT::Unique,
                                    _ => TT::Ident(s.into()),
                                }
                            }
                        }
                    }
                }
            }
            c => return Err(LexErrorKind::InvalidCharacter(c).span(self.start, self.loc)),
        };
        Ok(Some(Token {
            tt,
            span: Span::new(self.start, self.loc),
        }))
    }
    fn span_res<T, E>(&self, res: Result<T, E>) -> Result<T, LexError>
    where
        E: Into<LexErrorKind>,
    {
        res.map_err(|e| e.into().span(self.start, self.loc))
    }
    fn put_back_str(&mut self, s: &str, split: &str, n: Option<usize>) {
        self.put_back.extend(
            s.splitn(n.unwrap_or(10000), split)
                .intersperse(&format!("\u{0}{}\u{0}", split))
                .map(|s| s.chars())
                .flatten()
                .map(Ok),
        );
        self.loc.col = self.loc.col.saturating_sub(s.len());
    }
    pub fn finish(&self) -> Result<(), LexError> {
        if let Some(brack) = self.brackets.last() {
            return Err(LexErrorKind::UnmatchedBracket(*brack).span(self.start, self.loc));
        }
        Ok(())
    }
}

impl<R> Iterator for Lexer<R>
where
    R: Read,
{
    type Item = Result<Token, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
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
    c > ' ' && c as u32 != 127 && !"=\\[]{}()".contains(c)
}
