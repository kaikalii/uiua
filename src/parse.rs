use std::io::Read;

use crate::{ast::*, lex::*};

#[derive(Debug, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    Lex(LexErrorKind),
    #[error("Expected {0}")]
    Expected(&'static str),
    #[error("Expected {expected} found '{found}'")]
    ExpectedFound { expected: &'static str, found: TT },
}

impl ParseErrorKind {
    pub fn span(self, start: Loc, end: Loc) -> ParseError {
        ParseError {
            kind: self,
            start,
            end,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{kind} {end}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub start: Loc,
    pub end: Loc,
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError {
            kind: ParseErrorKind::Lex(e.kind),
            start: e.start,
            end: e.end,
        }
    }
}

struct Parser {
    tokens: std::vec::IntoIter<Token>,
    nodes: Vec<UnresolvedNode>,
}

impl Parser {
    fn mat<F, R>(
        &mut self,
        expected: &'static str,
        start: Loc,
        f: F,
    ) -> Result<Spanned<R>, ParseError>
    where
        F: Fn(TT) -> Option<R>,
    {
        self.try_mat(expected, f)?
            .ok_or_else(|| ParseErrorKind::Expected(expected).span(start, start))
    }
    fn try_mat<F, R>(
        &mut self,
        expected: &'static str,
        f: F,
    ) -> Result<Option<Spanned<R>>, ParseError>
    where
        F: Fn(TT) -> Option<R>,
    {
        let token = if let Some(token) = self.tokens.next() {
            token
        } else {
            return Ok(None);
        };
        if let Some(res) = f(token.tt.clone()) {
            Ok(Some(Spanned {
                data: res,
                start: token.start,
                end: token.end,
            }))
        } else {
            Err(ParseErrorKind::ExpectedFound {
                expected,
                found: token.tt,
            }
            .span(token.start, token.end))
        }
    }
    fn def(&mut self) -> Result<bool, ParseError> {
        if let Some(sp) = self.try_mat(":", |tt| bool_op(tt == TT::Colon))? {
            let name = self.mat("identifier", sp.end, TT::ident)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub fn parse<R>(input: R) -> Result<Vec<UnresolvedNode>, ParseError>
where
    R: Read,
{
    let tokens = lex(input)?;
    let mut parser = Parser {
        tokens: tokens.into_iter(),
        nodes: Vec::new(),
    };
    while parser.def()? {}
    Ok(parser.nodes)
}

struct Spanned<T> {
    data: T,
    start: Loc,
    end: Loc,
}

fn bool_op(b: bool) -> Option<()> {
    if b {
        Some(())
    } else {
        None
    }
}
