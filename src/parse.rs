use std::io::Read;

use crate::{ast::*, lex::*};

#[derive(Debug, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    Lex(LexErrorKind),
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

pub fn parse<R>(input: R) -> Result<Vec<Node>, ParseError>
where
    R: Read,
{
    let mut nodes = Vec::new();
    let tokens = lex(input)?;
    Ok(nodes)
}
