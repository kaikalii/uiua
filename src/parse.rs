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
    put_back: Option<Token>,
    defs: Vec<UnresolvedDef>,
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
        let token = if let Some(token) = self.put_back.take().or_else(|| self.tokens.next()) {
            token
        } else {
            return Err(ParseErrorKind::Expected(expected).span(start, start));
        };
        if let Some(res) = f(token.tt.clone()) {
            Ok(Spanned {
                data: res,
                start: token.start,
                end: token.end,
            })
        } else {
            self.put_back = Some(token.clone());
            Err(ParseErrorKind::ExpectedFound {
                expected,
                found: token.tt,
            }
            .span(token.start, token.end))
        }
    }
    fn try_mat<F, R>(
        &mut self,
        expected: &'static str,
        start: Loc,
        f: F,
    ) -> Result<Option<Spanned<R>>, ParseError>
    where
        F: Fn(TT) -> Option<R>,
    {
        match self.mat(expected, start, f) {
            Ok(sp) => Ok(Some(sp)),
            Err(ParseError {
                kind: ParseErrorKind::ExpectedFound { .. },
                ..
            }) => Ok(None),
            Err(e) => Err(e),
        }
    }
    fn def(&mut self, start: Loc) -> Result<Loc, ParseError> {
        let colon = self.mat(":", start, |tt| bool_op(tt == TT::Colon))?;
        let name = self.mat("identifier", colon.end, TT::ident)?;
        let mut nodes = Vec::new();
        let mut end = name.end;
        loop {
            if let Some(node) = self.try_mat("term", end, TT::node)? {
                end = node.end;
                nodes.push(node.data);
            } else {
                end = self.mat(";", end, |tt| bool_op(tt == TT::SemiColon))?.end;
                break;
            }
        }
        self.defs.push(UnresolvedDef {
            name: name.data,
            nodes,
        });
        Ok(end)
    }
}

pub fn parse<R>(input: R) -> Result<Vec<UnresolvedDef>, ParseError>
where
    R: Read,
{
    let tokens = lex(input)?;
    let mut parser = Parser {
        tokens: tokens.into_iter(),
        put_back: None,
        defs: Vec::new(),
    };
    let mut end = Loc::new(1, 1);
    while !parser.tokens.as_slice().is_empty() {
        end = parser.def(end)?;
    }
    Ok(parser.defs)
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
