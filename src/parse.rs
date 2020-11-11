use std::io::Read;

use crate::{ast::*, lex::*, types::*};

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
    loc: Loc,
}

impl Parser {
    fn next_token(&mut self) -> Option<Token> {
        self.put_back.take().or_else(|| self.tokens.next())
    }
    fn mat<T: TTTransform>(
        &mut self,
        expected: &'static str,
        transform: T,
    ) -> Result<Spanned<T::Output>, ParseError> {
        let token = if let Some(token) = self.next_token() {
            token
        } else {
            return Err(ParseErrorKind::Expected(expected).span(self.loc, self.loc));
        };
        if let Some(res) = transform.transform(token.tt.clone()) {
            self.loc = token.end;
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
    fn try_mat<T: TTTransform>(
        &mut self,
        expected: &'static str,
        transform: T,
    ) -> Result<Option<Spanned<T::Output>>, ParseError> {
        match self.mat(expected, transform) {
            Ok(sp) => Ok(Some(sp)),
            Err(ParseError {
                kind: ParseErrorKind::ExpectedFound { .. },
                ..
            }) => Ok(None),
            Err(e) => Err(e),
        }
    }
    // Match a type
    fn ty(&mut self) -> Result<Option<UnresolvedType>, ParseError> {
        Ok(if let Some(ident) = self.try_mat("type", TT::ident)? {
            Some(match ident.data.as_str() {
                "Bool" => UnresolvedType::Prim(Primitive::Bool),
                "Nat" => UnresolvedType::Prim(Primitive::Nat),
                "Int" => UnresolvedType::Prim(Primitive::Int),
                "Float" => UnresolvedType::Prim(Primitive::Float),
                "Text" => UnresolvedType::Prim(Primitive::String),
                _ => UnresolvedType::Other(ident.data),
            })
        } else if self.try_mat("[", TT::OpenBracket)?.is_some() {
            if let Some(ty) = self.ty()? {
                self.mat("]", TT::CloseBracket)?;
                Some(UnresolvedType::Prim(Primitive::List(Box::new(ty))))
            } else {
                return self.expected("type");
            }
        } else if let Some(sig) = self.sig()? {
            Some(UnresolvedType::Prim(Primitive::Op(sig)))
        } else {
            return Ok(None);
        })
    }
    // Match a type signature
    fn sig(&mut self) -> Result<Option<Signature<UnresolvedType>>, ParseError> {
        Ok(if self.try_mat("(", TT::OpenParen)?.is_some() {
            // Match before args types
            let mut before = Vec::new();
            while let Some(ty) = self.ty()? {
                before.push(ty);
            }
            // Match double dash
            self.mat("--", TT::DoubleDash)?;
            // Match after args types
            let mut after = Vec::new();
            while let Some(ty) = self.ty()? {
                after.push(ty);
            }
            // Match closing paren
            self.mat(")", TT::CloseParen)?;
            Some(Signature { before, after })
        } else {
            None
        })
    }
    // Match a sequence of terms
    fn seq(&mut self) -> Result<Vec<UnresolvedNode>, ParseError> {
        let mut nodes = Vec::new();
        loop {
            if let Some(node) = self.try_mat("term", TT::node)? {
                nodes.push(node.data);
            } else if self.try_mat("[", TT::OpenBracket)?.is_some() {
                let sub_nodes = self.seq()?;
                nodes.push(UnresolvedNode::Defered(sub_nodes));
                self.mat("]", TT::CloseBracket)?;
            } else {
                break;
            }
        }
        Ok(nodes)
    }
    // Match a definition
    fn def(&mut self) -> Result<(), ParseError> {
        // Match the colon and name
        self.mat(":", TT::Colon)?;
        let name = self.mat("identifier", TT::ident)?;
        // Match an optional type signature
        let sig = self.sig()?;
        // Match the nodes
        let nodes = self.seq()?;
        self.defs.push(UnresolvedDef {
            name: name.data,
            sig,
            nodes,
        });
        Ok(())
    }
    fn expected<T>(&mut self, expected: &'static str) -> Result<T, ParseError> {
        Err(if let Some(token) = self.next_token() {
            ParseErrorKind::ExpectedFound {
                expected,
                found: token.tt,
            }
            .span(token.start, token.end)
        } else {
            ParseErrorKind::Expected(expected).span(self.loc, self.loc)
        })
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
        loc: Loc::new(1, 1),
    };
    while !parser.tokens.as_slice().is_empty() {
        parser.def()?;
    }
    Ok(parser.defs)
}

struct Spanned<T> {
    data: T,
    start: Loc,
    end: Loc,
}

trait TTTransform {
    type Output;
    fn transform(self, tt: TT) -> Option<Self::Output>;
}

impl TTTransform for TT {
    type Output = ();
    fn transform(self, tt: TT) -> Option<Self::Output> {
        if self == tt {
            Some(())
        } else {
            None
        }
    }
}

impl<F, R> TTTransform for F
where
    F: Fn(TT) -> Option<R>,
{
    type Output = R;
    fn transform(self, tt: TT) -> Option<Self::Output> {
        self(tt)
    }
}
