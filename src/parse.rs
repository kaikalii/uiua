use std::io::Read;

use crate::{ast::*, lex::*, span::*, types::*};

#[derive(Debug, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    Lex(LexErrorKind),
    #[error("Expected '{0}'")]
    Expected(&'static str),
    #[error("Expected '{expected}' found '{found}'")]
    ExpectedFound { expected: &'static str, found: TT },
}

impl ParseErrorKind {
    pub fn span(self, span: Span) -> ParseError {
        ParseError { kind: self, span }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{kind} {}", span.end)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError {
            kind: ParseErrorKind::Lex(e.kind),
            span: e.span,
        }
    }
}

struct Parser {
    tokens: std::vec::IntoIter<Token>,
    put_back: Option<Token>,
    defs: Vec<Sp<UnresolvedWord>>,
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
    ) -> Result<Sp<T::Output>, ParseError> {
        let token = if let Some(token) = self.next_token() {
            token
        } else {
            return Err(ParseErrorKind::Expected(expected).span(Span::new(self.loc, self.loc)));
        };
        if let Some(res) = transform.transform(token.tt.clone()) {
            self.loc = token.span.end;
            Ok(token.span.sp(res))
        } else {
            self.put_back = Some(token.clone());
            Err(ParseErrorKind::ExpectedFound {
                expected,
                found: token.tt,
            }
            .span(token.span))
        }
    }
    fn try_mat<T: TTTransform>(
        &mut self,
        expected: &'static str,
        transform: T,
    ) -> Option<Sp<T::Output>> {
        self.mat(expected, transform).ok()
    }
    /// Match an identifier
    fn ident(&mut self) -> Result<Option<Sp<Ident>>, ParseError> {
        Ok(if let Some(first) = self.try_mat("identifier", TT::ident) {
            Some(if self.try_mat(".", TT::Period).is_some() {
                let second = self.mat("identifier", TT::ident)?;
                (first.span - second.span).sp(Ident::new(Some(first.data), second.data))
            } else {
                first.map(|s| Ident::new(None, s))
            })
        } else {
            None
        })
    }
    /// Match a type
    fn ty(&mut self) -> Result<Option<Sp<UnresolvedType>>, ParseError> {
        Ok(if let Some(ident) = self.ident()? {
            Some(ident.span.sp(match (&ident.module, ident.name.as_str()) {
                (None, "Bool") => UnresolvedType::Prim(Primitive::Bool),
                (None, "Nat") => UnresolvedType::Prim(Primitive::Nat),
                (None, "Int") => UnresolvedType::Prim(Primitive::Int),
                (None, "Float") => UnresolvedType::Prim(Primitive::Float),
                (None, "Text") => UnresolvedType::Prim(Primitive::Text),
                _ => UnresolvedType::Ident(ident.data),
            }))
        } else if let Some(open_bracket) = self.try_mat("[", TT::OpenBracket) {
            let start = open_bracket.span.start;
            if let Some(ty) = self.ty()? {
                let end = self.mat("]", TT::CloseBracket)?.span.end;
                Some(Span::new(start, end).sp(UnresolvedType::Prim(Primitive::List(Box::new(ty)))))
            } else {
                return self.expected("type");
            }
        } else if let Some(sig) = self.sig(None)? {
            Some(sig.map(|sig| UnresolvedType::Prim(Primitive::Quotation(sig))))
        } else {
            return Ok(None);
        })
    }
    /// Match type parameter declaration
    fn type_params(&mut self) -> Result<UnresolvedTypeParams, ParseError> {
        let start = self.loc;
        // Match type params names
        let mut names = Vec::new();
        let mut end = start;
        while let Some(name) = self.try_mat("type parameter", TT::ident) {
            end = name.span.end;
            names.push(name);
        }
        Ok(Span::new(start, end).sp(names))
    }
    /// Match a type signature
    fn sig(
        &mut self,
        type_params: Option<UnresolvedTypeParams>,
    ) -> Result<Option<UnresolvedSignature>, ParseError> {
        Ok(if let Some(open_paren) = self.try_mat("(", TT::OpenParen) {
            let start = open_paren.span.start;
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
            let end = self.mat(")", TT::CloseParen)?.span.end;
            Some(Span::new(start, end).sp(Signature::with_bounds(type_params, before, after)))
        } else {
            None
        })
    }
    /// Match a sequence of terms
    fn seq(&mut self) -> Result<Vec<Sp<UnresolvedNode>>, ParseError> {
        let mut nodes = Vec::new();
        loop {
            if let Some(node) = self.try_mat("term", TT::node) {
                nodes.push(node);
            } else if let Some(ident) = self.ident()? {
                nodes.push(ident.map(UnresolvedNode::Ident));
            } else if self.try_mat("[", TT::OpenBracket).is_some() {
                let sub_nodes = self.seq()?;
                let span = sub_nodes
                    .first()
                    .zip(sub_nodes.last())
                    .map(|(a, b)| a.span - b.span)
                    .unwrap_or_else(|| Span::new(self.loc, self.loc));
                nodes.push(span.sp(UnresolvedNode::Quotation(sub_nodes)));
                self.mat("]", TT::CloseBracket)?;
            } else {
                break;
            }
        }
        Ok(nodes)
    }
    /// Match a word definition
    fn word(&mut self) -> Result<(), ParseError> {
        // Match the colon
        let start = self.mat(":", TT::Colon)?.span.start;
        // Match the name
        let name = self.mat("identifier", TT::ident)?;
        // Match optional type parameters
        let type_params = self.type_params()?;
        // Match an optional type signature
        let sig = self.sig(Some(type_params))?;
        // Match equals sign
        self.mat("=", TT::Equals)?;
        // Match the nodes
        let nodes = self.seq()?;
        // Match the closing semicolon
        self.try_mat(";", TT::SemiColon);
        let end = self.loc;
        self.defs.push(Span::new(start, end).sp(UnresolvedWord {
            name: name.map(|name| Ident::new(None, name)),
            sig,
            nodes,
        }));
        Ok(())
    }
    fn expected<T>(&mut self, expected: &'static str) -> Result<T, ParseError> {
        Err(if let Some(token) = self.next_token() {
            ParseErrorKind::ExpectedFound {
                expected,
                found: token.tt,
            }
            .span(token.span)
        } else {
            ParseErrorKind::Expected(expected).span(Span::new(self.loc, self.loc))
        })
    }
}

pub fn parse<R>(input: R) -> Result<Vec<Sp<UnresolvedWord>>, ParseError>
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
        parser.word()?;
    }
    Ok(parser.defs)
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
