use std::{convert::*, io::Read};

use crate::{ast::*, lex::*, span::*, types::*};

#[derive(Debug, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    Lex(LexErrorKind),
    #[error("Expected {0}")]
    Expected(String),
    #[error("Expected {expected}, but found {found}")]
    ExpectedFound { expected: String, found: String },
    #[error("Expected {expected}, but found '{found}'")]
    ExpectedFoundTT { expected: String, found: TT },
    #[error("Expected signature because of type parameters")]
    ExpectedSignature,
    #[error("{0}")]
    IdentParse(#[from] IdentParseError),
    #[error("Cannot apply doc comments to use statements")]
    DocCommentOnUse,
    #[error("Lists must have exactly one type, and quotations require a '--'")]
    InvalidListOrQuotation,
    #[error("{0} is not a valid tuple size. Tuples may have between 2 and 8 items")]
    InvalidTupleSize(usize),
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

enum SigOrList {
    Sig(UnresolvedSignature),
    List(Sp<UnresolvedType>),
}

struct Parser {
    tokens: std::vec::IntoIter<Token>,
    put_back: Option<Token>,
    items: Vec<UnresolvedItem>,
    whitespace: Option<Sp<String>>,
    loc: Loc,
}

impl Parser {
    fn next_token(&mut self) -> Option<Token> {
        self.put_back.take().or_else(|| self.tokens.next())
    }
    fn mat<E: Expectation, T: TTTransform>(
        &mut self,
        expected: E,
        transform: T,
    ) -> Result<Sp<T::Output>, ParseError> {
        let token = if let Some(token) = self.next_token() {
            token
        } else {
            return Err(
                ParseErrorKind::Expected(expected.expected()).span(Span::new(self.loc, self.loc))
            );
        };
        if let Some(res) = transform.transform(token.tt.clone()) {
            self.loc = token.span.end;
            Ok(token.span.sp(res))
        } else if let TT::WhiteSpace(s) = token.tt {
            let span = Span::new(self.loc, self.loc);
            let whitespace = self
                .whitespace
                .get_or_insert_with(|| span.sp(String::new()));
            whitespace.push_str(&s);
            whitespace.span.end = self.loc;
            self.mat(expected, transform)
        } else {
            self.put_back = Some(token.clone());
            Err(ParseErrorKind::ExpectedFoundTT {
                expected: expected.expected(),
                found: token.tt.clone(),
            }
            .span(token.span))
        }
    }
    fn try_mat<T: TTTransform>(&mut self, transform: T) -> Option<Sp<T::Output>> {
        self.mat("", transform).ok()
    }
    fn clear_whitespace(&mut self) {
        self.whitespace = None;
    }
    /// Try to match some unhashed
    fn unhashed(&mut self) -> Option<Sp<Unhashed>> {
        if let Some(whitespace) = self.whitespace.take() {
            Some(whitespace.map(Unhashed::WhiteSpace))
        } else if let Some(whitespace) = self.try_mat(TT::whitespace) {
            Some(whitespace.map(Unhashed::WhiteSpace))
        } else if let Some(comment) = self.try_mat(TT::comment) {
            Some(comment.map(Unhashed::Comment))
        } else {
            None
        }
    }
    /// Match an identifier
    fn ident(&mut self) -> Result<Option<Sp<Ident>>, ParseError> {
        Ok(if let Some(first) = self.try_mat(TT::ident) {
            Some(if self.try_mat(TT::Period).is_some() {
                let second = self.mat("name", TT::ident)?;
                (first.span - second.span).sp(Ident::module(first.data, second.data))
            } else {
                first.map(Ident::no_module)
            })
        } else {
            None
        })
    }
    /// Try to match a type
    fn try_ty(&mut self) -> Result<Option<Sp<UnresolvedType>>, ParseError> {
        Ok(if let Some(ident) = self.ident()? {
            Some(ident.span.sp(match (&ident.module, ident.name.as_str()) {
                (None, "Bool") => UnresolvedType::Prim(Primitive::Bool),
                (None, "Nat") => UnresolvedType::Prim(Primitive::Nat),
                (None, "Int") => UnresolvedType::Prim(Primitive::Int),
                (None, "Float") => UnresolvedType::Prim(Primitive::Float),
                (None, "Char") => UnresolvedType::Prim(Primitive::Char),
                (None, "Text") => UnresolvedType::Prim(Primitive::Text),
                _ => {
                    let mut params_start = ident.span.end;
                    let mut params_end = params_start;
                    // Try matching given type parameters
                    let mut params = Vec::new();
                    if let Some(open_curly) = self.try_mat(TT::OpenCurly) {
                        params_start = open_curly.span.start;
                        while let Some(ty) = self.try_ty()? {
                            params.push(ty);
                        }
                        params_end = self.mat("}", TT::CloseCurly)?.span.end;
                    }
                    UnresolvedType::Ident {
                        ident,
                        params: Span::new(params_start, params_end).sp(params),
                    }
                }
            }))
        } else if let Some(open_paren) = self.try_mat(TT::OpenParen) {
            let start = open_paren.span.start;
            let mut types = Vec::new();
            while let Some(ty) = self.try_ty()? {
                types.push(ty);
            }
            let end = self.mat(')', TT::CloseParen)?.span.end;
            let span = Span::new(start, end);
            if types.len() < 2 || types.len() > 8 {
                return Err(ParseErrorKind::InvalidTupleSize(types.len()).span(span));
            }
            Some(span.sp(UnresolvedType::Prim(Primitive::Tuple(types))))
        } else if let Some(sig_or_list) = self.sig_or_list()? {
            Some(sig_or_list.map(|sol| match sol {
                SigOrList::Sig(sig) => UnresolvedType::Prim(Primitive::Quotation(sig)),
                SigOrList::List(inner) => UnresolvedType::Prim(Primitive::List(Box::new(inner))),
            }))
        } else {
            None
        })
    }
    /// Match a type
    fn ty(&mut self) -> Result<Sp<UnresolvedType>, ParseError> {
        if let Some(ty) = self.try_ty()? {
            Ok(ty)
        } else {
            self.expected("type")
        }
    }
    /// Match type parameter declaration
    fn params(&mut self) -> Result<Sp<UnresolvedParams>, ParseError> {
        let mut start = self.loc;
        let mut end = start;
        // Match type params names
        let mut names = Vec::new();
        if let Some(name) = self.try_mat(TT::ident) {
            start = name.span.start;
            end = name.span.end;
            names.push(name);
        }
        while let Some(name) = self.try_mat(TT::ident) {
            end = name.span.end;
            names.push(name);
        }
        Ok(Span::new(start, end).sp(names))
    }
    /// Match a type signature or a list
    fn sig_or_list(&mut self) -> Result<Option<Sp<SigOrList>>, ParseError> {
        Ok(if let Some(open_paren) = self.try_mat(TT::OpenBracket) {
            let start = open_paren.span.start;
            // Match before args types
            let mut before = Vec::new();
            while let Some(ty) = self.try_ty()? {
                before.push(ty);
            }
            // Match double dash
            let sig_or_list = if self.try_mat(TT::DoubleDash).is_some() {
                // Match after args types
                let mut after = Vec::new();
                while let Some(ty) = self.try_ty()? {
                    after.push(ty);
                }
                SigOrList::Sig(Signature::new_unresolved(before, after))
            } else if before.len() == 1 {
                SigOrList::List(before.remove(0))
            } else {
                let end = self.mat(']', TT::CloseBracket)?.span.end;
                return Err(ParseErrorKind::InvalidListOrQuotation.span(Span::new(start, end)));
            };
            // Match closing paren
            let end = self.mat(']', TT::CloseBracket)?.span.end;
            Some(Span::new(start, end).sp(sig_or_list))
        } else {
            None
        })
    }
    /// Match a type signature
    fn sig(&mut self) -> Result<Option<Sp<UnresolvedSignature>>, ParseError> {
        if let Some(sig_or_list) = self.sig_or_list()? {
            match sig_or_list.data {
                SigOrList::Sig(sig) => Ok(Some(sig_or_list.span.sp(sig))),
                SigOrList::List(_) => Err(ParseErrorKind::ExpectedFound {
                    expected: "signature".into(),
                    found: "list type".into(),
                }
                .span(sig_or_list.span)),
            }
        } else {
            Ok(None)
        }
    }
    /// Match a sequence of words
    fn seq(&mut self) -> Result<Vec<Sp<UnresolvedNode>>, ParseError> {
        let mut nodes = Vec::new();
        loop {
            if let Some(unhashes) = self.unhashed() {
                nodes.push(unhashes.map(UnresolvedNode::Unhashed))
            } else if let Some(node) = self.try_mat(TT::node) {
                nodes.push(node);
            } else if let Some(ident) = self.ident()? {
                nodes.push(ident.map(UnresolvedNode::Ident));
            } else if self.try_mat(TT::OpenBracket).is_some() {
                self.clear_whitespace();
                while let Some(unhashed) = self.unhashed() {
                    nodes.push(unhashed.map(UnresolvedNode::Unhashed))
                }
                let sub_nodes = self.seq()?;
                let span = sub_nodes
                    .first()
                    .zip(sub_nodes.last())
                    .map(|(a, b)| a.span - b.span)
                    .unwrap_or_else(|| Span::new(self.loc, self.loc));
                nodes.push(span.sp(UnresolvedNode::Quotation(sub_nodes)));
                self.mat(']', TT::CloseBracket)?;
            } else {
                break;
            }
        }
        Ok(nodes)
    }
    /// Match a word definition
    fn word(
        &mut self,
        doc: String,
        colon: Sp<()>,
        purpose: Sp<UnresolvedWordPurpose>,
    ) -> Result<Sp<UnresolvedWord>, ParseError> {
        let start = colon.span.start;
        let mut sig = None;
        let mut params = Span::new(start, start).sp(Vec::new());
        if purpose.name().is_some() {
            // Match type parameters
            params = self.params()?;
            // Match an optional type signature
            sig = self.sig()?;
            if sig.is_none() && !params.is_empty() {
                return Err(ParseErrorKind::ExpectedSignature.span(params.span));
            }
            // Match equals sign
            self.mat('=', TT::Equals)?;
        }
        self.clear_whitespace();
        // Match the nodes
        let nodes = self.seq()?;
        Ok(Span::new(start, self.loc).sp(UnresolvedWord {
            purpose,
            doc,
            params,
            sig,
            nodes,
        }))
    }
    /// Match a data type alias
    fn data(&mut self, data_token: Sp<()>) -> Result<Sp<UnresolvedTypeAlias>, ParseError> {
        let start = data_token.span.start;
        // Match the name
        let name = self.mat("name", TT::ident)?;
        // Match type parameters
        let params = self.params()?;
        // Match equals sign
        self.mat('=', TT::Equals)?;
        let first = self.mat("name", TT::ident)?;
        let kind_start = first.span.start;
        let is_record_type = if self.try_mat(TT::Colon).is_some() {
            true
        } else if params.is_empty() {
            false
        } else {
            self.mat(':', TT::Colon)?;
            true
        };
        Ok(if is_record_type {
            // Record
            let first_ty = self.ty()?;
            let mut fields = vec![(first.span - first_ty.span).sp(UnresolvedField {
                name: first,
                ty: first_ty,
            })];
            while let Some(name) = self.try_mat(TT::ident) {
                self.mat(':', TT::Colon)?;
                let ty = self.ty()?;
                fields.push((name.span - ty.span).sp(UnresolvedField { name, ty }));
            }
            Span::new(start, self.loc).sp(UnresolvedTypeAlias {
                name,
                unique: true,
                kind: Span::new(kind_start, self.loc)
                    .sp(UnresolvedTypeAliasKind::Record { params, fields }),
            })
        } else {
            // Enum
            let mut variant_name = first;
            let mut variants = Vec::new();
            loop {
                variants.push(variant_name);
                variant_name = if let Some(name) = self.try_mat(TT::ident) {
                    name
                } else {
                    break;
                }
            }
            Span::new(start, self.loc).sp(UnresolvedTypeAlias {
                name,
                unique: true,
                kind: Span::new(kind_start, self.loc).sp(UnresolvedTypeAliasKind::Enum(variants)),
            })
        })
    }
    /// Match an item
    fn item(&mut self) -> Result<Option<UnresolvedItem>, ParseError> {
        let mut doc = String::new();
        while let Some(comment) = self.try_mat(TT::doc_comment) {
            doc += &comment;
            doc += "\n";
        }
        if self.unhashed().is_some() {
            return Ok(None);
        } else if let Some(colon) = self.try_mat(TT::Colon) {
            let purpose = self.mat("name", TT::ident)?.map(WordPurpose::Regular);
            self.word(doc, colon, purpose).map(UnresolvedItem::Word)
        } else if let Some(colon) = self.try_mat(TT::TestColon) {
            let purpose = self.mat("name", TT::ident)?.map(WordPurpose::Test);
            self.word(doc, colon, purpose).map(UnresolvedItem::Word)
        } else if let Some(colon) = self.try_mat(TT::WatchColon) {
            let purpose = colon.span.sp(WordPurpose::Watch);
            self.word(doc, colon, purpose).map(UnresolvedItem::Word)
        } else if self.try_mat(TT::Use).is_some() {
            let module = self.mat("module name", TT::ident)?;
            if !doc.is_empty() {
                return Err(ParseErrorKind::DocCommentOnUse.span(module.span));
            }
            Ok(UnresolvedItem::Use(module))
        } else if let Some(data_token) = self.try_mat(TT::Data) {
            self.data(data_token).map(UnresolvedItem::Type)
        } else if let Some(token) = self.next_token() {
            Err(ParseErrorKind::ExpectedFoundTT {
                expected: "':', 'type', 'use', or comment".into(),
                found: token.tt.clone(),
            }
            .span(token.span))
        } else {
            return Ok(None);
        }
        .map(Some)
    }
    fn expected<T, E: Expectation>(&mut self, expected: E) -> Result<T, ParseError> {
        Err(if let Some(token) = self.next_token() {
            ParseErrorKind::ExpectedFoundTT {
                expected: expected.expected(),
                found: token.tt.clone(),
            }
            .span(token.span)
        } else {
            ParseErrorKind::Expected(expected.expected()).span(Span::new(self.loc, self.loc))
        })
    }
}

pub fn parse<R>(input: R) -> Result<Vec<UnresolvedItem>, ParseError>
where
    R: Read,
{
    let tokens = lex(input)?;
    let mut parser = Parser {
        tokens: tokens.into_iter(),
        put_back: None,
        items: Vec::new(),
        whitespace: None,
        loc: Loc::new(1, 1),
    };
    while !(parser.tokens.as_slice().is_empty() && parser.put_back.is_none()) {
        if let Some(item) = parser.item()? {
            parser.items.push(item);
        }
    }
    Ok(parser.items)
}

trait TTTransform {
    type Output;
    fn transform(&self, tt: TT) -> Option<Self::Output>;
}

impl TTTransform for TT {
    type Output = ();
    fn transform(&self, tt: TT) -> Option<Self::Output> {
        if self == &tt {
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
    fn transform(&self, tt: TT) -> Option<Self::Output> {
        self(tt)
    }
}

trait Expectation {
    fn expected(self) -> String;
}

impl Expectation for char {
    fn expected(self) -> String {
        format!("'{}'", self)
    }
}

impl<'a> Expectation for &'a str {
    fn expected(self) -> String {
        self.into()
    }
}

impl<'a> Expectation for &'a [&'a str] {
    fn expected(self) -> String {
        let mut s = String::new();
        for (i, item) in self.iter().enumerate() {
            if s.chars().all(char::is_alphabetic) {
                s.push_str(&format!("'{}'", item));
            } else {
                s.push_str(item);
            }
            if self.len() >= 2 && i == self.len() - 2 {
                s.push_str(" or ");
            } else if !self.is_empty() && i < self.len() - 1 {
                s.push_str(", ");
            }
        }
        s
    }
}

#[cfg(test)]
#[test]
fn expectation_test() {
    assert_eq!("':'", ':'.expected());
    assert_eq!("':'", [":"].expected());
}
