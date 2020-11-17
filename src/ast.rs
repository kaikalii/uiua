use std::{convert::*, fmt, mem, str::FromStr};

use colored::*;
use serde::*;
use sha3::*;

use crate::{builtin::*, codebase::*, span::*, types::*};

type HashInner =
    digest::generic_array::GenericArray<u8, digest::generic_array::typenum::consts::U32>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Hash(pub HashInner);

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum HashDecodeError {
    #[error("Error decoding hash {0}")]
    HexDecode(#[from] hex::FromHexError),
    #[error("Decoded hash is incorrect length")]
    IncorrectLength,
}

impl TryFrom<String> for Hash {
    type Error = HashDecodeError;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        let bytes = hex::decode(&s)?;
        if bytes.len() != 32 {
            return Err(HashDecodeError::IncorrectLength);
        }
        let mut inner = HashInner::default();
        for (i, j) in inner.iter_mut().zip(bytes) {
            *i = j;
        }
        Ok(Hash(inner))
    }
}

impl From<Hash> for String {
    fn from(hash: Hash) -> Self {
        hex::encode(hash)
    }
}

pub trait TreeHash {
    fn hash(&self, sha: &mut Sha3_256);
}

impl<T> TreeHash for T
where
    T: AsRef<[u8]>,
{
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(self);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Word {
    pub sig: Signature,
    #[serde(rename = "body")]
    pub kind: WordKind,
}

impl TreeHash for Word {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(&self.kind)) });
        self.sig.hash(sha);
        match &self.kind {
            WordKind::Uiua(nodes) => {
                for node in nodes {
                    node.hash(sha);
                }
            }
            WordKind::Builtin(bi) => bi.hash(sha),
        }
    }
}

impl From<BuiltinWord> for Word {
    fn from(builtin: BuiltinWord) -> Self {
        Word {
            sig: builtin.sig(),
            kind: WordKind::Builtin(builtin),
        }
    }
}

impl Word {
    pub fn references_hash(&self, hash: &Hash) -> bool {
        if let WordKind::Uiua(nodes) = &self.kind {
            nodes.iter().any(|node| node.references_hash(hash))
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum WordKind {
    Uiua(Vec<Node>),
    Builtin(BuiltinWord),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Node {
    Ident(Hash),
    SelfIdent,
    Quotation(Vec<Node>),
    Literal(Literal),
    WhiteSpace(String),
}

impl Node {
    pub fn references_hash(&self, hash: &Hash) -> bool {
        match self {
            Node::Ident(h) => h == hash,
            Node::Quotation(nodes) => nodes.iter().any(|node| node.references_hash(hash)),
            _ => false,
        }
    }
    pub fn format(nodes: &[Node], word_name: &str, words: &ItemDefs<Word>) -> String {
        nodes
            .iter()
            .map(|node| match node {
                Node::Ident(hash) => words
                    .entry_by_hash(hash, Query::All)
                    .and_then(|entry| entry.names.iter().next().map(ToString::to_string))
                    .unwrap_or_else(|| hash.to_string()[0..8].into()),
                Node::SelfIdent => word_name.into(),
                Node::Quotation(nodes) => format!("[ {}]", Node::format(nodes, word_name, words)),
                Node::Literal(lit) => lit.to_string(),
                Node::WhiteSpace(c) => c.to_string(),
            })
            .collect::<String>()
    }
}

impl TreeHash for Node {
    fn hash(&self, sha: &mut Sha3_256) {
        if let Node::WhiteSpace(_) = self {
            return;
        }
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Node::Ident(hash) => sha.update(hash),
            Node::Quotation(nodes) => {
                for node in nodes {
                    node.hash(sha);
                }
            }
            Node::Literal(lit) => lit.hash(sha),
            Node::SelfIdent | Node::WhiteSpace(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnresolvedItem {
    Word(Sp<UnresolvedWord>),
    Data(Sp<UnresolvedData>),
}

#[derive(Debug, Clone)]
pub struct UnresolvedWord {
    pub name: Sp<String>,
    pub params: Sp<UnresolvedParams>,
    pub sig: Option<Sp<UnresolvedSignature>>,
    pub nodes: Vec<Sp<UnresolvedNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Ident {
    pub module: Option<String>,
    pub name: String,
}

impl Ident {
    pub fn new<S: Into<String>>(module: Option<S>, name: S) -> Self {
        Ident {
            module: module.map(Into::into),
            name: name.into(),
        }
    }
    pub fn no_module<S: Into<String>>(name: S) -> Self {
        Ident::new(None, name)
    }
    pub fn module<S: Into<String>>(module: S, name: S) -> Self {
        Ident::new(Some(module), name)
    }
    pub fn single_and_eq(&self, s: &str) -> bool {
        self.module.is_none() && self.name == s
    }
}

impl TreeHash for Ident {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(&[self.module.is_some() as u8]);
        if let Some(module) = &self.module {
            sha.update(module);
        }
        sha.update(&self.name);
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(module) = &self.module {
            write!(f, "{}.{}", module, self.name)
        } else {
            self.name.fmt(f)
        }
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        ident.to_string()
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Error parsing ident")]
pub struct IdentParseError;

impl FromStr for Ident {
    type Err = IdentParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().filter(|c| c == &'.').count() > 1 {
            return Err(IdentParseError);
        }
        let mut iter = s.split('.');
        let first = iter.next().ok_or(IdentParseError)?;
        let second = iter.next();
        let ident = if let Some(second) = second {
            Ident::module(first, second)
        } else {
            Ident::no_module(first)
        };
        Ok(ident)
    }
}

impl TryFrom<String> for Ident {
    type Error = IdentParseError;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        s.parse()
    }
}

#[derive(Debug, Clone)]
pub enum UnresolvedNode {
    Ident(Ident),
    Quotation(Vec<Sp<UnresolvedNode>>),
    Literal(Literal),
    WhiteSpace(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Bool(bool),
    Nat(u64),
    Int(i64),
    Float(f64),
    Char(char),
    Text(String),
}

impl TreeHash for Literal {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Literal::Bool(b) => sha.update(&[*b as u8]),
            Literal::Nat(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Int(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Float(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Char(c) => sha.update(&unsafe { mem::transmute::<_, [u8; 4]>(*c) }),
            Literal::Text(s) => sha.update(s),
        }
    }
}

impl Literal {
    pub fn as_primitive<T>(&self) -> Primitive<T> {
        match self {
            Literal::Bool(_) => Primitive::Bool,
            Literal::Nat(_) => Primitive::Nat,
            Literal::Int(_) => Primitive::Int,
            Literal::Float(_) => Primitive::Float,
            Literal::Char(_) => Primitive::Char,
            Literal::Text(_) => Primitive::Text,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Bool(b) => b.to_string().blue().fmt(f),
            Literal::Nat(n) => n.to_string().cyan().fmt(f),
            Literal::Int(n) => n.to_string().cyan().fmt(f),
            Literal::Float(n) => n.to_string().cyan().fmt(f),
            Literal::Char(c) => c.to_string().yellow().fmt(f),
            Literal::Text(s) => s.yellow().fmt(f),
        }
    }
}
