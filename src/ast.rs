use std::{fmt, mem};

use sha3::*;

use crate::{builtin::BuiltinWord, span::*, types::*};

type HashInner =
    digest::generic_array::GenericArray<u8, digest::generic_array::typenum::consts::U32>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Hash(pub HashInner);

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

pub trait TreeHash {
    fn hash(&self, sha: &mut Sha3_256);
    fn hash_finish(&self) -> Hash {
        let mut sha = Sha3_256::default();
        self.hash(&mut sha);
        Hash(sha.finalize())
    }
}

#[derive(Debug, Clone)]
pub struct Word {
    pub sig: Signature,
    pub kind: WordKind,
}

impl TreeHash for Word {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(&self.kind)) });
        match &self.kind {
            WordKind::Uiua(nodes) => {
                for node in nodes {
                    node.hash(sha);
                }
            }
            WordKind::Builtin(bi) => {
                sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(bi)) });
                sha.update(unsafe { mem::transmute::<_, [u8; 3]>(*bi) });
            }
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

#[derive(Debug, Clone)]
pub enum WordKind {
    Uiua(Vec<Node>),
    Builtin(BuiltinWord),
}

#[derive(Debug, Clone)]
pub enum Node {
    Ident(Hash),
    SelfIdent,
    Quotation(Vec<Sp<Node>>),
    Literal(Literal),
}

impl TreeHash for Node {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Node::Ident(hash) => sha.update(hash),
            Node::Quotation(nodes) => {
                for node in nodes {
                    node.hash(sha);
                }
            }
            Node::SelfIdent => {}
            Node::Literal(lit) => lit.hash(sha),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedWord {
    pub name: Sp<Ident>,
    pub sig: Option<UnresolvedSignature>,
    pub nodes: Vec<Sp<UnresolvedNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    pub module: Option<String>,
    pub name: String,
}

impl Ident {
    pub fn new(module: Option<String>, name: String) -> Self {
        Ident { module, name }
    }
    pub fn base(name: String) -> Self {
        Ident::new(Some("base".into()), name)
    }
    pub fn single_and_eq(&self, s: &str) -> bool {
        self.module.is_none() && self.name == s
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

#[derive(Debug, Clone)]
pub enum UnresolvedNode {
    Ident(Ident),
    Quotation(Vec<Sp<UnresolvedNode>>),
    Literal(Literal),
}

#[derive(Debug, Clone)]
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
