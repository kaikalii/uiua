use std::{fmt, mem};

use sha3::*;

use crate::{builtin::BuiltinDef, span::*, types::*};

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

#[derive(Debug, Clone)]
pub struct Def {
    pub sig: Signature,
    pub kind: DefKind,
}

impl Def {
    pub fn hash_finish(&self) -> Hash {
        let mut sha = Sha3_256::default();
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(&self.kind)) });
        match &self.kind {
            DefKind::Uiua(nodes) => {
                for node in nodes {
                    node.hash(&mut sha);
                }
            }
            DefKind::Builtin(bi) => {
                sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(bi)) });
                sha.update(unsafe { mem::transmute::<_, [u8; 3]>(*bi) });
            }
        }
        Hash(sha.finalize())
    }
}

impl From<BuiltinDef> for Def {
    fn from(builtin: BuiltinDef) -> Self {
        Def {
            sig: builtin.sig(),
            kind: DefKind::Builtin(builtin),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Uiua(Vec<Node>),
    Builtin(BuiltinDef),
}

#[derive(Debug, Clone)]
pub enum Node {
    Ident(Hash),
    SelfIdent,
    Defered(Vec<Sp<Node>>),
    Literal(Literal),
}

impl Node {
    pub fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Node::Ident(hash) => sha.update(hash),
            Node::Defered(nodes) => {
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
pub struct UnresolvedDef {
    pub name: Sp<String>,
    pub sig: Option<UnresolvedSignature>,
    pub nodes: Vec<Sp<UnresolvedNode>>,
}

#[derive(Debug, Clone)]
pub enum UnresolvedNode {
    Ident(String),
    Defered(Vec<Sp<UnresolvedNode>>),
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

impl Literal {
    pub fn hash(&self, sha: &mut Sha3_256) {
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
