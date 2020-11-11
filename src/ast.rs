use std::mem;

use sha3::*;

use crate::types::Type;

pub type Hash =
    digest::generic_array::GenericArray<u8, digest::generic_array::typenum::consts::U32>;

#[derive(Debug, Clone)]
pub struct Def {
    pub ty: Type,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {
    Ident(Hash),
    SelfIdent,
    Defered(Vec<Node>),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone)]
pub enum UnresolvedNode {
    Ident(String),
    Defered(Vec<UnresolvedNode>),
    Literal(Literal),
}

impl UnresolvedNode {
    pub fn hash(&self) -> Hash {
        let mut sha = Sha3_256::default();
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        self.hash_recursive(&mut sha);
        sha.finalize()
    }
    fn hash_recursive(&self, sha: &mut Sha3_256) {
        match self {
            UnresolvedNode::Ident(hash) => sha.update(hash),
            UnresolvedNode::Defered(nodes) => {
                for node in nodes {
                    node.hash_recursive(sha);
                }
            }
            UnresolvedNode::Literal(lit) => lit.hash(sha),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(bool),
    NatOrInt(u64),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
}

impl Literal {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Literal::Bool(b) => sha.update(&[*b as u8]),
            Literal::NatOrInt(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Int(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Float(n) => sha.update(&unsafe { mem::transmute::<_, [u8; 8]>(*n) }),
            Literal::Char(c) => sha.update(&unsafe { mem::transmute::<_, [u8; 4]>(*c) }),
            Literal::String(s) => sha.update(s),
        }
    }
}
