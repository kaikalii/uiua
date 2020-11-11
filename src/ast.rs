use std::mem;

use sha3::*;

pub type Hash =
    digest::generic_array::GenericArray<u8, digest::generic_array::typenum::consts::U32>;

#[derive(Debug, Clone)]
pub enum Node {
    Ident(Hash),
}

impl Node {
    pub fn hash(&self) -> Hash {
        let mut sha = Sha3_256::default();
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Node::Ident(hash) => sha.update(hash),
        }
        sha.finalize()
    }
}
