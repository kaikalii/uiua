use std::{fmt, mem};

use sha3::*;

use crate::ast::Hash;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Generic(u8),
}

impl Type {
    pub fn hash_finish(&self) -> Hash {
        let mut sha = Sha3_256::default();
        self.hash(&mut sha);
        Hash(sha.finalize())
    }
    pub fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Type::Prim(prim) => prim.hash(sha),
            Type::Generic(i) => sha.update(&[*i]),
        }
    }
}

impl From<Primitive> for Type {
    fn from(prim: Primitive) -> Self {
        Type::Prim(prim)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Prim(prim) => prim.fmt(f),
            Type::Generic(i) => write!(f, "{{{}}}", i),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive<T = Type> {
    Bool,
    Nat,
    Int,
    Float,
    Char,
    Text,
    List(Box<T>),
    Op(Signature<T>),
}

impl<T> Primitive<T> {
    pub fn list(inner: T) -> Self {
        Primitive::List(Box::new(inner))
    }
}

impl Primitive {
    pub fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Primitive::List(inner) => inner.hash(sha),
            Primitive::Op(sig) => {
                sha.update(sig.before.len().to_le_bytes());
                sha.update(sig.after.len().to_le_bytes());
                for ty in &sig.before {
                    ty.hash(sha);
                }
                for ty in &sig.after {
                    ty.hash(sha);
                }
            }
            _ => {}
        }
    }
}

impl<T> fmt::Display for Primitive<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Bool => write!(f, "Bool"),
            Primitive::Nat => write!(f, "Nat"),
            Primitive::Int => write!(f, "Int"),
            Primitive::Float => write!(f, "Float"),
            Primitive::Char => write!(f, "Char"),
            Primitive::Text => write!(f, "Text"),
            Primitive::List(inner) => write!(f, "[{}]", inner),
            Primitive::Op(sig) => fmt::Display::fmt(sig, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature<T = Type> {
    pub before: Vec<T>,
    pub after: Vec<T>,
}

impl<T> Signature<T> {
    pub fn new(before: Vec<T>, after: Vec<T>) -> Self {
        Signature { before, after }
    }
}

impl Signature {
    pub fn compose(&self, b: &Self) -> Result<Self, TypeError> {
        let a = self;
        let mut i = 0;
        loop {
            if a.after.len() > i && b.before.len() > i {
                let a_after = &a.after[a.after.len() - 1 - i];
                let b_before = &b.before[b.before.len() - 1 - i];
                if a_after != b_before {
                    return Err(TypeError::Mismatch {
                        expected: b_before.clone(),
                        found: a_after.clone(),
                    });
                }
            } else {
                break;
            }
            i += 1;
        }
        let before = b
            .before
            .iter()
            .rev()
            .skip(i)
            .chain(&a.before)
            .cloned()
            .collect();
        let after = a
            .after
            .iter()
            .rev()
            .skip(i)
            .chain(&b.after)
            .cloned()
            .collect();
        Ok(Signature::new(before, after))
    }
}

impl<T> fmt::Display for Signature<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "( ")?;
        for ty in &self.before {
            write!(f, "{} ", ty)?;
        }
        write!(f, "-- ")?;
        for ty in &self.after {
            write!(f, "{} ", ty)?;
        }
        write!(f, ")")
    }
}

#[cfg(test)]
#[test]
fn sig_compose() {
    use Primitive::*;
    let a = Signature::new(vec![], vec![Bool.into()]);
    let b = Signature::new(vec![], vec![Nat.into()]);
    let c = Signature::new(vec![], vec![Bool.into(), Nat.into()]);
    assert_eq!(a.compose(&b).unwrap(), c);
    let a = Signature::new(vec![Nat.into()], vec![Float.into()]);
    let b = Signature::new(vec![Float.into()], vec![Bool.into()]);
    let c = Signature::new(vec![Nat.into()], vec![Bool.into()]);
    assert_eq!(a.compose(&b).unwrap(), c);
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Expected {expected:?} found {found:?}")]
    Mismatch { expected: Type, found: Type },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(Primitive<Self>),
    Other(String),
}
