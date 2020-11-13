use std::{fmt, mem};

use sha3::*;

use crate::{ast::Hash, span::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Generic(Generic),
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
            Type::Generic(g) => sha.update(&[g.index]),
        }
    }
    pub fn generics(&self) -> Vec<u8> {
        let mut generics = match self {
            Type::Generic(g) => vec![g.index],
            Type::Prim(prim) => match prim {
                Primitive::List(inner) => inner.generics(),
                Primitive::Op(sig) => sig
                    .before
                    .iter()
                    .flat_map(Type::generics)
                    .chain(sig.after.iter().flat_map(Type::generics))
                    .collect(),
                _ => Vec::new(),
            },
        };
        generics.sort_unstable();
        generics.dedup();
        generics
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
            Type::Generic(g) => g.name.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Generic {
    pub name: String,
    pub index: u8,
}

impl Generic {
    pub fn new<S: Into<String>>(name: S, index: u8) -> Generic {
        Generic {
            name: name.into(),
            index,
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
    fn generics(&self) -> Vec<u8> {
        let mut generics: Vec<_> = self
            .before
            .iter()
            .flat_map(Type::generics)
            .chain(self.after.iter().flat_map(Type::generics))
            .collect();
        generics.sort_unstable();
        generics.dedup();
        generics
    }
    pub fn compose(&self, b: &Self) -> Result<Self, TypeError> {
        // println!();
        let mut a = self.clone();
        let mut b = b.clone();
        // Make b's generics different than a's
        let add_to_b = a.generics().last().copied().unwrap_or(0)
            - b.generics().first().copied().unwrap_or(0)
            + 1;
        // println!("b + {}", add_to_b);
        for ty in &mut b.before {
            transform_type(ty, &|ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        for ty in &mut b.after {
            transform_type(ty, &|ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        // Loop over the reversed outputs of a zipped with the reversed inputs of b
        let mut i = 0;
        loop {
            if a.after.len() > i && b.before.len() > i {
                // println!();
                // println!("a {}", a);
                // println!("b {}", b);
                let a_len = a.after.len();
                let b_len = b.before.len();
                let a_after = &mut a.after[a_len - 1 - i];
                let b_before = &mut b.before[b_len - 1 - i];
                let mut conv = Vec::new();
                if a_after != b_before && a_after.generics() == b_before.generics() {
                    return Err(TypeError::Mismatch {
                        expected: b_before.clone(),
                        found: a_after.clone(),
                    });
                }
                trade_generics(a_after, b_before, &mut conv);
                for (g, new) in conv {
                    // println!("{} -> {}", g, new);
                    for ty in &mut a.before {
                        set_generic(ty, g, &new);
                    }
                    for ty in &mut a.after {
                        set_generic(ty, g, &new);
                    }
                    for ty in &mut b.before {
                        set_generic(ty, g, &new);
                    }
                    for ty in &mut b.after {
                        set_generic(ty, g, &new);
                    }
                }
                // println!("a {}", a);
                // println!("b {}", b);
                let a_after = &a.after[a_len - 1 - i];
                let b_before = &b.before[b_len - 1 - i];
                if a_after != b_before {
                    return Err(TypeError::Mismatch {
                        found: a_after.clone(),
                        expected: b_before.clone(),
                    });
                }
            } else {
                break;
            }
            i += 1;
        }
        // Build new states using the remamining types
        let before = b
            .before
            .iter()
            .take(b.before.len() - i)
            .chain(&a.before)
            .cloned()
            .collect();
        let after = a
            .after
            .iter()
            .take(a.after.len() - i)
            .chain(&b.after)
            .cloned()
            .collect();
        let sig = Signature::new(before, after);
        // println!("ab {}", sig);
        Ok(sig)
    }
}

fn trade_generics(a: &mut Type, b: &mut Type, conv: &mut Vec<(u8, Type)>) {
    match (a, b) {
        (Type::Generic(a), Type::Generic(b)) => conv.push((b.index, Type::Generic(a.clone()))),
        (Type::Generic(a), ref b) => conv.push((a.index, (*b).clone())),
        (ref a, Type::Generic(b)) => conv.push((b.index, (*a).clone())),
        (Type::Prim(a), Type::Prim(b)) => match (a, b) {
            (Primitive::List(a), Primitive::List(b)) => trade_generics(a, b, conv),
            (Primitive::Op(a), Primitive::Op(b)) => {
                for (a, b) in a.before.iter_mut().rev().zip(b.before.iter_mut().rev()) {
                    trade_generics(a, b, conv);
                }
                for (a, b) in a.after.iter_mut().rev().zip(b.after.iter_mut().rev()) {
                    trade_generics(a, b, conv);
                }
            }
            _ => {}
        },
    }
}

fn set_generic(ty: &mut Type, i: u8, new: &Type) {
    transform_type(ty, &|ty| {
        if let Type::Generic(g) = ty {
            if i == g.index {
                *ty = new.clone()
            }
        }
    })
}

fn transform_type<F>(ty: &mut Type, f: &F)
where
    F: Fn(&mut Type),
{
    f(ty);
    match ty {
        Type::Prim(prim) => match prim {
            Primitive::List(inner) => transform_type(inner, f),
            Primitive::Op(sig) => {
                for ty in &mut sig.before {
                    transform_type(ty, f)
                }
                for ty in &mut sig.after {
                    transform_type(ty, f)
                }
            }
            _ => {}
        },
        Type::Generic(_) => {}
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
    #[error("Expected {expected} found {found}")]
    Mismatch { expected: Type, found: Type },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(Primitive<Sp<UnresolvedType>>),
    Ident(String),
    Generic(Generic),
}
