use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    fmt, mem,
};

use sha3::*;

use crate::{ast::*, span::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Generic(Generic),
}

impl Type {
    pub fn generics(&self) -> Vec<u8> {
        let mut generics = match self {
            Type::Generic(g) => vec![g.index],
            Type::Prim(prim) => match prim {
                Primitive::List(inner) => inner.generics(),
                Primitive::Quotation(sig) => sig
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

impl TreeHash for Type {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Type::Prim(prim) => prim.hash(sha),
            Type::Generic(g) => sha.update(&[g.index]),
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
            Type::Generic(g) => g.name.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Eq)]
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

impl PartialEq for Generic {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl PartialOrd for Generic {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Generic {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive<T = Type, P = Params> {
    Unit,
    Bool,
    Nat,
    Int,
    Float,
    Char,
    Text,
    List(Box<T>),
    Quotation(Signature<T, P>),
}

pub type UnresolvedPrimitive = Primitive<Sp<UnresolvedType>, Sp<UnresolvedParams>>;

impl<T> Primitive<T> {
    pub fn list(inner: T) -> Self {
        Primitive::List(Box::new(inner))
    }
}

impl TreeHash for Primitive {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Primitive::List(inner) => inner.hash(sha),
            Primitive::Quotation(sig) => sig.hash(sha),
            _ => {}
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Unit => write!(f, "()"),
            Primitive::Bool => write!(f, "Bool"),
            Primitive::Nat => write!(f, "Nat"),
            Primitive::Int => write!(f, "Int"),
            Primitive::Float => write!(f, "Float"),
            Primitive::Char => write!(f, "Char"),
            Primitive::Text => write!(f, "Text"),
            Primitive::List(inner) => write!(f, "[{}]", inner),
            Primitive::Quotation(sig) => fmt::Display::fmt(sig, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature<T = Type, P = Params> {
    pub params: P,
    pub before: Vec<T>,
    pub after: Vec<T>,
}

pub type Params = Vec<String>;
pub type UnresolvedParams = Vec<Sp<String>>;
pub type UnresolvedSignature = Signature<Sp<UnresolvedType>, Sp<UnresolvedParams>>;

impl<T, P> Signature<T, P> {
    pub fn explicit(params: P, before: Vec<T>, after: Vec<T>) -> Self {
        Signature {
            params,
            before,
            after,
        }
    }
}

impl Signature {
    pub fn new(before: Vec<Type>, after: Vec<Type>) -> Self {
        let mut sig = Signature::explicit(Vec::new(), before, after);
        let reassign: HashMap<u8, u8> = sig
            .generics()
            .into_iter()
            .enumerate()
            .map(|(i, g)| (g, i as u8))
            .collect();
        let params = &mut sig.params;
        for ty in sig.before.iter_mut().chain(&mut sig.after) {
            transform_type(ty, &mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index = reassign[&g.index];
                    if params.len() <= g.index as usize {
                        params.resize(g.index as usize + 1, String::new());
                    }
                    params[g.index as usize] = g.name.clone();
                }
            });
        }
        sig
    }
}

impl TreeHash for Signature {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(self.before.len().to_le_bytes());
        sha.update(self.after.len().to_le_bytes());
        for ty in &self.before {
            ty.hash(sha);
        }
        for ty in &self.after {
            ty.hash(sha);
        }
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
            transform_type(ty, &mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        for ty in &mut b.after {
            transform_type(ty, &mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        let mut resolver = TypeResolver::default();
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
                if a_after != b_before && a_after.generics() == b_before.generics() {
                    return Err(TypeError::Mismatch {
                        expected: b_before.clone(),
                        found: a_after.clone(),
                    });
                }
                trade_generics(a_after, b_before, &mut resolver);
                // println!("{:?}", resolver);
                for ty in &mut a.before {
                    resolver.resolve(ty);
                }
                for ty in &mut a.after {
                    resolver.resolve(ty);
                }
                for ty in &mut b.before {
                    resolver.resolve(ty);
                }
                for ty in &mut b.after {
                    resolver.resolve(ty);
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

fn trade_generics(a: &mut Type, b: &mut Type, resolver: &mut TypeResolver) {
    match (a, b) {
        (Type::Generic(a), Type::Generic(b)) => {
            resolver.insert(b.clone(), &Type::Generic(a.clone()));
            resolver.insert(a.clone(), &Type::Generic(b.clone()));
        }
        (Type::Generic(a), ref b) => {
            resolver.insert(a.clone(), b);
        }
        (ref a, Type::Generic(b)) => {
            resolver.insert(b.clone(), a);
        }
        (Type::Prim(a), Type::Prim(b)) => match (a, b) {
            (Primitive::List(a), Primitive::List(b)) => trade_generics(a, b, resolver),
            (Primitive::Quotation(a), Primitive::Quotation(b)) => {
                for (a, b) in a.before.iter_mut().rev().zip(b.before.iter_mut().rev()) {
                    trade_generics(a, b, resolver);
                }
                for (a, b) in a.after.iter_mut().rev().zip(b.after.iter_mut().rev()) {
                    trade_generics(a, b, resolver);
                }
            }
            _ => {}
        },
    }
}

#[derive(Debug, Default)]
struct TypeResolver(Vec<(BTreeSet<Generic>, Option<Type>)>);

impl TypeResolver {
    fn insert(&mut self, a: Generic, ty: &Type) {
        if let Type::Generic(b) = ty {
            let a_buc = self.0.iter().position(|(buc, _)| buc.contains(&a));
            let b_buc = self.0.iter().position(|(buc, _)| buc.contains(&b));
            if let Some((a, b)) = a_buc.zip(b_buc) {
                if a != b {
                    let combined = a.min(b);
                    let mut removed = self.0.remove(a.max(b));
                    self.0[combined].0.append(&mut removed.0);
                    if self.0[combined].1.is_none() {
                        self.0[combined].1 = removed.1;
                    }
                }
            } else if let Some(a) = a_buc {
                self.0[a].0.insert(b.clone());
            } else if let Some(b) = b_buc {
                self.0[b].0.insert(a);
            } else {
                let mut bucket = BTreeSet::new();
                bucket.insert(a);
                bucket.insert(b.clone());
                self.0.push((bucket, None));
            }
        } else if let Some((bucket, concrete @ None)) =
            self.0.iter_mut().find(|(buc, _)| buc.contains(&a))
        {
            bucket.insert(a);
            *concrete = Some(ty.clone());
        } else {
            let mut bucket = BTreeSet::new();
            bucket.insert(a);
            self.0.push((bucket, Some(ty.clone())));
        }
    }
    fn get(&self, g: &Generic) -> Option<Type> {
        self.0
            .iter()
            .find(|(buc, _)| buc.contains(g))
            .map(|(buc, ty)| {
                if let Some(ty) = ty {
                    ty.clone()
                } else {
                    Type::Generic(buc.iter().max().unwrap().clone())
                }
            })
    }
    fn resolve(&mut self, ty: &mut Type) {
        transform_type(ty, &mut |ty| {
            if let Type::Generic(g) = ty {
                if let Some(new) = self.get(g) {
                    *ty = new;
                }
            }
        })
    }
}

fn set_generic(ty: &mut Type, i: u8, new: &Type) {
    transform_type(ty, &mut |ty| {
        if let Type::Generic(g) = ty {
            if i == g.index {
                *ty = new.clone()
            }
        }
    })
}

fn transform_type<F>(ty: &mut Type, f: &mut F)
where
    F: FnMut(&mut Type),
{
    f(ty);
    match ty {
        Type::Prim(prim) => match prim {
            Primitive::List(inner) => transform_type(inner, f),
            Primitive::Quotation(sig) => {
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

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for param in &self.params {
            write!(f, "{} ", param)?;
        }
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
    let a = Signature::new(vec![Nat.into(); 2], vec![Nat.into()]);
    let b = Signature::new(
        vec![Primitive::list(Nat.into()).into(), Nat.into()],
        vec![Primitive::list(Nat.into()).into()],
    );
    let c = Signature::new(
        vec![Primitive::list(Nat.into()).into(), Nat.into(), Nat.into()],
        vec![Primitive::list(Nat.into()).into()],
    );
    assert_eq!(a.compose(&b).unwrap(), c);
    let a = Signature::new(vec![Nat.into()], vec![Float.into()]);
    let b = Signature::new(vec![Nat.into()], vec![Bool.into()]);
    assert!(a.compose(&b).is_err());
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Expected {expected} found {found}")]
    Mismatch { expected: Type, found: Type },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(UnresolvedPrimitive),
    Ident(Ident),
}
