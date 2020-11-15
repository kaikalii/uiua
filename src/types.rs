use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    fmt, mem,
};

use colored::*;
use serde::*;
use sha3::*;

use crate::{ast::*, resolve::ResolutionError, span::*};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
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
    fn merge_in(&mut self, new: Type) {
        match (self, &new) {
            (Type::Generic(a), Type::Generic(b)) => {
                a.index = b.index;
                if !b.infered {
                    a.infered = false;
                    a.name = b.name.clone();
                }
            }
            (a, _) => *a = new,
        }
    }
    fn visit<F>(&self, f: &mut F)
    where
        F: FnMut(&Type),
    {
        f(self);
        match self {
            Type::Prim(prim) => match prim {
                Primitive::List(inner) => inner.visit(f),
                Primitive::Quotation(sig) => {
                    for ty in &sig.before {
                        ty.visit(f)
                    }
                    for ty in &sig.after {
                        ty.visit(f)
                    }
                }
                _ => {}
            },
            Type::Generic(_) => {}
        }
    }
    fn mutate<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Type),
    {
        f(self);
        match self {
            Type::Prim(prim) => match prim {
                Primitive::List(inner) => inner.mutate(f),
                Primitive::Quotation(sig) => {
                    for ty in &mut sig.before {
                        ty.mutate(f)
                    }
                    for ty in &mut sig.after {
                        ty.mutate(f)
                    }
                }
                _ => {}
            },
            Type::Generic(_) => {}
        }
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

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct Generic {
    pub name: String,
    pub index: u8,
    pub infered: bool,
}

impl Generic {
    pub fn new<S: Into<String>>(name: S, index: u8, infered: bool) -> Generic {
        Generic {
            name: name.into(),
            index,
            infered,
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Primitive<T = Type> {
    Unit,
    Bool,
    Nat,
    Int,
    Float,
    Char,
    Text,
    List(Box<T>),
    Quotation(Signature<T>),
}

pub type UnresolvedPrimitive = Primitive<Sp<UnresolvedType>>;

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Signature<T = Type> {
    pub before: Vec<T>,
    pub after: Vec<T>,
}

pub type UnresolvedParams = Vec<Sp<String>>;
pub type UnresolvedSignature = Signature<Sp<UnresolvedType>>;

impl UnresolvedSignature {
    pub fn new_unresolved(before: Vec<Sp<UnresolvedType>>, after: Vec<Sp<UnresolvedType>>) -> Self {
        Signature { before, after }
    }
}

impl Signature {
    pub fn new(before: Vec<Type>, after: Vec<Type>) -> Self {
        let mut sig = Signature { before, after };
        let reassign: HashMap<u8, u8> = sig
            .generics()
            .into_iter()
            .enumerate()
            .map(|(i, g)| (g, i as u8))
            .collect();
        for ty in sig.before.iter_mut().chain(&mut sig.after) {
            ty.mutate(&mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index = reassign[&g.index];
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
    pub fn params(&self) -> Vec<Generic> {
        let mut params: Vec<Generic> = Vec::new();
        for &state in &[&self.before, &self.after] {
            for ty in state {
                ty.visit(&mut |ty| {
                    if let Type::Generic(g) = ty {
                        if !params.iter().any(|p| p.index == g.index) {
                            params.push(g.clone());
                        }
                    }
                });
            }
        }
        params
    }
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
    pub fn exclusive_params(&self, other: &Self) -> Self {
        let add_to_b = self.generics().last().copied().unwrap_or(0)
            - other.generics().first().copied().unwrap_or(0)
            + 1;
        let mut res = other.clone();
        for ty in &mut res.before {
            ty.mutate(&mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        for ty in &mut res.after {
            ty.mutate(&mut |ty| {
                if let Type::Generic(g) = ty {
                    g.index += add_to_b;
                }
            });
        }
        res
    }
    pub fn compose(&self, other: &Self) -> Result<Self, SignatureError> {
        // println!();
        let mut a = self.clone();
        let mut b = self.exclusive_params(other);
        // println!("!a: {}", a);
        // println!("!b: {}", b);
        let mut resolver = TypeResolver::default();
        // Loop over the reversed outputs of a zipped with the reversed inputs of b
        let mut i = 0;
        loop {
            if a.after.len() > i && b.before.len() > i {
                // println!();
                // println!("a: {}", a);
                // println!("b: {}", b);
                let a_len = a.after.len();
                let b_len = b.before.len();
                let a_after = &a.after[a_len - 1 - i];
                let b_before = &b.before[b_len - 1 - i];
                if a_after != b_before && a_after.generics() == b_before.generics() {
                    return Err(SignatureError {
                        input: a.clone(),
                        output: b.clone(),
                    });
                }
                resolver.align(a_after, b_before);
                // println!("{:?}", resolver);
                resolver.resolve_sig(&mut a);
                resolver.resolve_sig(&mut b);
                // println!("a: {}", a);
                // println!("b: {}", b);
                let a_after = &a.after[a_len - 1 - i];
                let b_before = &b.before[b_len - 1 - i];
                if a_after != b_before {
                    return Err(SignatureError {
                        input: a.clone(),
                        output: b.clone(),
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
        // println!("ab: {}", sig);
        Ok(sig)
    }
    fn minimum_equivalent(&self) -> Self {
        let mut me = self.clone();
        while !me.before.is_empty() && !me.after.is_empty() && me.before.first() == me.after.first()
        {
            me.before.remove(0);
            me.after.remove(0);
        }
        me
    }
    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        if self.before.len() != other.before.len() || self.after.len() != self.after.len() {
            let a = self.minimum_equivalent();
            let b = other.minimum_equivalent();
            return if &a == self && &b == other {
                false
            } else {
                a.is_equivalent_to(&b)
            };
        }
        let mut a = self.clone();
        let mut b = self.exclusive_params(other);
        let mut resolver = TypeResolver::default();
        for (a, b) in a.before.iter().zip(&b.before) {
            resolver.align(a, b);
        }
        for (a, b) in a.after.iter().zip(&b.after) {
            resolver.align(a, b);
        }
        resolver.resolve_sig(&mut a);
        resolver.resolve_sig(&mut b);
        a.before == b.before && a.after == b.after
    }
    pub fn imagine_input_sig(&self) -> Self {
        Signature::new(Vec::new(), self.before.clone())
    }
}

#[derive(Debug, Default)]
struct TypeResolver(Vec<(BTreeSet<Generic>, Option<Type>)>);

impl TypeResolver {
    fn resolve_sig(&self, sig: &mut Signature) {
        for ty in &mut sig.before {
            self.resolve_ty(ty);
        }
        for ty in &mut sig.after {
            self.resolve_ty(ty);
        }
    }
    fn align(&mut self, a: &Type, b: &Type) {
        match (a, b) {
            (Type::Generic(a), Type::Generic(b)) => {
                self.insert(b.clone(), &Type::Generic(a.clone()));
                self.insert(a.clone(), &Type::Generic(b.clone()));
            }
            (Type::Generic(a), ref b) => {
                self.insert(a.clone(), b);
            }
            (ref a, Type::Generic(b)) => {
                self.insert(b.clone(), a);
            }
            (Type::Prim(a), Type::Prim(b)) => match (a, b) {
                (Primitive::List(a), Primitive::List(b)) => self.align(a, b),
                (Primitive::Quotation(a), Primitive::Quotation(b)) => {
                    for (a, b) in a.before.iter().rev().zip(b.before.iter().rev()) {
                        self.align(a, b);
                    }
                    for (a, b) in a.after.iter().rev().zip(b.after.iter().rev()) {
                        self.align(a, b);
                    }
                }
                _ => {}
            },
        }
    }
    fn insert(&mut self, a: Generic, ty: &Type) {
        if let Type::Generic(b) = ty {
            // The inserted type is generic
            let a_buc = self.0.iter().position(|(buc, _)| buc.contains(&a));
            let b_buc = self.0.iter().position(|(buc, _)| buc.contains(&b));
            if let Some((a, b)) = a_buc.zip(b_buc) {
                // Both generics already have buckets
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
                // Neighther generic has a bucket
                let mut bucket = BTreeSet::new();
                bucket.insert(a);
                bucket.insert(b.clone());
                self.0.push((bucket, None));
            }
        } else if let Some((bucket, concrete @ None)) =
            self.0.iter_mut().find(|(buc, _)| buc.contains(&a))
        {
            // If the bucket already exists, set the type
            bucket.insert(a);
            *concrete = Some(ty.clone());
        } else {
            // If the bucket does not exist, ecrate it
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
                    let max = buc.iter().max().unwrap();
                    let index = max.index;
                    let (infered, name) = if let Some(g) = buc.iter().find(|g| !g.infered) {
                        (false, g.name.clone())
                    } else {
                        (true, max.name.clone())
                    };
                    let selected_generic = Generic::new(name, index, infered);
                    Type::Generic(selected_generic)
                }
            })
    }
    fn resolve_ty(&self, ty: &mut Type) {
        ty.mutate(&mut |ty| {
            if let Type::Generic(g) = ty {
                if let Some(new) = self.get(g) {
                    ty.merge_in(new);
                }
            }
        })
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for param in &self.params() {
            write!(f, "{} ", param.name)?;
        }
        write!(f, "{}", "( ".bright_black())?;
        for ty in &self.before {
            write!(f, "{} ", ty)?;
        }
        write!(f, "{}", "-- ".bright_black())?;
        for ty in &self.after {
            write!(f, "{} ", ty)?;
        }
        write!(f, "{}", ")".bright_black())
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

#[derive(Debug)]
pub struct SignatureError {
    input: Signature,
    output: Signature,
}

impl SignatureError {
    pub fn name(self, name: String) -> ResolutionError {
        ResolutionError::TypeMismatch {
            ident: Ident::no_module(name),
            input: self.input,
            output: self.output,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(UnresolvedPrimitive),
    Ident(Ident),
}
