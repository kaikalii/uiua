use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    fmt, mem,
};

use colored::*;
use itertools::*;
use serde::*;
use sha3::*;

use crate::{ast::*, resolve::ResolutionError, span::*};

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Type {
    Prim(Primitive, Option<AliasName>),
    Generic(Generic),
}

impl Type {
    pub fn generics(&self) -> Vec<u8> {
        let mut generics = match self {
            Type::Generic(g) => vec![g.index],
            Type::Prim(prim, _) => match prim {
                Primitive::List(inner) => inner.generics(),
                Primitive::Quotation(sig) => sig
                    .before
                    .iter()
                    .flat_map(Type::generics)
                    .chain(sig.after.iter().flat_map(Type::generics))
                    .collect(),
                Primitive::Tuple(types) => types.iter().flat_map(Type::generics).collect(),
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
            Type::Prim(prim, _) => match prim {
                Primitive::List(inner) => inner.visit(f),
                Primitive::Quotation(sig) => {
                    for ty in &sig.before {
                        ty.visit(f)
                    }
                    for ty in &sig.after {
                        ty.visit(f)
                    }
                }
                Primitive::Tuple(types) => {
                    for ty in types {
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
            Type::Prim(prim, _) => match prim {
                Primitive::List(inner) => inner.mutate(f),
                Primitive::Quotation(sig) => {
                    for ty in &mut sig.before {
                        ty.mutate(f)
                    }
                    for ty in &mut sig.after {
                        ty.mutate(f)
                    }
                }
                Primitive::Tuple(types) => {
                    for ty in types {
                        ty.mutate(f)
                    }
                }
                _ => {}
            },
            Type::Generic(_) => {}
        }
    }
    pub fn is_subset_of(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Type::Generic(_)) => true,
            (Type::Generic(_), _) => false,
            (_, Type::Prim(Primitive::Never, _)) => true,
            (Type::Prim(Primitive::List(a), _), Type::Prim(Primitive::List(b), _)) => {
                a.is_subset_of(b)
            }
            (Type::Prim(Primitive::Quotation(a), _), Type::Prim(Primitive::Quotation(b), _)) => {
                a.is_subset_of(b)
            }
            (Type::Prim(Primitive::Tuple(a), _), Type::Prim(Primitive::Tuple(b), _)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.is_subset_of(b))
            }
            (Type::Prim(a, _), Type::Prim(b, _)) => a == b,
        }
    }
}

impl TreeHash for Type {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            Type::Prim(prim, alias_name) => {
                prim.hash(sha);
                if let Some(an) = alias_name {
                    if an.unique {
                        sha.update(&an.name);
                    }
                }
            }
            Type::Generic(g) => sha.update(&[g.index]),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Prim(a, a_name), Type::Prim(b, b_name)) => a == b && a_name == b_name,
            (Type::Generic(a), Type::Generic(b)) => a == b,
            _ => false,
        }
    }
}

impl From<Primitive> for Type {
    fn from(prim: Primitive) -> Self {
        Type::Prim(prim, None)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Prim(prim, an) => {
                if let Some(an) = an {
                    an.name.fmt(f)
                } else {
                    prim.fmt(f)
                }
            }
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
pub struct AliasName {
    pub name: String,
    pub unique: bool,
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub enum Primitive<T = Type> {
    Never,
    Unit,
    Bool,
    Nat,
    Int,
    Float,
    Char,
    Text,
    List(Box<T>),
    Quotation(Signature<T>),
    Tuple(Vec<T>),
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
            Primitive::Tuple(types) => {
                for ty in types {
                    ty.hash(sha);
                }
            }
            _ => {}
        }
    }
}

impl<T> PartialEq for Primitive<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Primitive::Never, _) | (_, Primitive::Never) => true,
            (Primitive::List(a), Primitive::List(b)) => a == b,
            (Primitive::Quotation(a), Primitive::Quotation(b)) => a == b,
            (Primitive::Tuple(a), Primitive::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a == b)
            }
            (a, b) => mem::discriminant(a) == mem::discriminant(b),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Never => "!".fmt(f),
            Primitive::Unit => "()".fmt(f),
            Primitive::Bool => "Bool".fmt(f),
            Primitive::Nat => "Nat".fmt(f),
            Primitive::Int => "Int".fmt(f),
            Primitive::Float => "Float".fmt(f),
            Primitive::Char => "Char".fmt(f),
            Primitive::Text => "Text".fmt(f),
            Primitive::List(inner) => write!(f, "[{}]", inner),
            Primitive::Quotation(sig) => fmt::Display::fmt(sig, f),
            Primitive::Tuple(types) => write!(
                f,
                "({})",
                types
                    .iter()
                    .map(ToString::to_string)
                    .intersperse(" ".into())
                    .collect::<String>()
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedTypeAlias {
    pub name: Sp<String>,
    pub unique: bool,
    pub kind: Sp<UnresolvedTypeAliasKind>,
}

#[derive(Debug, Clone)]
pub enum UnresolvedTypeAliasKind {
    Enum(Vec<Sp<String>>),
    Record {
        params: Sp<UnresolvedParams>,
        fields: Vec<Sp<UnresolvedField>>,
    },
}

#[derive(Debug, Clone)]
pub struct UnresolvedField {
    pub name: Sp<String>,
    pub ty: Sp<UnresolvedType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(UnresolvedPrimitive),
    Ident(Ident),
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
        let reassign: HashMap<u8, (u8, String)> = sig
            .generics()
            .into_iter()
            .zip('a'..)
            .enumerate()
            .map(|(i, (g, c))| (g, (i as u8, String::from(c))))
            .collect();
        for ty in sig.before.iter_mut().chain(&mut sig.after) {
            ty.mutate(&mut |ty| {
                if let Type::Generic(g) = ty {
                    let (index, name) = &reassign[&g.index];
                    g.index = *index;
                    if g.name.len() == 1 && g.name == g.name.to_lowercase() {
                        g.name = name.clone();
                    }
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
    /// Get a version of this signature with incremented generic indices
    /// that make it distinct from another signature
    pub fn exclusive_params(&self, other: &Self) -> Self {
        let a_max = self.generics().last().copied().unwrap_or(0);
        let b_min = other.generics().first().copied().unwrap_or(0);
        let add_to_b = if a_max >= b_min {
            a_max - b_min + 1
        } else {
            return other.clone();
        };
        let mut res = other.clone();
        for ty in res.before.iter_mut().chain(&mut res.after) {
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
    /// Check if two signatures are capable of describing the same thing
    pub fn is_joint_with(&self, other: &Self) -> bool {
        self.is_subset_of(other) || other.is_subset_of(self)
    }
    /// Check if this signature is a subset of some other signature
    pub fn is_subset_of(&self, other: &Self) -> bool {
        // println!("{} is subset of {}?", self, other);
        if self.before.len() != other.before.len() || self.after.len() != other.after.len() {
            let a = self.minimum_equivalent();
            let b = other.minimum_equivalent();
            return if &a == self && &b == other {
                false
            } else {
                a.is_subset_of(&b)
            };
        }
        let a = self;
        let b = self.exclusive_params(other);
        for (a, b) in &[(&a.before, b.before), (&a.after, b.after)] {
            for (a, b) in a.iter().zip(b) {
                // println!("{} vs {}", a, b);
                match (a, b) {
                    (_, Type::Generic(_)) => {}
                    (Type::Generic(_), _) => return false,
                    (a, b) => {
                        if !a.is_subset_of(b) {
                            return false;
                        }
                    }
                }
            }
        }
        // println!("is subset");
        true
    }
    pub fn imagine_input_sig(&self) -> Self {
        let after = self.before.clone();
        Signature::new(Vec::new(), after)
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
            (Type::Prim(a, _), Type::Prim(b, _)) => match (a, b) {
                (Primitive::List(a), Primitive::List(b)) => self.align(a, b),
                (Primitive::Quotation(a), Primitive::Quotation(b)) => {
                    for (a, b) in a.before.iter().rev().zip(b.before.iter().rev()) {
                        self.align(a, b);
                    }
                    for (a, b) in a.after.iter().rev().zip(b.after.iter().rev()) {
                        self.align(a, b);
                    }
                }
                (Primitive::Tuple(a), Primitive::Tuple(b)) => {
                    for (a, b) in a.iter().zip(b) {
                        self.align(a, b)
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

static SIG_COLORS: &[Color] = &[
    Color::Cyan,
    Color::Green,
    Color::Yellow,
    Color::Red,
    Color::Magenta,
    Color::Red,
];

fn format_ty(ty: &Type, s: &mut String, i: usize) {
    match ty {
        Type::Prim(Primitive::Quotation(sig), _) => sig.format(s, i),
        _ => s.push_str(&ty.to_string()),
    }
}

impl Signature {
    fn format(&self, s: &mut String, i: usize) {
        let color = SIG_COLORS[i % SIG_COLORS.len()];
        s.push_str(&"[ ".color(color).to_string());
        for ty in &self.before {
            format_ty(ty, s, i + 1);
            s.push(' ');
        }
        s.push_str(&"-- ".color(color).to_string());
        for ty in &self.after {
            format_ty(ty, s, i + 1);
            s.push(' ');
        }
        s.push_str(&"]".color(color).to_string())
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for param in &self.params() {
            write!(f, "{} ", param.name)?;
        }
        let mut s = String::new();
        self.format(&mut s, 0);
        write!(f, "{}", s)
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
