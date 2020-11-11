use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Struct(HashMap<String, Type>),
}

impl From<Primitive> for Type {
    fn from(prim: Primitive) -> Self {
        Type::Prim(prim)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive<T = Type> {
    Bool,
    Nat,
    Int,
    Float,
    String,
    List(Box<T>),
    Op(Signature<T>),
}

impl<T> Primitive<T> {
    pub fn list(inner: T) -> Self {
        Primitive::List(Box::new(inner))
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
    #[error("Expected {expected:?} found {found:?}")]
    Mismatch { expected: Type, found: Type },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    Prim(Primitive<Self>),
    Other(String),
}
