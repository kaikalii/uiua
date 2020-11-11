use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Struct(HashMap<String, Type>),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature<T = Type> {
    pub before: Vec<T>,
    pub after: Vec<T>,
}

impl Signature {
    pub fn compose(&self, b: &Self) -> Result<Self, TypeError> {
        let a = self;
        let mut a_after_iter = a.after.iter().rev();
        let mut b_before_iter = b.before.iter().rev();
        for (a_after, b_before) in a_after_iter.by_ref().zip(b_before_iter.by_ref()) {
            if a_after != b_before {
                return Err(TypeError::Mismatch {
                    expected: b_before.clone(),
                    found: a_after.clone(),
                });
            }
        }
        let before = b_before_iter.rev().chain(&a.before).cloned().collect();
        let after = a_after_iter.rev().chain(&b.after).cloned().collect();
        Ok(Signature { before, after })
    }
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
