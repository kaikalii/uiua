use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Prim(Primitive),
    Struct(HashMap<String, Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Bool,
    Nat,
    Int,
    Float,
    String,
    List(Box<Type>),
    Op(Operation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operation {
    pub before: Vec<Type>,
    pub after: Vec<Type>,
}

impl Operation {
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
        Ok(Operation { before, after })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Expected {expected:?} found {found:?}")]
    Mismatch { expected: Type, found: Type },
}
