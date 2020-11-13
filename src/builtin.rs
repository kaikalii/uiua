use std::{borrow::Cow, iter::once};

use once_cell::sync::Lazy;

use crate::types::*;

macro_rules! builtin {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum BuiltinDef {
            $(
                $(#[$doc])*
                $name,
            )*
            /// Call a quotation
            Call(u8, u8),
            /// An if statement
            If(u8, u8),
        }
        impl BuiltinDef {
            const ALL_SIMPLE: &'static [BuiltinDef] = &[$(BuiltinDef::$name),*];
        }
    };
}

static ALL_BUILTIN_DEFS: Lazy<Vec<BuiltinDef>> = Lazy::new(|| {
    BuiltinDef::ALL_SIMPLE
        .iter()
        .copied()
        .chain((0..10).flat_map(|bef| {
            (0..10).flat_map(move |aft| {
                once(BuiltinDef::Call(bef, aft)).chain(once(BuiltinDef::If(bef, aft)))
            })
        }))
        .collect()
});

builtin!(
    /// Duplicate
    Dup,
    /// Make a list
    List,
    /// Append to a list
    App,
    /// Swap the top 2 items
    Swap,
    /// Add 2 items
    Add,
    /// Remove the top item
    Pop,
);

fn generic_list() -> Type {
    Primitive::list(Type::Generic(Generic::new("T", 0))).into()
}

fn generic(name: &str, i: u8) -> Type {
    Type::Generic(Generic::new(name, i))
}

fn t() -> Type {
    generic("T", 0)
}

fn u() -> Type {
    generic("U", 1)
}

impl BuiltinDef {
    pub fn all() -> &'static Lazy<Vec<Self>> {
        &ALL_BUILTIN_DEFS
    }
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            BuiltinDef::Dup => (vec![t()], vec![t(); 2]),
            BuiltinDef::List => (vec![], vec![generic_list()]),
            BuiltinDef::App => (vec![generic_list(), t()], vec![generic_list()]),
            BuiltinDef::Swap => (vec![t(), u()], vec![u(), t()]),
            BuiltinDef::Add => (vec![t(); 2], vec![t()]),
            BuiltinDef::Pop => (vec![t()], vec![]),
            BuiltinDef::Call(before, after) => {
                let mut params = DefaultParams::default();
                let before: Vec<_> = params.by_ref().take(*before as usize).collect();
                let after: Vec<_> = params.by_ref().take(*after as usize).collect();
                let inner = Signature::new(before.clone(), after.clone());
                (
                    before
                        .into_iter()
                        .chain(once(Primitive::Quotation(inner).into()))
                        .collect(),
                    after,
                )
            }
            BuiltinDef::If(before, after) => {
                let mut params = DefaultParams::default();
                let before: Vec<_> = params.by_ref().take(*before as usize).collect();
                let after: Vec<_> = params.by_ref().take(*after as usize).collect();
                let inner = Signature::new(before.clone(), after.clone());
                (
                    before
                        .into_iter()
                        .chain(once(Primitive::Bool.into()))
                        .chain(once(Primitive::Quotation(inner.clone()).into()))
                        .chain(once(Primitive::Quotation(inner).into()))
                        .collect(),
                    after,
                )
            }
        };
        Signature::new(before, after)
    }
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            BuiltinDef::Dup => "dup",
            BuiltinDef::List => "list",
            BuiltinDef::App => "app",
            BuiltinDef::Swap => "swap",
            BuiltinDef::Add => "+",
            BuiltinDef::Pop => "pop",
            BuiltinDef::Call(before, after) => return format!("!{}--{}", before, after).into(),
            BuiltinDef::If(before, after) => return format!("?{}--{}", before, after).into(),
        }
        .into()
    }
}

#[derive(Default)]
struct DefaultParams {
    i: u8,
}

impl Iterator for DefaultParams {
    type Item = Type;
    fn next(&mut self) -> Option<Self::Item> {
        if self.i < 26 {
            self.i += 1;
            Some(Type::Generic(Generic::new(
                (b'A' + self.i - 1) as char,
                self.i - 1,
            )))
        } else {
            None
        }
    }
}
