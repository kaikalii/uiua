use std::{iter::once, mem};

use once_cell::sync::Lazy;
use sha3::*;

use crate::{ast::*, types::*};

pub static PRELUDE: &[&str] = &["base", "list", "math", "quote"];

macro_rules! builtin_words {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum BuiltinWord {
            $(
                $(#[$doc])*
                $name,
            )*
            /// Call a quotation
            Call(u8, u8),
            /// An if statement
            If(u8, u8),
        }
        impl BuiltinWord {
            const ALL_SIMPLE: &'static [BuiltinWord] = &[$(BuiltinWord::$name),*];
        }
    };
}

static ALL_BUILTIN_DEFS: Lazy<Vec<BuiltinWord>> = Lazy::new(|| {
    BuiltinWord::ALL_SIMPLE
        .iter()
        .copied()
        // .chain((0..10).flat_map(|bef| {
        //     (0..10).flat_map(move |aft| {
        //         once(BuiltinWord::Call(bef, aft)).chain(once(BuiltinWord::If(bef, aft)))
        //     })
        // }))
        .collect()
});

builtin_words!(
    /// Duplicate
    Dup,
    /// Make a list
    List,
    /// Append to a list
    App,
    /// Swap the top 2 items
    Swap,
    /// Remove the top item
    Pop,
);

fn generic_list() -> Type {
    Primitive::list(Type::Generic(Generic::new("T", 0))).into()
}

fn generic(name: &str, i: u8) -> Type {
    Type::Generic(Generic::new(name, i))
}

fn a() -> Type {
    generic("a", 0)
}

fn b() -> Type {
    generic("b", 1)
}

impl BuiltinWord {
    pub fn all() -> &'static Lazy<Vec<Self>> {
        &ALL_BUILTIN_DEFS
    }
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            BuiltinWord::Dup => (vec![a()], vec![a(); 2]),
            BuiltinWord::List => (vec![], vec![generic_list()]),
            BuiltinWord::App => (vec![generic_list(), a()], vec![generic_list()]),
            BuiltinWord::Swap => (vec![a(), b()], vec![b(), a()]),
            BuiltinWord::Pop => (vec![a()], vec![]),
            BuiltinWord::Call(before, after) => {
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
            BuiltinWord::If(before, after) => {
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
    pub fn ident(&self) -> Ident {
        match self {
            BuiltinWord::Dup => Ident::base("dup"),
            BuiltinWord::List => Ident::module("list", "list"),
            BuiltinWord::App => Ident::module("list", "|<"),
            BuiltinWord::Swap => Ident::base("swap"),
            BuiltinWord::Pop => Ident::base("pop"),
            BuiltinWord::Call(before, after) => {
                Ident::module("quote".into(), format!("!{}--{}", before, after))
            }
            BuiltinWord::If(before, after) => {
                Ident::module("quote".into(), format!("?{}--{}", before, after))
            }
        }
    }
}

impl TreeHash for BuiltinWord {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        sha.update(unsafe { mem::transmute::<_, [u8; 3]>(*self) });
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
                (b'a' + self.i - 1) as char,
                self.i - 1,
            )))
        } else {
            None
        }
    }
}
