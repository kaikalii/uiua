use std::mem;

use serde::*;
use sha3::*;

use crate::{ast::*, types::*};

pub static PRELUDE: &[&str] = &["stack", "list", "math", "control"];

macro_rules! builtin_words {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy, Serialize, Deserialize)]
        pub enum BuiltinWord {
            $(
                $(#[$doc])*
                $name,
            )*
            Call(u8, u8)
        }
        impl BuiltinWord {
            pub const ALL_SIMPLE: &'static [BuiltinWord] = &[$(BuiltinWord::$name),*];
        }
    };
}

builtin_words!(
    /// If
    If,
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
    /// Panic with a message,
    Panic,
);

fn generic_list() -> Type {
    Primitive::list(Type::Generic(Generic::new("a", 0, true))).into()
}

fn generic(name: &str, i: u8) -> Type {
    Type::Generic(Generic::new(name, i, true))
}

fn a() -> Type {
    generic("a", 0)
}

fn b() -> Type {
    generic("b", 1)
}

impl BuiltinWord {
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            BuiltinWord::Call(before, after) => {
                let mut params = DefaultParams::default();
                let mut before: Vec<_> = params.by_ref().take(*before as usize).collect();
                let after: Vec<_> = params.by_ref().take(*after as usize).collect();
                before.push(
                    Primitive::Quotation(Signature::new(before.clone(), after.clone())).into(),
                );
                (before, after)
            }
            BuiltinWord::If => (vec![Primitive::Bool.into(), a(), a()], vec![a()]),
            BuiltinWord::Dup => (vec![a()], vec![a(); 2]),
            BuiltinWord::List => (vec![], vec![generic_list()]),
            BuiltinWord::App => (vec![generic_list(), a()], vec![generic_list()]),
            BuiltinWord::Swap => (vec![a(), b()], vec![b(), a()]),
            BuiltinWord::Pop => (vec![a()], vec![]),
            BuiltinWord::Panic => (vec![Primitive::Text.into()], vec![Primitive::Never.into()]),
        };
        Signature::new(before, after)
    }
    pub fn ident(&self) -> Ident {
        match self {
            BuiltinWord::If => Ident::module("control", "?"),
            BuiltinWord::Call(..) => Ident::module("control", "!"),
            BuiltinWord::Panic => Ident::module("control", "panic"),
            BuiltinWord::Dup => Ident::module("stack", "dup"),
            BuiltinWord::List => Ident::module("list", "list"),
            BuiltinWord::App => Ident::module("list", "|<"),
            BuiltinWord::Swap => Ident::module("stack", "swap"),
            BuiltinWord::Pop => Ident::module("stack", "pop"),
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
pub struct DefaultParams {
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
                true,
            )))
        } else {
            None
        }
    }
}
