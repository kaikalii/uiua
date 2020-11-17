use std::mem;

use serde::*;
use sha3::*;

use crate::{ast::*, types::*};

pub static PRELUDE: &[&str] = &["stack", "list", "tuple", "control"];

macro_rules! builtin_words {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy, Serialize, Deserialize)]
        pub enum BuiltinWord {
            $(
                $(#[$doc])*
                $name,
            )*
            Call(u8, u8),
            TupleCompose(u8),
            TupleDecompose(u8),
            TupleGet(u8, u8),
            TupleSet(u8, u8),
        }
        impl BuiltinWord {
            pub const ALL_SIMPLE: &'static [BuiltinWord] = &[$(BuiltinWord::$name),*];
            pub fn doc(&self) -> String {
                match self {
                    $(
                        BuiltinWord::$name => {
                            let s = stringify!($($doc)*);
                            format!("{}\n", s[9..(s.len() - 1)].trim())
                        },
                    )*
                    BuiltinWord::Call(..) => "Invoke a quotation\n".into(),
                    BuiltinWord::TupleCompose(size) => format!("Pop the top {} items and push a {}", size, tuple(*size)),
                    BuiltinWord::TupleDecompose(size) => format!("Pop a {} and push the {} items in order", tuple(*size), size),
                    BuiltinWord::TupleGet(_, n) => format!("Get the {} item of a tuple", ordinal(*n)),
                    BuiltinWord::TupleSet(_, n) => format!("Set the {} item of a tuple", ordinal(*n)),
                }
            }
        }
    };
}

builtin_words!(
    /**
    Use a condition to select one of two values.
    If the condition is true, the lower item is selected.
    */
    If,
    /// Duplicate the top item
    Dup,
    /// Push a new list
    List,
    /// Append an item to the list below it
    ListPushBack,
    /// Remove an item from the back of a list
    ListPopBack,
    /// Prepend an item to the list below it
    ListPushFront,
    /// Remove an item from the front of a list
    ListPopFront,
    /// Swap the top 2 items
    Swap,
    /// Remove the top item
    Pop,
    /// Pop a message and panic with it
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
    pub fn all_complex() -> impl Iterator<Item = Self> {
        let call = (0..5)
            .map(|i| (0..5).map(move |j| BuiltinWord::Call(i, j)))
            .flatten();
        let tuple_compose = (2..=8).map(BuiltinWord::TupleCompose);
        let tuple_decompose = (2..=8).map(BuiltinWord::TupleDecompose);
        let tuple_get = (2..=8)
            .map(|size| (0..2).map(move |n| BuiltinWord::TupleGet(size, n)))
            .flatten();
        let tuple_set = (2..=8)
            .map(|size| (0..2).map(move |n| BuiltinWord::TupleSet(size, n)))
            .flatten();
        call.chain(tuple_compose)
            .chain(tuple_decompose)
            .chain(tuple_get)
            .chain(tuple_set)
    }
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
            BuiltinWord::TupleCompose(size) => {
                let params: Vec<_> = DefaultParams::default().take(*size as usize).collect();
                (params.clone(), vec![Primitive::Tuple(params).into()])
            }
            BuiltinWord::TupleDecompose(size) => {
                let params: Vec<_> = DefaultParams::default().take(*size as usize).collect();
                (vec![Primitive::Tuple(params.clone()).into()], params)
            }
            BuiltinWord::TupleGet(size, n) => {
                let params: Vec<_> = DefaultParams::default().take(*size as usize).collect();
                let output = params[*n as usize].clone();
                (vec![Primitive::Tuple(params).into()], vec![output])
            }
            BuiltinWord::TupleSet(size, n) => {
                let params: Vec<_> = DefaultParams::default().take(*size as usize).collect();
                let input = params[*n as usize].clone();
                (vec![input], vec![Primitive::Tuple(params).into()])
            }
            BuiltinWord::If => (vec![Primitive::Bool.into(), a(), a()], vec![a()]),
            BuiltinWord::Dup => (vec![a()], vec![a(); 2]),
            BuiltinWord::List => (vec![], vec![generic_list()]),
            BuiltinWord::ListPushBack | BuiltinWord::ListPushFront => {
                (vec![generic_list(), a()], vec![generic_list()])
            }
            BuiltinWord::ListPopBack | BuiltinWord::ListPopFront => {
                (vec![generic_list()], vec![generic_list(), a()])
            }
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
            BuiltinWord::ListPushBack => Ident::module("list", "|<"),
            BuiltinWord::ListPushFront => Ident::module("list", ">|"),
            BuiltinWord::ListPopBack => Ident::module("list", "|>"),
            BuiltinWord::ListPopFront => Ident::module("list", "<|"),
            BuiltinWord::Swap => Ident::module("stack", "swap"),
            BuiltinWord::Pop => Ident::module("stack", "pop"),
            BuiltinWord::TupleCompose(size) => {
                Ident::module("tuple".into(), format!(">>{}", tuple(*size)))
            }
            BuiltinWord::TupleDecompose(size) => {
                Ident::module("tuple".into(), format!("{}>>", tuple(*size)))
            }
            BuiltinWord::TupleGet(_, n) => Ident::module("tuple".into(), format!("{}>>", n)),
            BuiltinWord::TupleSet(_, n) => Ident::module("tuple".into(), format!(">>{}", n)),
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
                true,
            )))
        } else {
            None
        }
    }
}

fn ordinal(n: u8) -> &'static str {
    match n {
        0 => "first",
        1 => "second",
        2 => "third",
        3 => "fourth",
        4 => "fifth",
        5 => "sixth",
        6 => "seventh",
        7 => "eighth",
        8 => "ninth",
        9 => "tenth",
        n => unimplemented!("ordinal not implement for {}", n),
    }
}

fn tuple(n: u8) -> &'static str {
    match n {
        2 => "pair",
        3 => "trio",
        4 => "quad",
        5 => "quint",
        6 => "sext",
        7 => "sept",
        8 => "oct",
        n => unimplemented!("tuple not implement for {}", n),
    }
}
