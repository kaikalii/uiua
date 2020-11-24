use std::mem;

use serde::*;
use sha3::*;

use crate::{ast::*, runtime::*, types::*};

pub static PRELUDE: &[&str] = &[
    "stack", "list", "tuple", "io", "control", "string", "option",
];

macro_rules! builtin_words {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Serialize, Deserialize)]
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
            Format(Primitive),
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
                    BuiltinWord::Format(_) => format!("Pop an item and push it converted to text"),
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
    /// Print a message to the standard output
    Print,
    /// Print a message to the standard output with a newline
    Println,
    /// Unwrap an Option. Panics if the Option is empty
    OptionUnwrap,
    /// Check if an Option has a value
    OptionIsSome,
    /// Check if an Option is empty
    OptionIsNone,
);

fn generic_list() -> Type {
    Type::Generic(Generic::new("a", 0, true)).list()
}

fn generic(name: &str, i: u8) -> Type {
    Type::Generic(Generic::new(name, i, true))
}

pub fn a() -> Type {
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
            .map(|size| (0..size).map(move |n| BuiltinWord::TupleGet(size, n)))
            .flatten();
        let tuple_set = (2..=8)
            .map(|size| (0..size).map(move |n| BuiltinWord::TupleSet(size, n)))
            .flatten();
        let format = vec![
            Primitive::<Type>::Bool,
            Primitive::Nat,
            Primitive::Int,
            Primitive::Float,
            Primitive::Char,
        ]
        .into_iter()
        .map(BuiltinWord::Format);
        call.chain(tuple_compose)
            .chain(tuple_decompose)
            .chain(tuple_get)
            .chain(tuple_set)
            .chain(format)
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
                (
                    vec![Primitive::Tuple(params.clone()).into()],
                    vec![Primitive::Tuple(params).into(), output],
                )
            }
            BuiltinWord::TupleSet(size, n) => {
                let params: Vec<_> = DefaultParams::default().take(*size as usize).collect();
                let input = params[*n as usize].clone();
                (
                    vec![Primitive::Tuple(params.clone()).into(), input],
                    vec![Primitive::Tuple(params).into()],
                )
            }
            BuiltinWord::If => (vec![Primitive::Bool.into(), a(), a()], vec![a()]),
            BuiltinWord::Dup => (vec![a()], vec![a(); 2]),
            BuiltinWord::List => (vec![], vec![generic_list()]),
            BuiltinWord::ListPushBack | BuiltinWord::ListPushFront => {
                (vec![generic_list(), a()], vec![generic_list()])
            }
            BuiltinWord::ListPopBack | BuiltinWord::ListPopFront => {
                (vec![generic_list()], vec![generic_list(), a().option()])
            }
            BuiltinWord::Swap => (vec![a(), b()], vec![b(), a()]),
            BuiltinWord::Pop => (vec![a()], vec![]),
            BuiltinWord::Panic => (vec![Primitive::Text.into()], vec![]),
            BuiltinWord::Print | BuiltinWord::Println => (vec![Primitive::Text.into()], vec![]),
            BuiltinWord::Format(prim) => (vec![prim.clone().into()], vec![Primitive::Text.into()]),
            BuiltinWord::OptionUnwrap => (vec![a().option()], vec![a()]),
            BuiltinWord::OptionIsSome | BuiltinWord::OptionIsNone => (
                vec![a().option()],
                vec![a().option(), Primitive::Bool.into()],
            ),
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
            BuiltinWord::Print => Ident::module("io", "print"),
            BuiltinWord::Println => Ident::module("io", "println"),
            BuiltinWord::Format(_) => Ident::module("string", "fmt"),
            BuiltinWord::OptionUnwrap => Ident::module("option", "unwrap"),
            BuiltinWord::OptionIsSome => Ident::module("option", "is_some"),
            BuiltinWord::OptionIsNone => Ident::module("option", "is_none"),
        }
    }
    pub fn aliases(&self) -> Vec<Ident> {
        match self {
            BuiltinWord::ListPushBack => vec![Ident::module("list", ",")],
            _ => Vec::new(),
        }
    }
    pub fn run_fn(&self, _sig: &Signature) -> StackFn {
        match self {
            BuiltinWord::Pop => Box::new(|stack| {
                stack.pop();
            }),
            BuiltinWord::Dup => Box::new(|stack| {
                let duped = stack.top().clone();
                stack.push(duped);
            }),
            BuiltinWord::Swap => Box::new(|stack| {
                let a = stack.pop();
                let b = stack.pop();
                stack.push(a);
                stack.push(b);
            }),
            BuiltinWord::Call(..) => Box::new(|stack| {
                let j = stack.pop().get::<usize>();
                stack.jump(j);
                stack.i -= 1;
            }),
            BuiltinWord::List => Box::new(|stack| stack.push(List::new().to_val())),
            BuiltinWord::ListPushBack => Box::new(|stack| {
                let item = stack.pop();
                stack.top_mut().get_ptr_mut::<List>().push_back(item);
            }),
            BuiltinWord::ListPushFront => Box::new(|stack| {
                let item = stack.pop();
                stack.top_mut().get_ptr_mut::<List>().push_front(item);
            }),
            BuiltinWord::ListPopBack => Box::new(|stack| {
                let item = stack.top_mut().get_ptr_mut::<List>().pop_back();
                stack.push(item.to_val());
            }),
            BuiltinWord::ListPopFront => Box::new(|stack| {
                let item = stack.top_mut().get_ptr_mut::<List>().pop_front();
                stack.push(item.to_val());
            }),
            BuiltinWord::Print => Box::new(|stack| print!("{}", stack.pop().get_ptr::<Text>())),
            BuiltinWord::Println => Box::new(|stack| println!("{}", stack.pop().get_ptr::<Text>())),
            BuiltinWord::Format(prim) => match prim {
                Primitive::Bool => Box::new(|stack| {
                    let s = stack.pop().get::<bool>().to_string();
                    stack.push(s.to_val());
                }),
                Primitive::Nat => Box::new(|stack| {
                    let s = stack.pop().get::<u64>().to_string();
                    stack.push(s.to_val());
                }),
                Primitive::Int => Box::new(|stack| {
                    let s = stack.pop().get::<i64>().to_string();
                    stack.push(s.to_val());
                }),
                Primitive::Float => Box::new(|stack| {
                    let s = stack.pop().get::<f64>().to_string();
                    stack.push(s.to_val());
                }),
                Primitive::Char => Box::new(|stack| {
                    let s = stack.pop().get::<char>().to_string();
                    stack.push(s.to_val());
                }),
                _ => unimplemented!(),
            },
            BuiltinWord::If => Box::new(|stack| {
                let b = stack.pop();
                let a = stack.pop();
                let cond = stack.pop();
                stack.push(if cond.get() { a } else { b });
            }),
            BuiltinWord::Panic => Box::new(|stack| {
                let message = stack.pop().unwrap::<Text>();
                stack.panic = Some(message);
            }),
            BuiltinWord::OptionUnwrap => Box::new(|stack| {
                let op = stack.pop().unwrap::<Option<Value>>();
                if let Some(val) = op {
                    stack.push(val);
                } else {
                    stack.panic = Some("Attempted to unwrap an empty Option".into());
                }
            }),
            BuiltinWord::OptionIsSome => Box::new(|stack| {
                let b = stack.top().get_ptr::<Option<Value>>().is_some();
                stack.push(b);
            }),
            BuiltinWord::OptionIsNone => Box::new(|stack| {
                let b = stack.top().get_ptr::<Option<Value>>().is_none();
                stack.push(b);
            }),
            _ => todo!("other builtin functions"),
        }
    }
}

impl TreeHash for BuiltinWord {
    fn hash(&self, sha: &mut Sha3_256) {
        sha.update(unsafe { mem::transmute::<_, [u8; 8]>(mem::discriminant(self)) });
        match self {
            BuiltinWord::Call(a, b) | BuiltinWord::TupleGet(a, b) | BuiltinWord::TupleSet(a, b) => {
                sha.update(&[*a]);
                sha.update(&[*b]);
            }
            BuiltinWord::TupleCompose(size) | BuiltinWord::TupleDecompose(size) => {
                sha.update(&[*size])
            }
            _ => {}
        }
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
