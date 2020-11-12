use crate::types::*;

macro_rules! builtin {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum Builtin {
            $(
                $(#[$doc])*
                $name
            ),*
        }
        impl Builtin {
            pub const ALL: &'static [Builtin] = &[$(Builtin::$name),*];
        }
    };
}

builtin!(
    /// Duplicate
    Dup,
    /// Make a list
    List,
    /// Append to a list
    App,
    /// Swap the top 2 items
    Swap,
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

impl Builtin {
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            Builtin::Dup => (vec![t()], vec![t(); 2]),
            Builtin::List => (vec![], vec![generic_list()]),
            Builtin::App => (vec![generic_list(), t()], vec![generic_list()]),
            Builtin::Swap => (vec![t(), u()], vec![u(), t()]),
        };
        Signature::new(before, after)
    }
    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Dup => "dup",
            Builtin::List => "list",
            Builtin::App => "app",
            Builtin::Swap => "swap",
        }
    }
}
