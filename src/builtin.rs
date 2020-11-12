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
    Primitive::list(Type::Generic(0)).into()
}

fn generic(g: u8) -> Type {
    Type::Generic(g)
}

impl Builtin {
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            Builtin::Dup => (vec![generic(0)], vec![generic(0); 2]),
            Builtin::List => (vec![], vec![generic_list()]),
            Builtin::App => (vec![generic_list(), generic(0)], vec![generic_list()]),
            Builtin::Swap => (vec![generic(0), generic(1)], vec![generic(1), generic(0)]),
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
