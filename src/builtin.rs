use crate::types::*;

macro_rules! builtin {
    ($($(#[$doc:meta])? $name:ident,)*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum BuiltinDef {
            $(
                $(#[$doc])*
                $name
            ),*
        }
        impl BuiltinDef {
            pub const ALL: &'static [BuiltinDef] = &[$(BuiltinDef::$name),*];
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
    pub fn sig(&self) -> Signature {
        let (before, after) = match self {
            BuiltinDef::Dup => (vec![t()], vec![t(); 2]),
            BuiltinDef::List => (vec![], vec![generic_list()]),
            BuiltinDef::App => (vec![generic_list(), t()], vec![generic_list()]),
            BuiltinDef::Swap => (vec![t(), u()], vec![u(), t()]),
            BuiltinDef::Add => (vec![t(); 2], vec![t()]),
            BuiltinDef::Pop => (vec![t()], vec![]),
        };
        Signature::new(before, after)
    }
    pub fn name(&self) -> &'static str {
        match self {
            BuiltinDef::Dup => "dup",
            BuiltinDef::List => "list",
            BuiltinDef::App => "app",
            BuiltinDef::Swap => "swap",
            BuiltinDef::Add => "+",
            BuiltinDef::Pop => "pop",
        }
    }
}
