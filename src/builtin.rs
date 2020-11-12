use crate::types::*;

macro_rules! builtin {
    ($($(#[$doc:meta])? $name:ident),*) => {
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
    Dup
);

impl Builtin {
    pub fn sig(&self) -> Signature {
        match self {
            Builtin::Dup => Signature::new(vec![Type::Generic(0)], vec![Type::Generic(0); 2]),
        }
    }
    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Dup => "dup",
        }
    }
}
