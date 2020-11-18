use std::mem::transmute;

use crate::{ast::*, codebase::*, types::*};

// pub type Ptr<T> = std::rc::Rc<T>;

pub type StackFn = Box<dyn Fn(&mut Stack)>;

pub type Stack = Vec<Value>;

pub struct RunNode {
    pub f: StackFn,
    pub ty: Primitive,
}

pub fn run(word: Word, defs: &Defs) {}

#[derive(Debug)]
pub struct Value(pub u64);

pub trait ValueAssert {
    fn interpret(val: u64) -> Self;
}

impl ValueAssert for u64 {
    fn interpret(val: u64) -> Self {
        val
    }
}

impl ValueAssert for i64 {
    fn interpret(val: u64) -> Self {
        unsafe { transmute(val) }
    }
}

impl ValueAssert for f64 {
    #[allow(clippy::transmute_int_to_float)]
    fn interpret(val: u64) -> Self {
        unsafe { transmute(val) }
    }
}

impl ValueAssert for bool {
    fn interpret(val: u64) -> Self {
        val != 0
    }
}

impl ValueAssert for char {
    #[allow(clippy::transmute_int_to_char)]
    fn interpret(val: u64) -> Self {
        unsafe { transmute(val as u32) }
    }
}
