#![allow(dead_code)]

use std::{collections::VecDeque, convert::identity, fmt, mem::*, rc::Rc};

use itertools::*;

use crate::{ast::*, codebase::*, types::*};

pub type StackFn = Box<dyn Fn(&mut Stack)>;

#[derive(Default)]
pub struct Stack {
    pub values: Vec<Value>,
}

impl Stack {
    pub fn push<V>(&mut self, val: V)
    where
        V: Into<Value>,
    {
        self.values.push(val.into())
    }
    pub fn pop(&mut self) -> Value {
        self.values.pop().expect("nothing to pop")
    }
    pub fn top(&self) -> Value {
        *self.values.last().expect("nothing on top")
    }
}

pub struct Instruction {
    pub f: StackFn,
    pub ty: Primitive,
}

#[allow(unused_variables)]
pub fn run(word: Word, defs: &Defs) {
    let nodes = if let WordKind::Uiua(nodes) = word.kind {
        nodes
    } else {
        panic!("somehow run builtin word")
    };
    let mut instrs = Vec::new();
    let mut sig = word.sig;
    for node in nodes {
        match node {
            Node::Ident(hash) => {
                let entry = defs.words.entry_by_hash(&hash, Query::All).unwrap();
                let word = entry.item;
                sig = sig.compose(&word.sig).expect("uncomposable signatures");
                match word.kind {
                    WordKind::Builtin(bi) => {
                        instrs.push(Instruction {
                            f: bi.run_fn(),
                            ty: sig
                                .after
                                .last()
                                .cloned()
                                .expect("composed signature has no last type")
                                .unwrap_primitive(),
                        });
                    }
                    WordKind::Uiua(_) => todo!("uiua words"),
                }
            }
            Node::Literal(lit) => {
                let prim = lit.as_primitive();
                let f: StackFn = match lit {
                    Literal::Bool(b) => Box::new(move |stack| stack.push(b)),
                    Literal::Nat(n) => Box::new(move |stack| stack.push(n)),
                    Literal::Int(i) => Box::new(move |stack| stack.push(i)),
                    Literal::Float(f) => Box::new(move |stack| stack.push(f)),
                    Literal::Char(c) => Box::new(move |stack| stack.push(c)),
                    Literal::Text(s) => {
                        let val = s.to_val();
                        Box::new(move |stack| stack.push(val))
                    }
                };
                sig = sig
                    .compose(&Signature::new(vec![], vec![prim.clone().into()]))
                    .unwrap();
                instrs.push(Instruction { f, ty: prim });
            }
            _ => {}
        }
    }
    let mut i = 0;
    let mut stack = Stack::default();
    while i < instrs.len() {
        (instrs[i].f)(&mut stack);
        i += 1;
    }
    for val in stack.values {
        print!("{}", (val.debug)(val.u));
        val.drop();
        print!(" ");
    }
}

#[derive(Clone, Copy)]
pub struct Value {
    u: u64,
    dup: fn(u64) -> u64,
    drop: fn(u64),
    debug: fn(u64) -> String,
    display: Option<fn(u64) -> String>,
}

impl Value {
    pub fn dup(self) -> Value {
        Value {
            u: (self.dup)(self.u),
            ..self
        }
    }
    pub fn drop(self) {
        (self.drop)(self.u)
    }
    pub fn get<T>(self) -> T
    where
        T: StackVal,
    {
        T::from_u(self.u)
    }
    pub fn get_ptr_mut<'a, T>(self) -> &'a mut T
    where
        T: HeapVal,
    {
        Ptr::make_mut(T::ptr_from_u(self.u))
    }
}

impl<T> From<T> for Value
where
    T: StackVal,
{
    fn from(val: T) -> Self {
        val.to_val()
    }
}

pub type List = VecDeque<Value>;

pub trait StackVal: Copy + fmt::Debug + fmt::Display {
    fn from_u(u: u64) -> Self;
    fn to_u(self) -> u64;
    fn debug(u: u64) -> String {
        format!("{:?}", Self::from_u(u))
    }
    fn display(u: u64) -> String {
        format!("{}", Self::from_u(u))
    }
    fn to_val(self) -> Value {
        Value {
            u: self.to_u(),
            dup: identity,
            drop: no_drop,
            debug: Self::debug,
            display: Some(Self::display),
        }
    }
}

fn no_drop(_: u64) {}

pub type Ptr<T> = Rc<ManuallyDrop<T>>;

pub trait HeapVal: Clone {
    fn drop_it(&self);
    fn debug(&self) -> String;
    fn display_u() -> Option<fn(u64) -> String>;
    fn drop(u: u64) {
        Self::drop_it(&*unsafe { Box::from_raw(u as usize as *mut Ptr<Self>) });
    }
    fn ptr_from_u<'a>(u: u64) -> &'a mut Ptr<Self> {
        unsafe { &mut *(u as usize as *mut Ptr<Self>) }
    }
    fn ptr_to_u(ptr: Ptr<Self>) -> u64 {
        Box::leak(Box::new(ptr)) as *mut Ptr<Self> as usize as u64
    }
    fn to_u(self) -> u64 {
        Self::ptr_to_u(Ptr::new(ManuallyDrop::new(self)))
    }
    fn debug_u(u: u64) -> String {
        Self::ptr_from_u(u).debug()
    }
    fn dup(u: u64) -> u64 {
        let ptr = Self::ptr_from_u(u);
        let ptr_clone = Ptr::clone(ptr);
        Self::ptr_to_u(ptr_clone)
    }
    fn to_val(self) -> Value {
        Value {
            u: self.to_u(),
            dup: Self::dup,
            drop: Self::drop,
            debug: Self::debug_u,
            display: Self::display_u(),
        }
    }
}

impl StackVal for u64 {
    fn from_u(u: u64) -> Self {
        u
    }
    fn to_u(self) -> u64 {
        self
    }
}

impl StackVal for i64 {
    fn from_u(u: u64) -> Self {
        unsafe { transmute(u) }
    }
    fn to_u(self) -> u64 {
        unsafe { transmute(self) }
    }
}

impl StackVal for f64 {
    #[allow(clippy::transmute_int_to_float)]
    fn from_u(u: u64) -> Self {
        unsafe { transmute(u) }
    }
    #[allow(clippy::transmute_float_to_int)]
    fn to_u(self) -> u64 {
        unsafe { transmute(self) }
    }
}

impl StackVal for bool {
    fn from_u(u: u64) -> Self {
        u != 0
    }
    fn to_u(self) -> u64 {
        if self {
            1
        } else {
            0
        }
    }
}

impl StackVal for char {
    #[allow(clippy::transmute_int_to_char)]
    fn from_u(u: u64) -> Self {
        unsafe { transmute(u as u32) }
    }
    fn to_u(self) -> u64 {
        unsafe { transmute(self as u64) }
    }
}

impl HeapVal for String {
    fn drop_it(&self) {}
    fn debug(&self) -> String {
        format!("{:?}", self)
    }
    fn display_u() -> Option<fn(u64) -> String> {
        Some(|u: u64| (***Self::ptr_from_u(u)).clone())
    }
}

impl HeapVal for List {
    fn drop_it(&self) {
        for val in self {
            val.drop();
        }
    }
    fn debug(&self) -> String {
        format!(
            "[{}]",
            self.iter()
                .map(|val| (val.debug)(val.u))
                .intersperse(" ".into())
                .collect::<String>()
        )
    }
    fn display_u() -> Option<fn(u64) -> String> {
        None
    }
}
