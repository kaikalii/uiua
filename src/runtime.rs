#![allow(dead_code)]

use std::{collections::VecDeque, convert::identity, fmt, mem::*, rc::Rc};

use itertools::*;

use crate::{ast::*, codebase::*, types::*};

pub type StackFn = Box<dyn Fn(&mut Stack)>;

#[derive(Default)]
pub struct Stack {
    pub values: Vec<Value>,
    pub i: usize,
    pub ret: Vec<usize>,
}

impl Stack {
    pub fn jump(&mut self, j: usize) {
        self.ret.push(self.i + 1);
        self.i = j;
        println!("jump to {}", j);
    }
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

pub enum Instruction {
    Execute(StackFn),
    Jump(usize),
    Return,
}

pub fn run(word: Word, defs: &Defs) {
    let mut nodes = if let WordKind::Uiua(nodes) = word.kind {
        nodes
    } else {
        panic!("somehow run builtin word")
    };
    nodes.retain(Node::does_something);
    let mut instrs = Vec::new();
    let mut running_sig;
    let mut word_start = 0;
    let mut next_word_start = nodes.len() + 1;
    // ( prev sig, nodes sig, nodes )
    let mut node_queue = VecDeque::new();
    node_queue.push_back((Signature::new(vec![], vec![]), word.sig, nodes));
    while let Some((prev_sig, word_sig, seq)) = node_queue.pop_front() {
        running_sig = prev_sig;
        let seq_len = seq.len();
        for node in seq {
            match node {
                Node::Ident(hash) => {
                    let entry = defs.words.entry_by_hash(&hash, Query::All).unwrap();
                    let word = entry.item;
                    running_sig = running_sig.compose(&word.sig).unwrap();
                    match word.kind {
                        WordKind::Builtin(bi) => {
                            instrs.push(Instruction::Execute(bi.run_fn()));
                        }
                        WordKind::Uiua(mut nodes) => {
                            nodes.retain(Node::does_something);
                            instrs.push(Instruction::Jump(next_word_start));
                            next_word_start += nodes.len() + 1;
                            node_queue.push_back((running_sig.clone(), word.sig, nodes));
                        }
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
                    running_sig = running_sig
                        .compose(&Signature::new(vec![], vec![prim.clone().into()]))
                        .unwrap();
                    instrs.push(Instruction::Execute(f));
                }
                Node::SelfIdent => {
                    instrs.push(Instruction::Jump(word_start));
                    running_sig = running_sig.compose(&word_sig).unwrap();
                }
                Node::Quotation { sig, mut nodes } => {
                    nodes.retain(Node::does_something);
                    instrs.push(Instruction::Execute(Box::new(move |stack| {
                        stack.push(next_word_start)
                    })));
                    let node_sig = Signature::new(vec![], vec![Primitive::Quotation(sig).into()]);
                    running_sig = running_sig.compose(&node_sig).unwrap();
                    next_word_start += nodes.len() + 1;
                    node_queue.push_back((running_sig.clone(), node_sig, nodes));
                }
                Node::Unhashed(_) => {}
            }
        }
        instrs.push(Instruction::Return);
        word_start += seq_len;
    }
    let mut stack = Stack::default();
    println!();
    while stack.i < instrs.len() {
        print!("{}: ", stack.i);
        match &instrs[stack.i] {
            Instruction::Execute(f) => {
                f(&mut stack);
                for val in &stack.values {
                    print!("{} ", (val.debug)(val.u));
                }
                println!();
                stack.i += 1;
            }
            Instruction::Jump(j) => stack.jump(*j),
            Instruction::Return => {
                if let Some(r) = stack.ret.pop() {
                    println!("ret to {}", r);
                    stack.i = r;
                } else {
                    println!("ret to end");
                    break;
                }
            }
        }
    }
    println!();
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

impl StackVal for usize {
    fn from_u(u: u64) -> Self {
        u as usize
    }
    fn to_u(self) -> u64 {
        self as u64
    }
    fn debug(u: u64) -> String {
        format!("jump({})", u)
    }
    fn display(u: u64) -> String {
        format!("jump({})", u)
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
