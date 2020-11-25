#![allow(dead_code)]

use std::{collections::VecDeque, convert::identity, fmt, mem::*, rc::Rc};

use crate::{ast::*, codebase::*, types::*};

pub type StackFn = Box<dyn Fn(&mut Stack)>;

#[derive(Default)]
pub struct Stack {
    pub values: Vec<Value>,
    pub i: usize,
    pub ret: Vec<usize>,
    pub strings: Vec<String>,
    pub panic: Option<String>,
}

impl Stack {
    pub fn jump(&mut self, j: usize) {
        self.ret.push(self.i + 1);
        self.i = j;
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
    pub fn top(&self) -> &Value {
        self.values.last().expect("nothing on top")
    }
    pub fn top_mut(&mut self) -> &mut Value {
        self.values.last_mut().expect("nothing on top")
    }
}

pub enum Instruction {
    Execute(StackFn),
    Jump(usize),
    Return,
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Execute(_) => write!(f, "execute"),
            Instruction::Jump(i) => write!(f, "jump {}", i),
            Instruction::Return => write!(f, "return"),
        }
    }
}

pub fn run(word: Word, defs: &Defs) {
    let mut nodes = if let WordKind::Uiua(nodes) = word.kind {
        nodes
    } else {
        panic!("somehow run builtin word")
    };
    nodes.retain(Node::does_something);
    let mut stack = Stack::default();
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
                    let entry = defs.words.entry_by_hash(&hash, StateQuery::all()).unwrap();
                    let word = entry.item;
                    match word.kind {
                        WordKind::Builtin(bi) => {
                            instrs.push(Instruction::Execute(bi.run_fn(&running_sig)));
                            running_sig = running_sig.compose(&word.sig).unwrap();
                        }
                        WordKind::Uiua(mut nodes) => {
                            running_sig = running_sig.compose(&word.sig).unwrap();
                            nodes.retain(Node::does_something);
                            instrs.push(Instruction::Jump(next_word_start));
                            println!("word ident jumps to {}", next_word_start);
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
                            let index = stack.strings.len();
                            stack.strings.push(s);
                            Box::new(move |stack| stack.push(stack.strings[index].clone().to_val()))
                        }
                    };
                    running_sig = running_sig
                        .compose(&Signature::new(vec![], vec![prim.clone().into()]))
                        .unwrap();
                    instrs.push(Instruction::Execute(f));
                }
                Node::SelfIdent => {
                    println!("self ident jumps to {}", word_start);
                    instrs.push(Instruction::Jump(word_start + 1));
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
                    node_queue.push_back((Signature::new(vec![], vec![]), node_sig, nodes));
                }
                Node::TypeHint(_) | Node::Unhashed(_) => {}
            }
        }
        instrs.push(Instruction::Return);
        word_start += seq_len;
    }
    for (i, instr) in instrs.iter().enumerate() {
        println!("{}: {:?}", i, instr);
    }
    while stack.i < instrs.len() {
        match &instrs[stack.i] {
            Instruction::Execute(f) => {
                f(&mut stack);
                stack.i += 1;
                if let Some(message) = stack.panic.take() {
                    println!("panicked: {}", message);
                    break;
                }
            }
            Instruction::Jump(j) => stack.jump(*j),
            Instruction::Return => {
                if let Some(r) = stack.ret.pop() {
                    stack.i = r;
                } else {
                    break;
                }
            }
        }
    }
}

pub struct Value {
    u: u64,
    clone: fn(u64) -> u64,
    drop: fn(u64),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        Value {
            u: (self.clone)(self.u),
            clone: self.clone,
            drop: self.drop,
        }
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        (self.drop)(self.u);
    }
}

impl Value {
    pub fn get<T>(&self) -> T
    where
        T: StackVal,
    {
        T::from_u(self.u)
    }
    pub fn get_ptr<T>(&self) -> &T
    where
        T: HeapVal,
    {
        T::ptr_from_u(self.u)
    }
    pub fn get_ptr_mut<T>(&mut self) -> &mut T
    where
        T: HeapVal,
    {
        Ptr::make_mut(T::ptr_from_u(self.u))
    }
    pub fn unwrap<T>(mut self) -> T
    where
        T: HeapVal,
    {
        self.drop = no_drop;
        Ptr::try_unwrap(*unsafe { Box::from_raw(self.u as usize as *mut Ptr<T>) })
            .unwrap_or_else(|ptr| (*ptr).clone())
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

pub type Text = String;
pub type List = VecDeque<Value>;

pub trait StackVal: Copy {
    fn from_u(u: u64) -> Self;
    fn to_u(self) -> u64;
    fn to_val(self) -> Value {
        Value {
            u: self.to_u(),
            clone: identity,
            drop: no_drop,
        }
    }
}

fn no_drop(_: u64) {}

pub type Ptr<T> = Rc<T>;

pub trait HeapVal: Clone {
    fn drop(u: u64) {
        unsafe { Box::from_raw(u as usize as *mut Ptr<Self>) };
    }
    fn ptr_from_u<'a>(u: u64) -> &'a mut Ptr<Self> {
        unsafe { &mut *(u as usize as *mut Ptr<Self>) }
    }
    fn ptr_to_u(ptr: Ptr<Self>) -> u64 {
        Box::leak(Box::new(ptr)) as *mut Ptr<Self> as usize as u64
    }
    fn to_u(self) -> u64 {
        Self::ptr_to_u(Ptr::new(self))
    }
    fn clone_u(u: u64) -> u64 {
        let ptr = Self::ptr_from_u(u);
        let ptr_clone = Ptr::clone(ptr);
        Self::ptr_to_u(ptr_clone)
    }
    fn to_val(self) -> Value {
        Value {
            u: self.to_u(),
            clone: Self::clone_u,
            drop: Self::drop,
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
}

impl<T> HeapVal for T where T: Clone {}
