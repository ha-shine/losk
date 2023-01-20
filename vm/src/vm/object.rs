use std::cell::RefCell;
use std::fmt::{Debug, Formatter};

use crate::chunk::{Chunk, UpvalueIndex};
use crate::limits::UPVALUE_LIMIT;
use crate::value::ConstantValue;
use crate::vm::StackValue;

pub(super) type NativeFn = fn(&[StackValue]) -> Option<NativeValue>;

#[allow(dead_code)]
pub(super) enum NativeValue {
    Num(usize),
    Bool(bool),
    Str(String),
    Nil,
}

impl From<NativeValue> for ConstantValue {
    fn from(value: NativeValue) -> Self {
        match value {
            NativeValue::Num(val) => ConstantValue::Double(val as f64),
            NativeValue::Bool(val) => ConstantValue::Bool(val),
            NativeValue::Str(val) => ConstantValue::Str(val),
            NativeValue::Nil => ConstantValue::Nil,
        }
    }
}

pub(super) struct NativeFunction {
    pub(super) name: String,
    pub(super) arity: usize,
    pub(super) fun: NativeFn,
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFunction {}:{}>", self.name, self.arity)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub(crate) struct Upvalue {
    pub(crate) index: usize,
    pub(crate) is_local: bool,
}

impl Upvalue {
    pub(crate) fn new(index: usize, is_local: bool) -> Self {
        Upvalue { index, is_local }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) upvalue_count: usize,
    pub(crate) upvalues: [Upvalue; UPVALUE_LIMIT],
}

impl Function {
    pub(crate) fn new(name: &str, arity: usize) -> Self {
        Function {
            name: name.to_string(),
            arity,
            chunk: Chunk::new(),
            upvalues: [Default::default(); UPVALUE_LIMIT],
            upvalue_count: 0,
        }
    }

    pub(crate) fn add_upvalue(&mut self, upvalue: Upvalue) -> Result<UpvalueIndex, &'static str> {
        let idx = self.upvalue_count;

        // First check if the same captured variable has been referenced before
        if let Some(i) = (0..idx).rev().find(|i| {
            self.upvalues[*i].index == upvalue.index
                && self.upvalues[*i].is_local == upvalue.is_local
        }) {
            Ok(UpvalueIndex(i))
        } else if idx == UPVALUE_LIMIT {
            Err("Too many closure variables in a function.")
        } else {
            self.upvalue_count += 1;
            self.upvalues[idx] = upvalue;
            Ok(UpvalueIndex(idx))
        }
    }
}

impl NativeFunction {
    pub(crate) fn new(
        name: &str,
        arity: usize,
        fun: fn(&[StackValue]) -> Option<NativeValue>,
    ) -> Self {
        NativeFunction {
            name: name.to_string(),
            arity,
            fun,
        }
    }
}

// Closure contains upvalues and upvalue count? It has access to upvaulues through its function
// but how does it knows the actual values of those upvalues?
#[derive(Debug)]
pub(super) struct Closure {
    pub(super) fun: &'static Function,

    // Not sure how to capture variables yet if they are stack values like boolean?
    // I can't store the whole object in this vector as well, instead of the pointer to object
    // because the captured value might be modified by outer scope. e.g
    //
    // var name = "Shine";
    // fun inner() { print name; }
    // name = "John";
    // inner();
    //
    // When this happens, should Lox prints "John" or "Shine"?
    // Should the variables be final if they were to be captured in a closure like Java?
    // It would certainly make the code more performant since now I can just store those objects
    // in this closure.
    //
    // In the text book, the captured (called `upvalues` verbatim) are pointers to original
    // stack value. I can't do this easily here because of Rust without re-writing a big
    // chunk of this.
    // The good thing about this is that for normal stack values, the values are copied, and
    // modification to the captured objects will not affect the original. This even works for
    // strings because strings can only be concatenated and the results are new heap allocated
    // string and the pointer to there are captured. The original pointer is not modified while
    // the captured one will point to new string after concatenation.
    // With classes, this will be an issue because mutation to objects will not create new objects.
    pub(super) captured: RefCell<Vec<StackValue>>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
