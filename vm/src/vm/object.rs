use std::fmt::{Debug, Formatter};

use crate::chunk::{Chunk, StackPosition, UpvalueIndex};
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

// Closure contains upvalues and upvalue count through the function pointer, so captured can be
// created with known size in advance.
// Some primer on upvalues, closed upvalues are those which have been captured by the closure, i.e
// their ownership is transferred from their original scope to the closure because the original
// scope will go out and the upvalue would be dropped. The opened upvalues are those which are
// still referring to the original value through stack offsets. Note that in the textbook, they
// are pointers. But I will need to use an index because of Rust.
#[derive(Debug)]
pub(super) struct Closure {
    pub(super) fun: &'static Function,
    pub(super) captured: Vec<StackPosition>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
