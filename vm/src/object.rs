use crate::chunk::Chunk;
use crate::value::Value;
use crate::vm::StackValue;
use std::fmt::{Debug, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
}

pub(crate) type NativeFn = fn(&[StackValue]) -> Option<Value>;

pub(crate) struct NativeFunction {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) fun: NativeFn,
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFunction {}:{}>", self.name, self.arity)
    }
}

impl Function {
    pub(crate) fn new(name: &str, arity: usize) -> Self {
        Function {
            name: name.to_string(),
            arity,
            chunk: Chunk::new(),
        }
    }
}

impl NativeFunction {
    pub(crate) fn new(name: &str, arity: usize, fun: fn(&[StackValue]) -> Option<Value>) -> Self {
        NativeFunction {
            name: name.to_string(),
            arity,
            fun,
        }
    }
}
