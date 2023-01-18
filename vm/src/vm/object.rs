use crate::chunk::Chunk;
use crate::value::ConstantValue;
use crate::vm::StackValue;
use std::fmt::{Debug, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
}

pub(super) type NativeFn = fn(&[StackValue]) -> Option<ConstantValue>;

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
    pub(crate) fn new(
        name: &str,
        arity: usize,
        fun: fn(&[StackValue]) -> Option<ConstantValue>,
    ) -> Self {
        NativeFunction {
            name: name.to_string(),
            arity,
            fun,
        }
    }
}
