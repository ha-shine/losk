use crate::errors::LoskError;
use crate::interpreter::Interpreter;
use crate::token::Literal;
use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub(crate) enum CallableType {
    Function,
}

pub(crate) trait Callable {
    fn ty(&self) -> CallableType {
        CallableType::Function
    }

    fn name(&self) -> &str;
    fn arity(&self) -> usize;
    fn execute(
        &self,
        interpreter: &mut Interpreter,
        args: &[Literal],
    ) -> Result<Literal, LoskError>;
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?} {}>", self.ty(), self.name())
    }
}

pub(crate) type BoxedFunction = Box<dyn Fn(&[Literal]) -> Result<Literal, LoskError>>;

pub(crate) struct NativeCallable {
    func: BoxedFunction,
    name: String,
    arity: usize,
}

impl NativeCallable {
    pub(crate) fn new(func: BoxedFunction, name: String, arity: usize) -> Self {
        Self { func, name, arity }
    }
}

impl Callable for NativeCallable {
    fn name(&self) -> &str {
        &self.name
    }

    fn arity(&self) -> usize {
        self.arity
    }

    fn execute(&self, _: &mut Interpreter, args: &[Literal]) -> Result<Literal, LoskError> {
        (self.func)(args)
    }
}
