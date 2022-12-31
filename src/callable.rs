use crate::ast::Stmt;
use crate::env::Environment;
use crate::errors::LoskError;
use crate::interpreter::Interpreter;
use crate::token::{Literal, Token};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

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

// `NativeCallable` bridges the native rust calls and the Losk interpreter environment.
// This implements callable and all of these trait objects will live in the global namespace.
pub(crate) struct Native {
    func: BoxedFunction,
    name: String,
    arity: usize,
}

impl Native {
    pub(crate) fn new(func: BoxedFunction, name: String, arity: usize) -> Self {
        Self { func, name, arity }
    }
}

impl Callable for Native {
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

pub(crate) struct Function {
    closure: Rc<RefCell<Environment>>,
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

impl Function {
    // This can be wasteful because I am storing the statements in the body. Maybe it would be
    // better if an Rc instead of a reference is used? That way the functions can just refer to the
    // statements as pointers. But locality is lost since now the statements (and expressions)
    // are just pointers to another place in heap.
    pub(crate) fn new(
        closure: Rc<RefCell<Environment>>,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Self {
        Function {
            closure,
            name: name.clone(),
            params: Vec::from(params),
            body: Vec::from(body),
        }
    }
}

impl Callable for Function {
    fn name(&self) -> &str {
        &self.name.lexeme
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn execute(
        &self,
        interpreter: &mut Interpreter,
        args: &[Literal],
    ) -> Result<Literal, LoskError> {
        let mut env = Environment::with(self.closure.clone());
        for (param, arg) in self.params.iter().zip(args) {
            env.define(&param.lexeme, arg.clone());
        }

        // TODO: Return type??
        interpreter.execute_block_with_env(&self.body, Rc::new(RefCell::new(env)))?;
        Ok(Literal::Nil)
    }
}
