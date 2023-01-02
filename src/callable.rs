use crate::ast::Stmt;
use crate::env::Environment;
use crate::errors::LoskError;
use crate::interpreter::Interpreter;
use crate::token::{Literal, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub(crate) enum CallableType {
    Function,
    Class,
}

pub(crate) trait Callable {
    fn ty(&self) -> CallableType {
        CallableType::Function
    }

    fn name(&self) -> &str;
    fn arity(&self) -> usize;
    fn execute(
        self: Rc<Self>,
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

    fn execute(
        self: Rc<Self>,
        _: &mut Interpreter,
        args: &[Literal],
    ) -> Result<Literal, LoskError> {
        (self.func)(args)
    }
}

#[derive(Debug)]
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
    fn ty(&self) -> CallableType {
        CallableType::Function
    }

    fn name(&self) -> &str {
        &self.name.lexeme
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn execute(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        args: &[Literal],
    ) -> Result<Literal, LoskError> {
        let mut env = Environment::with(self.closure.clone());
        for (param, arg) in self.params.iter().zip(args) {
            env.define(&param.lexeme, arg.clone());
        }

        match interpreter.execute_block_with_env(&self.body, Rc::new(RefCell::new(env))) {
            Ok(()) => Ok(Literal::Nil),
            Err(LoskError::Return(value)) => Ok(value.value),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Class {
    name: String,
    methods: HashMap<String, Rc<Function>>,
}

impl Class {
    pub(crate) fn new(name: &str, methods: HashMap<String, Rc<Function>>) -> Rc<Self> {
        Rc::new(Class {
            name: name.to_string(),
            methods,
        })
    }

    fn find_method(&self, name: &str) -> Option<Rc<Function>> {
        self.methods.get(name).cloned()
    }
}

impl Callable for Class {
    fn ty(&self) -> CallableType {
        CallableType::Class
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn arity(&self) -> usize {
        0
    }

    fn execute(self: Rc<Self>, _: &mut Interpreter, _: &[Literal]) -> Result<Literal, LoskError> {
        Ok(Literal::Instance(Instance::new(self)))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Literal>,
}

impl Instance {
    pub(crate) fn new(class: Rc<Class>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Instance {
            class,
            fields: HashMap::new(),
        }))
    }

    pub(crate) fn get(&self, name: &str) -> Option<Literal> {
        if let Some(field) = self.fields.get(name) {
            Some(field.clone())
        } else {
            self.class
                .find_method(name)
                .map(|method| Literal::Callable(method))
        }
    }

    pub(crate) fn set(&mut self, name: &str, value: Literal) -> Literal {
        self.fields.insert(String::from(name), value.clone());
        value
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Instance {}.class>", self.class.name())
    }
}
