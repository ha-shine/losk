use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use losk_core::Token;

use crate::ast::Stmt;
use crate::env::Environment;
use crate::error::Error;
use crate::interpreter::Interpreter;
use crate::value::Value;

#[derive(Debug, PartialEq)]
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
        args: &[Value],
    ) -> Result<Value, Error>;

    // This is a bit hacky
    fn as_class(self: Rc<Self>) -> Option<Rc<Class>>;
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?} {}>", self.ty(), self.name())
    }
}

pub(crate) type BoxedFunction = Box<dyn Fn(&[Value]) -> Result<Value, Error>>;

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

    fn execute(self: Rc<Self>, _: &mut Interpreter, args: &[Value]) -> Result<Value, Error> {
        (self.func)(args)
    }

    fn as_class(self: Rc<Self>) -> Option<Rc<Class>> {
        None
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

// Helper method that will be called from both functions and methods
fn execute_function(
    function: Rc<Function>,
    closure: Rc<RefCell<Environment>>,
    interpreter: &mut Interpreter,
    args: &[Value],
) -> Result<Value, Error> {
    let mut env = Environment::with(closure);
    for (param, arg) in function.params.iter().zip(args) {
        env.define(&param.lexeme, arg.clone());
    }

    match interpreter.execute_block_with_env(&function.body, Rc::new(RefCell::new(env))) {
        Ok(()) => Ok(Value::Nil),
        Err(Error::Return(value)) => Ok(value.value),
        Err(err) => Err(err),
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
        args: &[Value],
    ) -> Result<Value, Error> {
        let closure = Rc::clone(&self.closure);
        execute_function(self, closure, interpreter, args)
    }

    fn as_class(self: Rc<Self>) -> Option<Rc<Class>> {
        None
    }
}

// This is a little bit different from the textbook's class methods. In textbook, a new function
// which has the same type as `Function` is returned with a new closure with `this` bounded to
// calling instance. Since the java objects are references, that is fast. But in Rust, if a new
// function is created whenever a class method is called, it'll not be as efficient since the
// body statements need to be cloned. This tries to remedy that by taking the middle approach where
// the original function is referenced through an Rc. This requires making two Rc clones, which
// consists of making two ref-count increments.
#[derive(Debug)]
pub(crate) struct Method {
    closure: Rc<RefCell<Environment>>,
    function: Rc<Function>,
    is_init: bool,
}

impl Method {
    pub(crate) fn bind(
        function: Rc<Function>,
        instance: Rc<RefCell<Instance>>,
        is_init: bool,
    ) -> Self {
        let closure = Rc::new(RefCell::new(Environment::with(Rc::clone(
            &function.closure,
        ))));
        closure
            .borrow_mut()
            .define("this", Value::Instance(instance));

        Method {
            closure,
            function,
            is_init,
        }
    }
}

impl Callable for Method {
    fn name(&self) -> &str {
        self.function.name()
    }

    fn arity(&self) -> usize {
        self.function.arity()
    }

    fn execute(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, Error> {
        let res = execute_function(
            Rc::clone(&self.function),
            Rc::clone(&self.closure),
            interpreter,
            args,
        );

        match res {
            Ok(_) if self.is_init => Ok(self.closure.borrow().get_at(0, "this").unwrap()),
            _ => res,
        }
    }

    fn as_class(self: Rc<Self>) -> Option<Rc<Class>> {
        None
    }
}

#[derive(Debug)]
pub(crate) struct Class {
    name: String,
    methods: HashMap<String, Rc<Function>>,
    superclass: Option<Rc<Class>>,
}

impl Class {
    pub(crate) fn new(
        name: &str,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, Rc<Function>>,
    ) -> Rc<Self> {
        Rc::new(Class {
            name: name.to_string(),
            methods,
            superclass,
        })
    }

    pub(crate) fn find_method(&self, name: &str) -> Option<Rc<Function>> {
        if let Some(fun) = self.methods.get(name) {
            Some(Rc::clone(fun))
        } else if let Some(superclass) = &self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
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
        match self.find_method("init") {
            Some(init) => init.arity(),
            _ => 0,
        }
    }

    fn execute(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, Error> {
        let instance = Instance::new(Rc::clone(&self));
        if let Some(init) = self.find_method("init") {
            // Not very efficient here, the created instance is copied over to Rc
            Rc::new(Method::bind(init, Rc::clone(&instance), true)).execute(interpreter, args)?;
        }

        Ok(Value::Instance(instance))
    }

    fn as_class(self: Rc<Self>) -> Option<Rc<Class>> {
        Some(self)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub(crate) fn new(class: Rc<Class>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Instance {
            class,
            fields: HashMap::new(),
        }))
    }

    pub(crate) fn get(instance: &Rc<RefCell<Self>>, name: &str) -> Option<Value> {
        if let Some(field) = instance.borrow().fields.get(name) {
            Some(field.clone())
        } else {
            instance.borrow().class.find_method(name).map(|function| {
                Value::Callable(Rc::new(Method::bind(
                    function,
                    Rc::clone(instance),
                    name == "init",
                )))
            })
        }
    }

    pub(crate) fn set(&mut self, name: &str, value: Value) -> Value {
        self.fields.insert(String::from(name), value.clone());
        value
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Instance {}.class>", self.class.name())
    }
}
