use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use losk_core::{Token, TokenIndex, Type};

use crate::ast::{Expr, ExprVisitor, Stmt, StmtVisitor};
use crate::callable::{BoxedFunction, CallableType, Class, Function, Instance, Method, Native};
use crate::env::Environment;
use crate::error::Error;
use crate::resolver::ResolvedStmts;
use crate::value::Value;

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    env: Rc<RefCell<Environment>>,
    locals: HashMap<TokenIndex, usize>,
    stdout: Rc<RefCell<dyn Write>>,
}

#[allow(dead_code)]
impl Interpreter {
    pub fn new(stdout: Rc<RefCell<dyn Write>>) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));

        let clock_out = stdout.clone();
        let clock: BoxedFunction = Box::new(move |_| {
            let start = SystemTime::now();
            let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();
            writeln!(clock_out.borrow_mut(), "{:?}\n", since_epoch).unwrap();
            Ok(Value::Nil)
        });
        let clock_callable = Native::new(clock, String::from("clock"), 0);
        globals
            .borrow_mut()
            .define("clock", Value::Callable(Rc::new(clock_callable)));

        Interpreter {
            env: globals.clone(),
            globals,
            locals: HashMap::new(),
            stdout,
        }
    }

    pub fn interpret(&mut self, resolved: &ResolvedStmts) -> Result<(), Error> {
        for stmt in &resolved.0 {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub(crate) fn execute_block_with_env(
        &mut self,
        stmts: &[Stmt],
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), Error> {
        let current = self.env.clone();
        self.env = env;
        for stmt in stmts {
            if let err @ Err(_) = self.visit_stmt(stmt) {
                self.env = current;
                return err;
            }
        }
        self.env = current;
        Ok(())
    }

    pub(crate) fn resolve(&mut self, token: &Token, depth: usize) {
        self.locals.insert(token.idx, depth);
    }

    fn lookup_variable(&self, token: &Token) -> Option<Value> {
        match self.locals.get(&token.idx) {
            None => self.globals.borrow().get(&token.lexeme),
            Some(dist) => self.env.borrow().get_at(*dist, &token.lexeme),
        }
    }
}

impl ExprVisitor for Interpreter {
    type Item = Value;

    fn visit_assign(&mut self, name: &Token, value: &Expr) -> Result<Value, Error> {
        let value = self.visit_expr(value)?;

        match self.locals.get(&name.idx) {
            Some(dist) => {
                self.env
                    .borrow_mut()
                    .assign_at(*dist, &name.lexeme, value.clone())
                    .unwrap();
            }
            None => {
                self.env
                    .borrow_mut()
                    .assign(&name.lexeme, value.clone())
                    .unwrap();
            }
        }

        Ok(value)
    }

    fn visit_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Value, Error> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match operator.ty {
            Type::Minus => match (left, right) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::from(left - right)),
                _ => Err(Error::runtime_error(operator, "Operands must be numbers.")),
            },
            Type::Plus => match (left, right) {
                (Value::Str(left), Value::Str(right)) => {
                    Ok(Value::from(String::from(left.as_str()) + &right))
                }
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left + right)),
                _ => Err(Error::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Slash => match (left, right) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::from(left / right)),
                _ => Err(Error::runtime_error(operator, "Operands must be numbers.")),
            },
            Type::Star => match (left, right) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::from(left * right)),
                _ => Err(Error::runtime_error(operator, "Operands must be numbers.")),
            },
            Type::Greater => match (left, right) {
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left > right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left > right)),
                _ => Err(Error::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::GreaterEqual => match (left, right) {
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left >= right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left >= right)),
                _ => Err(Error::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Less => match (left, right) {
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left < right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left < right)),
                _ => Err(Error::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::LessEqual => match (left, right) {
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left <= right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left <= right)),
                _ => Err(Error::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::EqualEqual => Ok(Value::Bool(left == right)),
            Type::BangEqual => Ok(Value::Bool(left != right)),
            _ => Err(Error::runtime_error(operator, "Invalid operator.")),
        }
    }

    fn visit_call(&mut self, callee: &Expr, paren: &Token, args: &[Expr]) -> Result<Value, Error> {
        let callee = self.visit_expr(callee)?;
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.visit_expr(arg)?);
        }

        match callee {
            Value::Callable(func) => {
                if func.arity() == evaluated_args.len() {
                    func.execute(self, &evaluated_args)
                } else {
                    Err(Error::runtime_error(
                        paren,
                        &format!(
                            "Expected {} arguments but got {}.",
                            func.arity(),
                            evaluated_args.len()
                        ),
                    ))
                }
            }
            _ => Err(Error::runtime_error(
                paren,
                "Can only call functions and classes",
            )),
        }
    }

    fn visit_get(&mut self, object: &Expr, name: &Token) -> Result<Self::Item, Error> {
        if let Value::Instance(instance) = self.visit_expr(object)? {
            match Instance::get(&instance, &name.lexeme) {
                Some(val) => Ok(val),
                None => Err(Error::runtime_error(
                    name,
                    &format!("Object has no property with name '{}'.", name.lexeme),
                )),
            }
        } else {
            Err(Error::runtime_error(
                name,
                "Only instances have properties.",
            ))
        }
    }

    fn visit_set(
        &mut self,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, Error> {
        match self.visit_expr(object)? {
            Value::Instance(instance) => {
                let value = self.visit_expr(value)?;
                Ok(instance.borrow_mut().set(&name.lexeme, value))
            }
            _ => Err(Error::runtime_error(name, "Only instances have fields.")),
        }
    }

    fn visit_this(&mut self, keyword: &Token) -> Result<Self::Item, Error> {
        match self.lookup_variable(keyword) {
            Some(val) => Ok(val),
            None => Err(Error::runtime_error(
                keyword,
                "'this' not bound to anything.",
            )),
        }
    }

    fn visit_super(&mut self, keyword: &Token, method: &Token) -> Result<Self::Item, Error> {
        let dist = self.locals.get(&keyword.idx).unwrap();
        let superclass = self.env.borrow().get_at(*dist, "super").unwrap();
        let this = self.env.borrow().get_at(*dist - 1, "this").unwrap();

        match (superclass, this) {
            (Value::Callable(callable), Value::Instance(instance)) => {
                let supe = callable.as_class().unwrap();
                let fun = match supe.find_method(&method.lexeme) {
                    Some(method) => method,
                    _ => {
                        return Err(Error::runtime_error(
                            method,
                            &format!("Unknown method super.{}", method.lexeme),
                        ))
                    }
                };

                Ok(Value::Callable(Rc::new(Method::bind(
                    fun,
                    instance,
                    method.lexeme == "init",
                ))))
            }
            (supe, this) => panic!(
                "Unexpected literal types for super({:?}) and this({:?})",
                supe, this
            ),
        }
    }

    fn visit_grouping(&mut self, expression: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(expression)
    }

    fn visit_literal(&mut self, value: &Value) -> Result<Self::Item, Error> {
        Ok(value.clone())
    }

    fn visit_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Value, Error> {
        let left = match self.visit_expr(left)? {
            Value::Bool(val) => val,
            _ => {
                return Err(Error::runtime_error(
                    operator,
                    "Operand must be boolean expression.",
                ))
            }
        };

        // These expression needs to be carefully executed because of their logical nature
        // to avoid executing unnecessary expressions.
        // If token is "or", execution of right is only necessary if left is false
        // If token is "and", execution of right is only necessary if left is true
        // Otherwise, the value is already known and it can be returned immediately.
        if operator.ty == Type::Or {
            if left {
                return Ok(Value::from(true));
            }
        } else if !left {
            return Ok(Value::from(false));
        }

        match self.visit_expr(right)? {
            val @ Value::Bool(_) => Ok(val),
            _ => Err(Error::runtime_error(
                operator,
                "Operand must be boolean expression.",
            )),
        }
    }

    fn visit_unary(&mut self, operator: &Token, right: &Expr) -> Result<Value, Error> {
        let right = self.visit_expr(right)?;
        match (operator.ty, right) {
            (Type::Minus, Value::Num(val)) => Ok(Value::from(-val)),
            (Type::Bang, Value::Bool(val)) => Ok(Value::from(!val)),
            _ => Err(Error::runtime_error(
                operator,
                "Invalid types for unary operators.",
            )),
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Value, Error> {
        match self.lookup_variable(name) {
            None => Err(Error::runtime_error(
                name,
                &format!("Use of undefined variable '{}'.", name.lexeme),
            )),
            Some(value) => Ok(value),
        }
    }

    fn visit_empty(&mut self) -> Result<Self::Item, Error> {
        Ok(Value::Nil)
    }
}

impl StmtVisitor for Interpreter {
    type Item = ();

    fn visit_block(&mut self, statements: &[Stmt]) -> Result<(), Error> {
        let env = Rc::new(RefCell::new(Environment::with(self.env.clone())));
        self.execute_block_with_env(statements, env)
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(expression)?;
        Ok(())
    }

    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, Error> {
        let boxed = Rc::new(Function::new(self.env.clone(), name, params, body));
        let callable = Value::Callable(boxed);
        self.env.borrow_mut().define(&name.lexeme, callable);
        Ok(())
    }

    fn visit_class(
        &mut self,
        name: &Token,
        superclass: &Expr,
        methods: &[Stmt],
    ) -> Result<Self::Item, Error> {
        self.env.borrow_mut().define(&name.lexeme, Value::Nil);
        let closure = Rc::new(RefCell::new(Environment::with(Rc::clone(&self.env))));
        let superclass = match self.visit_expr(superclass)? {
            Value::Nil => None,
            Value::Callable(callable) if callable.ty() == CallableType::Class => {
                closure
                    .borrow_mut()
                    .define("super", Value::Callable(Rc::clone(&callable)));
                callable.as_class()
            }
            _ => return Err(Error::runtime_error(name, "Superclass must be a class")),
        };

        let mut methods_map = HashMap::new();
        for method in methods {
            if let Stmt::Function { name, params, body } = method {
                methods_map.insert(
                    name.lexeme.clone(),
                    Rc::new(Function::new(Rc::clone(&closure), name, params, body)),
                );
            } else {
                panic!(
                    "Unexpected statement '{:?}' found in class body, expecting a method.",
                    method
                )
            }
        }

        let class = Class::new(&name.lexeme, superclass, methods_map);
        if self
            .env
            .borrow_mut()
            .assign(&name.lexeme, Value::Callable(class))
            .is_err()
        {
            Err(Error::runtime_error(name, "Undefined variable."))
        } else {
            Ok(())
        }
    }

    fn visit_if(
        &mut self,
        expression: &Expr,
        token: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<(), Error> {
        let value = self.visit_expr(expression)?;
        match value {
            Value::Bool(true) => self.visit_stmt(then_branch),
            Value::Bool(false) => self.visit_stmt(else_branch),
            _ => Err(Error::runtime_error(
                token,
                "If condition must be a boolean expression.",
            )),
        }
    }

    fn visit_while(&mut self, condition: &Expr, body: &Stmt, token: &Token) -> Result<(), Error> {
        loop {
            match self.visit_expr(condition) {
                Ok(Value::Bool(true)) => self.visit_stmt(body)?,
                Ok(Value::Bool(false)) => return Ok(()),
                Err(err) => return Err(err),
                _ => {
                    return Err(Error::runtime_error(
                        token,
                        "While condition must be a boolean expression.",
                    ))
                }
            }
        }
    }

    fn visit_print(&mut self, expression: &Expr) -> Result<(), Error> {
        let value = self.visit_expr(expression)?;
        writeln!(self.stdout.borrow_mut(), "{}", value).unwrap();
        Ok(())
    }

    fn visit_return(&mut self, _: &Token, value: &Expr) -> Result<Self::Item, Error> {
        let value = self.visit_expr(value)?;
        Err(Error::return_value(value))
    }

    fn visit_var(&mut self, name: &Token, expression: &Expr) -> Result<(), Error> {
        let value = self.visit_expr(expression)?;
        self.env.borrow_mut().define(&name.lexeme, value);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str;

    use losk_core::Scanner;

    use crate::error::Error;
    use crate::interpreter::Interpreter;
    use crate::parser::Parser;
    use crate::resolver::Resolver;

    fn test_program(src: &str, out: Option<&str>, err: Option<&str>) {
        println!("Testing source:\n{}", src);

        let mut scanner = Scanner::new();
        let mut parser = Parser::new(scanner.scan_tokens(src));
        let output: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));

        let mut interpreter = Interpreter::new(output.clone());
        let mut resolver = Resolver::new(&mut interpreter);
        let parsed = parser.parse().unwrap();

        // Some of the test cases test the resolver error. So if there's an error from resolver,
        // ensure the test messages match and end the program.
        let resolved = match (resolver.resolve(parsed), err) {
            (Err(Error::RuntimeError { msg, .. }), Some(expected)) => {
                assert_eq!(expected, msg);
                return;
            }
            (Err(rerr), _) => {
                panic!("Unexpected error from resolver '{:?}'.", rerr);
            }
            (Ok(val), _) => val,
        };

        let result = interpreter.interpret(&resolved);
        match (result, err) {
            (Err(Error::RuntimeError { msg, .. }), Some(err)) => assert_eq!(err, msg),
            (Err(Error::RuntimeError { msg, .. }), None) => {
                panic!("Not expecting any error, found '{}'", msg)
            }
            (Ok(_), Some(err)) => panic!("Expecting an error '{}', found none.", err),
            _ => {}
        }

        if let Some(out) = out {
            assert_eq!(str::from_utf8(&output.borrow()).unwrap(), out);
        }
        println!("\n\n");
    }

    #[test]
    fn test_programs() {
        let tests = [
            (
                include_str!("../../data/print_expression.lox"),
                include_str!("../../data/print_expression.lox.expected"),
            ),
            (
                include_str!("../../data/var_assignment.lox"),
                include_str!("../../data/var_assignment.lox.expected"),
            ),
            (
                include_str!("../../data/block.lox"),
                include_str!("../../data/block.lox.expected"),
            ),
            // printing function
            ("print clock;", "<Function clock>\n"),
            (
                include_str!("../../data/if_else.lox"),
                include_str!("../../data/if_else.lox.expected"),
            ),
            (
                include_str!("../../data/while.lox"),
                include_str!("../../data/while.lox.expected"),
            ),
            (
                include_str!("../../data/for.lox"),
                include_str!("../../data/for.lox.expected"),
            ),
            (
                include_str!("../../data/print_function.lox"),
                include_str!("../../data/print_function.lox.expected"),
            ),
            (
                include_str!("../../data/fun_no_return.lox"),
                include_str!("../../data/fun_no_return.lox.expected"),
            ),
            (
                include_str!("../../data/binding.lox"),
                include_str!("../../data/binding.lox.expected"),
            ),
            (
                include_str!("../../data/fib.lox"),
                include_str!("../../data/fib.lox.expected"),
            ),
            (
                include_str!("../../data/make_counter.lox"),
                include_str!("../../data/make_counter.lox.expected"),
            ),
            (
                include_str!("../../data/class.lox"),
                include_str!("../../data/class.lox.expected"),
            ),
            (
                include_str!("../../data/inheritance.lox"),
                include_str!("../../data/inheritance.lox.expected"),
            ),
        ];

        for (src, expected) in tests {
            test_program(src, Some(expected), None);
        }
    }

    #[test]
    fn test_binary_expression_with_wrong_types() {
        let tests = [
            (
                "print 1 + false;",
                "Operands must be either strings or numbers.",
            ),
            (
                "print true + false;",
                "Operands must be either strings or numbers.",
            ),
            (
                "print true + false;",
                "Operands must be either strings or numbers.",
            ),
            (
                "print \"hello\" + 10;",
                "Operands must be either strings or numbers.",
            ),
        ];

        for (src, expected) in tests {
            test_program(src, None, Some(expected));
        }
    }

    #[test]
    fn test_unary_expression_with_wrong_types() {
        let tests = [
            ("-false;", "Invalid types for unary operators."),
            ("!10;", "Invalid types for unary operators."),
        ];

        for (src, expected) in tests {
            test_program(src, None, Some(expected));
        }
    }

    #[test]
    fn test_use_of_undefined_variable() {
        let tests = [(
            "var foo = \"bar\";\
                 print bar;",
            "Use of undefined variable 'bar'.",
        )];

        for (src, expected) in tests {
            test_program(src, None, Some(expected));
        }
    }

    #[test]
    fn test_native_functions() {
        let tests = ["clock();"];
        for test in tests {
            test_program(test, None, None);
        }
    }

    #[test]
    fn test_native_functions_with_wrong_argument_number() {
        test_program("clock(1);", None, Some("Expected 0 arguments but got 1."))
    }

    #[test]
    fn test_this_cant_be_used_outside_of_a_class() {
        test_program(
            "print this;",
            None,
            Some("Can't use 'this' outside of a class."),
        );
    }

    #[test]
    fn test_cant_return_from_initializer() {
        test_program(
            "class Person { init() { return 1; } }",
            None,
            Some("Can't return a value from an initializer."),
        );
    }

    #[test]
    fn test_can_return_early_from_initializer() {
        test_program("class Person { init() { return; } }", None, None);
    }

    #[test]
    fn test_non_existent_super_method_throws_error() {
        test_program(
            "\
        class Person {}
        class Employee < Person {
            greet() { super.greet(); }
        }

        var emp = Employee();
        emp.greet();
        ",
            None,
            Some("Unknown method super.greet"),
        )
    }

    #[test]
    fn test_super_cant_be_used_outside_of_a_class() {
        test_program(
            "print super.cook();",
            None,
            Some("Can't use 'super' outside of a class."),
        );
    }

    #[test]
    fn test_super_cant_be_used_inside_a_class_with_no_superclass() {
        test_program(
            "class Person { greet() { super.cook(); } }",
            None,
            Some("Can't use 'super' in a class with no superclass."),
        );
    }
}
