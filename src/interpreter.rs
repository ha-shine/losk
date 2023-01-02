use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, ExprVisitor, Stmt, StmtVisitor};
use crate::callable::{BoxedFunction, Class, Function, Native};
use crate::env::Environment;
use crate::errors::LoskError;
use crate::resolver::ResolvedStmts;
use crate::token::{Literal, Token, Type};

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    env: Rc<RefCell<Environment>>,
    locals: HashMap<Token, usize>,
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
            writeln!(RefCell::borrow_mut(&clock_out), "{:?}\n", since_epoch).unwrap();
            Ok(Literal::Nil)
        });
        let clock_callable = Native::new(clock, String::from("clock"), 0);
        RefCell::borrow_mut(&globals).define("clock", Literal::Callable(Rc::new(clock_callable)));

        Interpreter {
            env: globals.clone(),
            globals,
            locals: HashMap::new(),
            stdout,
        }
    }

    pub fn interpret(&mut self, resolved: &ResolvedStmts) -> Result<(), LoskError> {
        for stmt in &resolved.0 {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub(crate) fn execute_block_with_env(
        &mut self,
        stmts: &[Stmt],
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), LoskError> {
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
        self.locals.insert(token.clone(), depth);
    }

    fn lookup_variable(&self, token: &Token) -> Option<Literal> {
        match self.locals.get(token) {
            None => self.globals.borrow().get(&token.lexeme),
            Some(dist) => RefCell::borrow(&self.env).get_at(*dist, &token.lexeme),
        }
    }
}

impl ExprVisitor for Interpreter {
    type Item = Literal;

    fn visit_assign(
        &mut self,
        expr: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Literal, LoskError> {
        let value = self.visit_expr(value)?;

        match self.locals.get(name) {
            Some(dist) => {
                RefCell::borrow_mut(&self.env)
                    .assign_at(*dist, &name.lexeme, value.clone())
                    .unwrap();
            }
            None => {
                RefCell::borrow_mut(&self.env)
                    .assign(&name.lexeme, value.clone())
                    .unwrap();
            }
        }

        Ok(value)
    }

    fn visit_binary(
        &mut self,
        expr: &Expr,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match operator.ty {
            Type::Minus => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left - right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be numbers.",
                )),
            },
            Type::Plus => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => {
                    Ok(Literal::from(String::from(left.as_str()) + &right))
                }
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Num(left + right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Slash => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left / right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be numbers.",
                )),
            },
            Type::Star => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left * right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be numbers.",
                )),
            },
            Type::Greater => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left > right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left > right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::GreaterEqual => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left >= right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left >= right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Less => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left < right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left < right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::LessEqual => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left <= right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left <= right)),
                _ => Err(LoskError::runtime_error(
                    operator,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::EqualEqual => Ok(Literal::Bool(left == right)),
            Type::BangEqual => Ok(Literal::Bool(left != right)),
            _ => Err(LoskError::runtime_error(operator, "Invalid operator.")),
        }
    }

    fn visit_call(
        &mut self,
        expr: &Expr,
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Literal, LoskError> {
        let callee = self.visit_expr(callee)?;
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.visit_expr(arg)?);
        }

        match callee {
            Literal::Callable(func) => {
                if func.arity() == evaluated_args.len() {
                    func.execute(self, &evaluated_args)
                } else {
                    Err(LoskError::runtime_error(
                        paren,
                        &format!(
                            "Expected {} arguments but got {}.",
                            func.arity(),
                            evaluated_args.len()
                        ),
                    ))
                }
            }
            _ => Err(LoskError::runtime_error(
                paren,
                "Can only call functions and classes",
            )),
        }
    }

    fn visit_get(
        &mut self,
        expr: &Expr,
        object: &Expr,
        name: &Token,
    ) -> Result<Self::Item, LoskError> {
        if let Literal::Instance(instance) = self.visit_expr(object)? {
            match RefCell::borrow(&instance).get(&name.lexeme) {
                Some(val) => Ok(val),
                None => Err(LoskError::runtime_error(
                    name,
                    &format!("Object has no property with name '{}'.", name.lexeme),
                )),
            }
        } else {
            Err(LoskError::runtime_error(
                name,
                "Only instances have properties.",
            ))
        }
    }

    fn visit_set(
        &mut self,
        _: &Expr,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError> {
        match self.visit_expr(object)? {
            Literal::Instance(instance) => {
                let value = self.visit_expr(value)?;
                Ok(RefCell::borrow_mut(&instance).set(&name.lexeme, value))
            }
            _ => Err(LoskError::runtime_error(
                name,
                "Only instances have fields.",
            )),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)
    }

    fn visit_literal(&mut self, expr: &Expr, value: &Literal) -> Result<Self::Item, LoskError> {
        Ok(value.clone())
    }

    fn visit_logical(
        &mut self,
        expr: &Expr,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = match self.visit_expr(left)? {
            Literal::Bool(val) => val,
            _ => {
                return Err(LoskError::runtime_error(
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
                return Ok(Literal::from(true));
            }
        } else if !left {
            return Ok(Literal::from(false));
        }

        match self.visit_expr(right)? {
            val @ Literal::Bool(_) => Ok(val),
            _ => Err(LoskError::runtime_error(
                operator,
                "Operand must be boolean expression.",
            )),
        }
    }

    fn visit_unary(
        &mut self,
        expr: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let right = self.visit_expr(right)?;
        match (operator.ty, right) {
            (Type::Minus, Literal::Num(val)) => Ok(Literal::from(-val)),
            (Type::Bang, Literal::Bool(val)) => Ok(Literal::from(!val)),
            _ => Err(LoskError::runtime_error(
                operator,
                "Invalid types for unary operators.",
            )),
        }
    }

    fn visit_variable(&mut self, expr: &Expr, name: &Token) -> Result<Literal, LoskError> {
        match self.lookup_variable(name) {
            None => Err(LoskError::runtime_error(
                name,
                &format!("Use of undefined variable '{}'.", name.lexeme),
            )),
            Some(value) => Ok(value),
        }
    }
}

impl StmtVisitor for Interpreter {
    type Item = ();

    fn visit_block(&mut self, expr: &Stmt, statements: &[Stmt]) -> Result<(), LoskError> {
        let env = Rc::new(RefCell::new(Environment::with(self.env.clone())));
        self.execute_block_with_env(statements, env)
    }

    fn visit_expression(
        &mut self,
        expr: &Stmt,
        expression: &Expr,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)?;
        Ok(())
    }

    fn visit_function(
        &mut self,
        expr: &Stmt,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, LoskError> {
        let boxed = Rc::new(Function::new(self.env.clone(), name, params, body));
        let callable = Literal::Callable(boxed);
        RefCell::borrow_mut(&self.env).define(&name.lexeme, callable);
        Ok(())
    }

    fn visit_class(
        &mut self,
        expr: &Stmt,
        name: &Token,
        methods: &[Stmt],
    ) -> Result<Self::Item, LoskError> {
        RefCell::borrow_mut(&self.env).define(&name.lexeme, Literal::Nil);
        let class = Class::new(&name.lexeme);
        if let Err(_) =
            RefCell::borrow_mut(&self.env).assign(&name.lexeme, Literal::Callable(class))
        {
            Err(LoskError::runtime_error(name, "Undefined variable."))
        } else {
            Ok(())
        }
    }

    fn visit_if(
        &mut self,
        expr: &Stmt,
        expression: &Expr,
        token: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<(), LoskError> {
        let value = self.visit_expr(expression)?;
        match value {
            Literal::Bool(true) => self.visit_stmt(then_branch),
            Literal::Bool(false) => self.visit_stmt(else_branch),
            _ => Err(LoskError::runtime_error(
                token,
                "If condition must be a boolean expression.",
            )),
        }
    }

    fn visit_while(
        &mut self,
        expr: &Stmt,
        condition: &Expr,
        body: &Stmt,
        token: &Token,
    ) -> Result<(), LoskError> {
        loop {
            match self.visit_expr(condition) {
                Ok(Literal::Bool(true)) => self.visit_stmt(body)?,
                Ok(Literal::Bool(false)) => return Ok(()),
                Err(err) => return Err(err),
                _ => {
                    return Err(LoskError::runtime_error(
                        token,
                        "While condition must be a boolean expression.",
                    ))
                }
            }
        }
    }

    fn visit_print(&mut self, expr: &Stmt, expression: &Expr) -> Result<(), LoskError> {
        let value = self.visit_expr(expression)?;
        writeln!(RefCell::borrow_mut(&self.stdout), "{}", value).unwrap();
        Ok(())
    }

    fn visit_return(
        &mut self,
        expr: &Stmt,
        keyword: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError> {
        let value = self.visit_expr(value)?;
        Err(LoskError::return_value(value))
    }

    fn visit_var(&mut self, expr: &Stmt, name: &Token, expression: &Expr) -> Result<(), LoskError> {
        let value = self.visit_expr(expression)?;
        RefCell::borrow_mut(&self.env).define(&name.lexeme, value);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str;

    use crate::errors::LoskError;
    use crate::interpreter::Interpreter;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::scanner::Scanner;

    fn test_statements(src: &str, out: Option<&str>, err: Option<&str>) {
        println!("Testing source:\n{}", src);

        let mut scanner = Scanner::new(src);
        let tokens = scanner.scan_tokens().unwrap();

        let mut parser = Parser::new(&tokens);
        let output: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));

        let mut interpreter = Interpreter::new(output.clone());
        let mut resolver = Resolver::new(&mut interpreter);
        let parsed = parser.parse().unwrap();
        let resolved = resolver.resolve(parsed);
        let result = interpreter.interpret(&resolved.unwrap());

        match (result, err) {
            (Err(LoskError::RuntimeError { msg, .. }), Some(err)) => assert_eq!(err, msg),
            (Err(LoskError::RuntimeError { msg, .. }), None) => {
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
    fn test_lox_programs() {
        let tests = [
            // binary and grouping expressions, with precedence
            ("print (1 + 2) * 5 + 2;", "17\n"),
            ("print \"hello \" + \"world\";", "hello world\n"),
            // logical expressions
            ("print false or true;", "true\n"),
            // unary expressions
            ("print !true;", "false\n"),
            ("print -10.5;", "-10.5\n"),
            // variable assignment
            (
                "var foo = \"bar\";\
                 print foo;",
                "bar\n",
            ),
            // printing function
            ("print clock;", "<Function clock>\n"),
            (
                include_str!("../data/if_else.lox"),
                include_str!("../data/if_else.lox.expected"),
            ),
            (
                include_str!("../data/while.lox"),
                include_str!("../data/while.lox.expected"),
            ),
            (
                include_str!("../data/for.lox"),
                include_str!("../data/for.lox.expected"),
            ),
            (
                include_str!("../data/binding.lox"),
                include_str!("../data/binding.lox.expected"),
            ),
            (
                include_str!("../data/fib.lox"),
                include_str!("../data/fib.lox.expected"),
            ),
            (
                include_str!("../data/make_counter.lox"),
                include_str!("../data/make_counter.lox.expected"),
            ),
            (
                include_str!("../data/class.lox"),
                include_str!("../data/class.lox.expected"),
            ),
        ];

        for (src, expected) in tests {
            test_statements(src, Some(expected), None);
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
            test_statements(src, None, Some(expected));
        }
    }

    #[test]
    fn test_unary_expression_with_wrong_types() {
        let tests = [
            ("-false;", "Invalid types for unary operators."),
            ("!10;", "Invalid types for unary operators."),
        ];

        for (src, expected) in tests {
            test_statements(src, None, Some(expected));
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
            test_statements(src, None, Some(expected));
        }
    }

    #[test]
    fn test_native_functions() {
        let tests = ["clock();"];
        for test in tests {
            test_statements(test, None, None);
        }
    }

    #[test]
    fn test_native_functions_with_wrong_argument_number() {
        test_statements("clock(1);", None, Some("Expected 0 arguments but got 1."))
    }
}
