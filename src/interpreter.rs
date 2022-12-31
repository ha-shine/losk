use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, ExprVisitor, Stmt, StmtVisitor};
use crate::callable::{BoxedFunction, NativeCallable};
use crate::env::Environment;
use crate::errors::LoskError;
use crate::resolver::ResolvedStmts;
use crate::token::{Literal, Token, Type};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
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
            Ok(Literal::Nil)
        });
        let clock_callable = NativeCallable::new(clock, String::from("clock"), 0);
        globals
            .borrow_mut()
            .define("clock", Literal::Callable(Rc::new(clock_callable)));

        let env = Rc::new(RefCell::new(Environment::with(globals)));

        Interpreter { env, stdout }
    }

    pub fn interpret(&mut self, resolved: &ResolvedStmts) -> Result<(), LoskError> {
        for stmt in &resolved.stmts {
            self.interpret_stmt(stmt)?;
        }
        Ok(())
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<(), LoskError> {
        match stmt {
            Stmt::Expression { expression } => self.visit_expression(expression),
            Stmt::Block { statements } => self.visit_block(statements),
            Stmt::Function { name, params, body } => self.visit_function(name, params, body),
            Stmt::Class { name, methods } => self.visit_class(name, methods),
            Stmt::If {
                expression,
                token,
                then_branch,
                else_branch,
            } => self.visit_if(expression, token, then_branch, else_branch),
            Stmt::While {
                condition,
                body,
                token,
            } => self.visit_while(condition, body, token),
            Stmt::Print { expression } => self.visit_print(expression),
            Stmt::Return { keyword, value } => self.visit_return(keyword, value),
            Stmt::Var { name, init } => self.visit_var(name, init),
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal, LoskError> {
        match expr {
            Expr::Assign { name, value } => self.visit_assign(name, value),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.visit_binary(left, operator, right),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.visit_call(callee, paren, args),
            Expr::Get { object, name } => self.visit_get(object, name),
            Expr::Grouping { expression } => self.visit_grouping(expression),
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_logical(left, operator, right),
            Expr::Unary { operator, right } => self.visit_unary(operator, right),
            Expr::Variable { name } => self.visit_variable(name),
        }
    }
}

impl ExprVisitor for Interpreter {
    type Item = Literal;

    // TODO: This will need to be modified when resolver is implemented since we need to assign to
    //   correct variable
    fn visit_assign(&mut self, name: &Token, value: &Expr) -> Result<Literal, LoskError> {
        let value = self.interpret_expr(value)?;
        match self.env.borrow_mut().assign(&name.lexeme, value.clone()) {
            Ok(_) => Ok(value),
            Err(_) => Err(LoskError::runtime_error(
                name,
                &format!("Undefined variable '{}'.", &name.lexeme),
            )),
        }
    }

    fn visit_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;

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
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Literal, LoskError> {
        let callee = self.interpret_expr(callee)?;
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.interpret_expr(arg)?);
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

    fn visit_get(&mut self, object: &Expr, name: &Token) -> Result<Self::Item, LoskError> {
        todo!()
    }

    fn visit_grouping(&mut self, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.interpret_expr(expression)
    }

    fn visit_literal(&mut self, value: &Literal) -> Result<Self::Item, LoskError> {
        Ok(value.clone())
    }

    fn visit_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = match self.interpret_expr(left)? {
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

        match self.interpret_expr(right)? {
            val @ Literal::Bool(_) => Ok(val),
            _ => Err(LoskError::runtime_error(
                operator,
                "Operand must be boolean expression.",
            )),
        }
    }

    fn visit_unary(&mut self, operator: &Token, right: &Expr) -> Result<Literal, LoskError> {
        let right = self.interpret_expr(right)?;
        match (operator.ty, right) {
            (Type::Minus, Literal::Num(val)) => Ok(Literal::from(-val)),
            (Type::Bang, Literal::Bool(val)) => Ok(Literal::from(!val)),
            _ => Err(LoskError::runtime_error(
                operator,
                "Invalid types for unary operators.",
            )),
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Literal, LoskError> {
        if let Some(value) = self.env.borrow_mut().get(&name.lexeme) {
            Ok(value)
        } else {
            Err(LoskError::runtime_error(
                name,
                &format!("Use of undefined variable '{}'.", name.lexeme),
            ))
        }
    }
}

impl StmtVisitor for Interpreter {
    type Item = ();

    fn visit_block(&mut self, statements: &[Stmt]) -> Result<(), LoskError> {
        let current = self.env.clone();
        self.env = Rc::new(RefCell::new(Environment::with(current.clone())));
        for stmt in statements {
            if let err @ Err(_) = self.interpret_stmt(stmt) {
                self.env = current;
                return err;
            }
        }
        self.env = current;
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.interpret_expr(expression)?;
        Ok(())
    }

    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, LoskError> {
        todo!()
    }

    fn visit_class(&mut self, name: &Token, methods: &[Stmt]) -> Result<Self::Item, LoskError> {
        todo!()
    }

    fn visit_if(
        &mut self,
        expression: &Expr,
        token: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<(), LoskError> {
        let value = self.interpret_expr(expression)?;
        match value {
            Literal::Bool(true) => self.interpret_stmt(then_branch),
            Literal::Bool(false) => self.interpret_stmt(else_branch),
            _ => Err(LoskError::runtime_error(
                token,
                "If condition must be a boolean expression.",
            )),
        }
    }

    fn visit_while(
        &mut self,
        condition: &Expr,
        body: &Stmt,
        token: &Token,
    ) -> Result<(), LoskError> {
        loop {
            match self.interpret_expr(condition) {
                Ok(Literal::Bool(true)) => self.interpret_stmt(body)?,
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

    fn visit_print(&mut self, expression: &Expr) -> Result<(), LoskError> {
        let value = self.interpret_expr(expression)?;
        writeln!(self.stdout.borrow_mut(), "{}", value).unwrap();
        Ok(())
    }

    fn visit_return(&mut self, keyword: &Token, value: &Expr) -> Result<Self::Item, LoskError> {
        todo!()
    }

    fn visit_var(&mut self, name: &Token, expression: &Expr) -> Result<(), LoskError> {
        let value = self.interpret_expr(expression)?;
        self.env.borrow_mut().define(&name.lexeme, value);
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
        let mut scanner = Scanner::new(src);
        let tokens = scanner.scan_tokens().unwrap();

        let mut parser = Parser::new(&tokens);
        let output: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));

        let mut interpreter = Interpreter::new(output.clone());
        let resolver = Resolver::new();
        let parsed = parser.parse().unwrap();
        let resolved = resolver.resolve(parsed);
        let result = interpreter.interpret(&resolved);

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
