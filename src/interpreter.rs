use std::io::Write;

use crate::ast::{Expr, Stmt};
use crate::env::Environment;
use crate::errors::LoskError;
use crate::parser::StmtStream;
use crate::token::{Literal, Token, Type};

struct Interpreter<'a, T>
where
    T: Write,
{
    stdout: T,
    env: Environment<'a, 'a>,
}

impl<'a, T> Interpreter<'a, T>
where
    T: Write,
{
    pub fn new(stdout: T) -> Self {
        Interpreter {
            stdout,
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &StmtStream) -> Result<(), LoskError> {
        self.interpret_stmt_vec(&stmts.0)
    }

    fn interpret_stmt_vec(&mut self, stmts: &Vec<Stmt>) -> Result<(), LoskError> {
        for stmt in stmts {
            self.interpret_stmt(stmt)?;
        }
        Ok(())
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<(), LoskError> {
        match stmt {
            Stmt::Expression { expression } => {
                self.interpret_expr(expression)?;
            }
            Stmt::Block { statements } => {
                self.interpret_stmt_vec(statements)?;
            }
            Stmt::Function { name, params, body } => {
                todo!()
            }
            Stmt::Class { name, methods } => {
                todo!()
            }
            Stmt::If {
                expression,
                token,
                then_branch,
                else_branch,
            } => {
                self.interpret_if_stmt(expression, token, then_branch, else_branch)?;
            }
            Stmt::While {
                condition,
                body,
                token,
            } => {
                self.interpret_while_stmt(condition, body, token)?;
            }
            Stmt::Print { expression } => {
                self.interpret_print_stmt(expression)?;
            }
            Stmt::Return { keyword, value } => {
                todo!()
            }
            Stmt::Var { name, init } => {
                self.interpret_var_stmt(name, init)?;
            }
        };

        Ok(())
    }

    fn interpret_if_stmt(
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

    fn interpret_while_stmt(
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

    fn interpret_print_stmt(&mut self, expression: &Expr) -> Result<(), LoskError> {
        let value = self.interpret_expr(expression)?;

        // TODO: Write returns an error, this should bubble up to the caller
        writeln!(self.stdout, "{}", value);
        Ok(())
    }

    fn interpret_var_stmt(&mut self, name: &Token, expression: &Expr) -> Result<(), LoskError> {
        let value = self.interpret_expr(expression)?;
        self.env.assign(name.lexeme.clone(), value);
        Ok(())
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal, LoskError> {
        match expr {
            Expr::Assign { name, value } => self.interpret_assign_expr(name, value),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.interpret_binary_expr(left, operator, right),
            Expr::Call { .. } => {
                todo!()
            }
            Expr::Get { .. } => {
                todo!()
            }
            Expr::Grouping { expression } => self.interpret_expr(expression),
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.interpret_logical_expr(left, operator, right),
            Expr::Unary { operator, right } => self.interpret_unary_expr(operator, right),
            Expr::Variable { name } => self.interpret_variable_expr(name),
        }
    }

    // TODO: This will need to be modified when resolver is implemented since we need to assign to
    //   correct variable
    fn interpret_assign_expr(&mut self, name: &Token, value: &Expr) -> Result<Literal, LoskError> {
        let value = self.interpret_expr(value)?;
        self.env.assign(name.lexeme.clone(), value.clone());
        Ok(value)
    }

    fn interpret_binary_expr(
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
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::from(left + &right)),
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

    fn interpret_logical_expr(
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

    fn interpret_unary_expr(
        &mut self,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
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

    fn interpret_variable_expr(&mut self, name: &Token) -> Result<Literal, LoskError> {
        if let Some(value) = self.env.get(&name.lexeme) {
            Ok(value.clone())
        } else {
            Err(LoskError::runtime_error(
                name,
                &format!("Use of undefined variable '{}'.", name.lexeme),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::LoskError;
    use std::str;

    use crate::interpreter::Interpreter;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn test_statements(src: &str, out: Option<&str>, err: Option<&str>) {
        let mut scanner = Scanner::new(src);
        let tokens = scanner.scan_tokens().unwrap();

        let mut parser = Parser::new(&tokens);
        let mut output: Vec<u8> = Vec::new();
        let mut interpreter = Interpreter::new(&mut output);
        let result = interpreter.interpret(&parser.parse().unwrap());

        if let Some(out) = out {
            assert_eq!(out, str::from_utf8(&output).unwrap());
        }

        if let Some(err) = err {
            if let Err(LoskError::RuntimeError { msg, .. }) = result {
                assert_eq!(&msg, err);
            } else {
                panic!()
            }
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
            (
                include_str!("../data/if_else.lox"),
                include_str!("../data/if_else.lox.expected"),
            ),
            (
                include_str!("../data/while.lox"),
                include_str!("../data/while.lox.expected"),
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
}
