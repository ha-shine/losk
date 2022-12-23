use crate::ast::{Expr, Stmt};
use crate::errors::LoskError;
use crate::parser::StmtStream;
use crate::token::{Literal, Token, Type};
use std::io::{stdout, Write};

struct Interpreter<T>
where
    T: Write,
{
    stdout: T,
}

impl<T> Interpreter<T>
where
    T: Write,
{
    pub fn new(stdout: T) -> Self {
        Interpreter { stdout }
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
                todo!()
            }
            Stmt::While {
                condition,
                body,
                token,
            } => {
                todo!()
            }
            Stmt::Print { expression } => {
                self.interpret_print_stmt(expression)?;
            }
            Stmt::Return { keyword, value } => {
                todo!()
            }
            Stmt::Var { name, init } => {
                todo!()
            }
        };

        Ok(())
    }

    fn interpret_print_stmt(&mut self, expr: &Expr) -> Result<(), LoskError> {
        let value = self.interpret_expr(expr)?;

        // TODO: Write returns an error, this should bubble up to the caller
        writeln!(self.stdout, "{}", value);
        Ok(())
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal, LoskError> {
        match expr {
            Expr::Assign { .. } => {
                todo!()
            }
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
            Expr::Unary { .. } => {
                todo!()
            }
            Expr::Variable { .. } => {
                todo!()
            }
        }
    }

    fn interpret_binary_expr(
        &mut self,
        left: &Expr,
        token: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;

        match token.ty {
            Type::Minus => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left - right)),
                _ => Err(LoskError::runtime_error(token, "Operands must be numbers.")),
            },
            Type::Plus => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Str(left + &right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Num(left + right)),
                _ => Err(LoskError::runtime_error(
                    token,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Slash => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left / right)),
                _ => Err(LoskError::runtime_error(token, "Operands must be numbers.")),
            },
            Type::Star => match (left, right) {
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::from(left * right)),
                _ => Err(LoskError::runtime_error(token, "Operands must be numbers.")),
            },
            Type::Greater => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left > right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left > right)),
                _ => Err(LoskError::runtime_error(
                    token,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::GreaterEqual => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left >= right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left >= right)),
                _ => Err(LoskError::runtime_error(
                    token,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::Less => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left < right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left < right)),
                _ => Err(LoskError::runtime_error(
                    token,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::LessEqual => match (left, right) {
                (Literal::Str(left), Literal::Str(right)) => Ok(Literal::Bool(left <= right)),
                (Literal::Num(left), Literal::Num(right)) => Ok(Literal::Bool(left <= right)),
                _ => Err(LoskError::runtime_error(
                    token,
                    "Operands must be either strings or numbers.",
                )),
            },
            Type::EqualEqual => Ok(Literal::Bool(left == right)),
            Type::BangEqual => Ok(Literal::Bool(left != right)),
            _ => Err(LoskError::runtime_error(token, "Invalid operator.")),
        }
    }

    fn interpret_logical_expr(
        &mut self,
        left: &Expr,
        token: &Token,
        right: &Expr,
    ) -> Result<Literal, LoskError> {
        let left = match self.interpret_expr(left)? {
            Literal::Bool(val) => val,
            _ => {
                return Err(LoskError::runtime_error(
                    token,
                    "Operand must be boolean expression.",
                ))
            }
        };

        // These expression needs to be carefully executed because of their logical nature
        // to avoid executing unnecessary expressions.
        // If token is "or", execution of right is only necessary if left is false
        // If token is "and", execution of right is only necessary if left is true
        // Otherwise, the value is already known and it can be returned immediately.
        if token.ty == Type::Or {
            if left {
                return Ok(Literal::from(true));
            }
        } else if !left {
            return Ok(Literal::from(false));
        }

        match self.interpret_expr(right)? {
            val @ Literal::Bool(_) => Ok(val),
            _ => Err(LoskError::runtime_error(
                token,
                "Operand must be boolean expression.",
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::Interpreter;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    #[test]
    fn test_expression_statements() {
        let tests = [
            ("print (1 + 2) * 5;", "15\n"),
            ("print false or true;", "true\n"),
        ];

        for (src, expected) in tests {
            let mut scanner = Scanner::new(src);
            let tokens = scanner.scan_tokens().unwrap();

            let mut parser = Parser::new(&tokens);
            let mut output: Vec<u8> = Vec::new();
            let mut interpreter = Interpreter::new(&mut output);
            interpreter.interpret(&parser.parse().unwrap()).unwrap();
            assert_eq!(expected.as_bytes(), &output);
        }
    }
}
