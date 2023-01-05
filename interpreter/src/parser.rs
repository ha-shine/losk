use crate::ast::{Expr, Stmt};
use crate::error::Error;
use core::{Token, Type};
use std::rc::Rc;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

// A wrapper over vector of statements to not leak Stmt to public
#[derive(Debug, PartialEq)]
pub struct StmtStream(pub(crate) Vec<Stmt>);

// Helper alias for shorter return types
type ParserResult = Result<StmtStream, Vec<Error>>;
type BlockResult = Result<Vec<Stmt>, Error>;
type StmtResult = Result<Stmt, Error>;
type ExprResult = Result<Expr, Error>;

// Function kind to differentiate between normal functions and class methods during parsing
#[allow(dead_code)]
#[derive(Debug)]
enum FunctionKind {
    Function,
    Method,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut statements = Vec::new();
        let mut errs = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => errs.push(err),
            };
        }

        if errs.is_empty() {
            Ok(StmtStream(statements))
        } else {
            Err(errs)
        }
    }

    fn declaration(&mut self) -> StmtResult {
        let res = if self.match_one(Type::Class) {
            self.class_declaration()
        } else if self.match_one(Type::Fun) {
            self.function(FunctionKind::Function)
        } else if self.match_one(Type::Var) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if res.is_err() {
            self.synchronize();
        }

        res
    }

    fn class_declaration(&mut self) -> StmtResult {
        let name = self
            .consume(Type::Identifier, "Expect class name.")?
            .clone();

        let mut superclass = Expr::empty();
        if self.match_one(Type::Less) {
            self.consume(Type::Identifier, "Expect superclass name.")?;
            superclass = Expr::variable(self.previous().clone());
        }

        self.consume(Type::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while !self.check(Type::RightBrace) && !self.is_at_end() {
            methods.push(self.function(FunctionKind::Method)?);
        }
        self.consume(Type::RightBrace, "Expect '}' after class body.")?;
        Ok(Stmt::class(name, superclass, methods))
    }

    fn function(&mut self, kind: FunctionKind) -> StmtResult {
        let name = self
            .consume(Type::Identifier, &format!("Expect {:?} name.", kind))?
            .clone();
        self.consume(
            Type::LeftParen,
            &format!("Expect '(' after {:?} name.", kind),
        )?;

        let mut params = Vec::new();
        if !self.check(Type::RightParen) {
            loop {
                if params.len() > 255 {
                    return Err(Error::parser_error(
                        self.peek(),
                        "Can't have more than 255 parameters.",
                    ));
                }

                params.push(
                    self.consume(Type::Identifier, "Expect parameter name.")?
                        .clone(),
                );
                if !self.match_one(Type::Comma) {
                    break;
                }
            }
        }

        self.consume(Type::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            Type::LeftBrace,
            &format!("Expect '{{' before {:?} body.", kind),
        )?;

        let body = self.block()?;
        Ok(Stmt::function(name, params, body))
    }

    fn var_declaration(&mut self) -> StmtResult {
        let name = self
            .consume(Type::Identifier, "Expect variable name.")?
            .clone();
        let mut init = Expr::empty();
        if self.match_one(Type::Equal) {
            init = self.expression()?;
        }

        self.consume(Type::SemiColon, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var {
            name,
            init: Rc::new(init),
        })
    }

    fn statement(&mut self) -> StmtResult {
        if self.match_one(Type::If) {
            self.if_statement()
        } else if self.match_one(Type::Print) {
            self.print_statement()
        } else if self.match_one(Type::Return) {
            self.return_statement()
        } else if self.match_one(Type::While) {
            self.while_statement()
        } else if self.match_one(Type::For) {
            self.for_statement()
        } else if self.match_one(Type::LeftBrace) {
            Ok(Stmt::Block {
                statements: self.block()?,
            })
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> BlockResult {
        let mut stmts = Vec::new();
        while !self.check(Type::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(Type::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(Type::SemiColon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression {
            expression: Rc::new(expr),
        })
    }

    fn if_statement(&mut self) -> StmtResult {
        let token = self.previous().clone();
        self.consume(Type::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(Type::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let mut else_branch = Stmt::block(Vec::new());
        if self.match_one(Type::Else) {
            else_branch = self.statement()?;
        }

        Ok(Stmt::if_(condition, token, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> StmtResult {
        let token = self.previous().clone();
        self.consume(Type::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(Type::RightParen, "Expect ')' after while condition.")?;
        let body = self.statement()?;
        Ok(Stmt::while_(condition, body, token))
    }

    fn for_statement(&mut self) -> StmtResult {
        let token = self.previous().clone();
        self.consume(Type::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_one(Type::SemiColon) {
            Stmt::block(Vec::new())
        } else if self.match_one(Type::Var) {
            self.var_declaration()?
        } else {
            self.expression_statement()?
        };

        let condition = if !self.check(Type::SemiColon) {
            self.expression()?
        } else {
            Expr::empty()
        };
        self.consume(Type::SemiColon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(Type::RightParen) {
            self.expression()?
        } else {
            Expr::empty()
        };
        self.consume(Type::RightParen, "Expect ')' after for clauses.")?;

        let while_body = Stmt::block(vec![self.statement()?, Stmt::expression(increment)]);

        Ok(Stmt::block(vec![
            // initialise the variables first
            initializer,
            // after that, it's just normal while loop
            Stmt::while_(condition, while_body, token),
        ]))
    }

    fn print_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(Type::SemiColon, "Expect ';' after value.")?;
        Ok(Stmt::print(expr))
    }

    fn return_statement(&mut self) -> StmtResult {
        let keyword = self.previous().clone();
        let mut value = Expr::empty();
        if !self.check(Type::SemiColon) {
            value = self.expression()?;
        }

        self.consume(Type::SemiColon, "Expect ';' after return value.")?;
        Ok(Stmt::return_(keyword, value))
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.or_expression()?;
        if self.match_one(Type::Equal) {
            let equals = self.previous().clone();
            let value = Box::new(self.assignment()?);

            match expr {
                Expr::Variable { name } => Ok(Expr::Assign { name, value }),
                Expr::Get { name, object } => Ok(Expr::Set {
                    object,
                    name,
                    value,
                }),
                _ => Err(Error::parser_error(&equals, "Invalid assignment target.")),
            }
        } else {
            Ok(expr)
        }
    }

    fn or_expression(&mut self) -> ExprResult {
        let mut expr = self.and_expression()?;
        while self.match_one(Type::Or) {
            let operator = self.previous().clone();
            let right = self.and_expression()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn and_expression(&mut self) -> ExprResult {
        let mut expr = self.equality()?;
        while self.match_one(Type::And) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison()?;
        while self.match_either(&[Type::BangEqual, Type::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ExprResult {
        let mut expr = self.term()?;
        while self.match_either(&[
            Type::Greater,
            Type::GreaterEqual,
            Type::Less,
            Type::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor()?;
        while self.match_either(&[Type::Plus, Type::Minus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary()?;
        while self.match_either(&[Type::Slash, Type::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        if self.match_either(&[Type::Bang, Type::Minus]) {
            Ok(Expr::Unary {
                operator: self.previous().clone(),
                right: Box::new(self.unary()?),
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;
        loop {
            if self.match_one(Type::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.match_one(Type::Dot) {
                let name = self
                    .consume(Type::Identifier, "Expect property name after '.'.")?
                    .clone();
                expr = Expr::get(expr, name);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ExprResult {
        let mut args: Vec<Expr> = Vec::new();
        if !self.check(Type::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err(Error::parser_error(
                        self.peek(),
                        "Can't have more than 255 arguments.",
                    ));
                }

                args.push(self.expression()?);
                if !self.match_one(Type::Comma) {
                    break;
                }
            }
        }

        let paren = self.consume(Type::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::call(callee, paren.clone(), args))
    }

    fn primary(&mut self) -> ExprResult {
        if self.match_one(Type::True) {
            Ok(Expr::literal(true))
        } else if self.match_one(Type::False) {
            Ok(Expr::literal(false))
        } else if self.match_one(Type::Nil) {
            Ok(Expr::empty())
        } else if self.match_either(&[Type::Number, Type::String]) {
            Ok(Expr::literal(self.previous().value.clone()))
        } else if self.match_one(Type::LeftParen) {
            let expr = self.expression()?;
            self.consume(Type::RightParen, "Expect ')' after expression.")?;
            Ok(Expr::grouping(expr))
        } else if self.match_one(Type::Identifier) {
            Ok(Expr::variable(self.previous().clone()))
        } else if self.match_one(Type::This) {
            Ok(Expr::this(self.previous().clone()))
        } else if self.match_one(Type::Super) {
            let token = self.previous().clone();
            self.consume(Type::Dot, "Expect '.' after 'super'.")?;
            let method = self
                .consume(Type::Identifier, "Expect superclass method name.")?
                .clone();
            Ok(Expr::super_(token, method))
        } else {
            Err(Error::parser_error(self.peek(), "Expect expression."))
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == Type::Eof
    }

    fn check(&self, ty: Type) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().ty == ty
        }
    }

    fn consume(&mut self, ty: Type, msg: &str) -> Result<&Token, Error> {
        if self.check(ty) {
            Ok(self.advance())
        } else {
            Err(Error::parser_error(self.peek(), msg))
        }
    }

    fn synchronize(&mut self) {
        todo!()
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn match_either(&mut self, types: &[Type]) -> bool {
        for ty in types {
            if self.match_one(*ty) {
                // Already skipped in the `match_one`, just return result
                return true;
            }
        }

        false
    }

    fn match_one(&mut self, ty: Type) -> bool {
        if self.check(ty) {
            self.advance();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Stmt};
    use crate::parser::{Parser, StmtStream};
    use core::Scanner;
    use core::{Literal, Token, Type};

    macro_rules! token {
        ($ty:ident, $lex:literal, $col:literal, $idx:literal) => {
            Token::new(Type::$ty, String::from($lex), 0, $col, $idx, Literal::Nil)
        };
    }

    #[test]
    fn test_statements() {
        let tests = [
            // simple expression
            (
                "3 < 4;",
                Stmt::expression(Expr::binary(
                    Expr::literal(3),
                    token!(Less, "<", 2, 1),
                    Expr::literal(4),
                )),
            ),
            // grouping expression
            (
                "1 + (\"hello\" - 4) - foo;",
                Stmt::expression(Expr::binary(
                    Expr::binary(
                        Expr::literal(1),
                        token!(Plus, "+", 2, 1),
                        Expr::grouping(Expr::binary(
                            Expr::literal("hello"),
                            token!(Minus, "-", 13, 4),
                            Expr::literal(4),
                        )),
                    ),
                    token!(Minus, "-", 18, 7),
                    Expr::variable(token!(Identifier, "foo", 20, 8)),
                )),
            ),
            // logical expression
            (
                "true and false;", // logical or expressions
                Stmt::expression(Expr::logical(
                    Expr::literal(true),
                    token!(And, "and", 5, 1),
                    Expr::literal(false),
                )),
            ),
            // nested grouping
            (
                "((1 + 2) / 4) * 10;",
                Stmt::expression(Expr::binary(
                    Expr::grouping(Expr::binary(
                        Expr::grouping(Expr::binary(
                            Expr::literal(1),
                            token!(Plus, "+", 4, 3),
                            Expr::literal(2),
                        )),
                        token!(Slash, "/", 9, 6),
                        Expr::literal(4),
                    )),
                    token!(Star, "*", 14, 9),
                    Expr::literal(10),
                )),
            ),
            // print statement
            (
                "print 1 + 2;",
                Stmt::print(Expr::binary(
                    Expr::literal(1),
                    token!(Plus, "+", 8, 2),
                    Expr::literal(2),
                )),
            ),
        ];

        for (src, expected) in tests {
            let mut scanner = Scanner::new();
            let tokens = scanner.scan_tokens(src).collect();
            let mut parser = Parser::new(&tokens);
            let stmts: Vec<Stmt> = vec![expected];

            assert_eq!(parser.parse().unwrap(), StmtStream(stmts));
        }
    }
}
