use crate::ast::{Expr, ExprVisitor, Stmt, StmtVisitor};
use crate::error::Error;
use crate::interpreter::Interpreter;
use crate::parser::StmtStream;
use crate::value::Value;
use core::Token;
use std::collections::HashMap;

enum State {
    Defined,
    Declared,
}

#[derive(Copy, Clone)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Copy, Clone)]
enum ClassType {
    None,
    Class,
    Subclass,
}

pub(crate) struct Resolver<'a> {
    scopes: Vec<HashMap<String, State>>,
    interpreter: &'a mut Interpreter,
    current_fun: FunctionType,
    current_cls: ClassType,
}

pub struct ResolvedStmts(pub(crate) Vec<Stmt>);

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            current_fun: FunctionType::None,
            current_cls: ClassType::None,
        }
    }

    pub fn resolve(&mut self, stmts: StmtStream) -> Result<ResolvedStmts, Error> {
        self.resolve_stmts(&stmts.0)?;
        Ok(ResolvedStmts(stmts.0))
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn declare(&mut self, token: &Token) -> Result<(), Error> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let last = self.scopes.last_mut().unwrap();
        if last.contains_key(&token.lexeme) {
            Err(Error::runtime_error(
                token,
                "Already a variable with this name in this scope.",
            ))
        } else {
            last.insert(token.lexeme.clone(), State::Declared);
            Ok(())
        }
    }

    fn define(&mut self, token: &Token) {
        if !self.scopes.is_empty() {
            self.scopes
                .last_mut()
                .unwrap()
                .insert(token.lexeme.clone(), State::Defined);
        }
    }

    fn resolve_local(&mut self, token: &Token) {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&token.lexeme) {
                self.interpreter.resolve(token, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        _name: &Token,
        params: &[Token],
        body: &[Stmt],
        ty: FunctionType,
    ) -> Result<(), Error> {
        let enclosing = self.current_fun;
        self.current_fun = ty;

        self.begin_scope();
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve_stmts(body)?;
        self.end_scope();

        self.current_fun = enclosing;
        Ok(())
    }
}

impl<'a> StmtVisitor for Resolver<'a> {
    type Item = ();

    fn visit_block(&mut self, statements: &[Stmt]) -> Result<Self::Item, Error> {
        self.begin_scope();
        self.resolve_stmts(statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(expression)
    }

    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, Error> {
        self.declare(name)?;
        self.define(name);
        self.resolve_function(name, params, body, FunctionType::Function)
    }

    fn visit_class(
        &mut self,
        name: &Token,
        superclass: &Expr,
        methods: &[Stmt],
    ) -> Result<Self::Item, Error> {
        let current = self.current_cls;
        self.current_cls = ClassType::Class;
        self.declare(name)?;
        self.define(name);

        if let Expr::Variable { name: super_name } = superclass {
            if name == super_name {
                return Err(Error::runtime_error(
                    super_name,
                    "A class can't inherit from itself.",
                ));
            }
            self.current_cls = ClassType::Subclass;
            self.visit_variable(super_name)?;
            self.begin_scope();
            self.scopes
                .last_mut()
                .unwrap()
                .insert("super".to_string(), State::Defined);
        }

        self.begin_scope();
        self.scopes
            .last_mut()
            .unwrap()
            .insert("this".to_string(), State::Defined);
        for method in methods {
            if let Stmt::Function { name, params, body } = method {
                let ty = if name.lexeme == "init" {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                };

                self.resolve_function(name, params, body, ty)?;
            } else {
                panic!(
                    "Unexpected statement '{:?}' found in class body, expecting a method.",
                    method
                )
            }
        }

        self.end_scope();
        if let Expr::Variable { .. } = superclass {
            self.end_scope();
        }

        self.current_cls = current;
        Ok(())
    }

    fn visit_if(
        &mut self,
        expression: &Expr,
        _: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<Self::Item, Error> {
        self.visit_expr(expression)?;
        self.visit_stmt(then_branch)?;
        self.visit_stmt(else_branch)
    }

    fn visit_while(
        &mut self,
        condition: &Expr,
        body: &Stmt,
        _: &Token,
    ) -> Result<Self::Item, Error> {
        self.visit_expr(condition)?;
        self.visit_stmt(body)
    }

    fn visit_print(&mut self, expression: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(expression)
    }

    fn visit_return(&mut self, keyword: &Token, value: &Expr) -> Result<Self::Item, Error> {
        match value {
            Expr::Empty => Ok(()),
            _ => {
                if let FunctionType::Initializer = self.current_fun {
                    Err(Error::runtime_error(
                        keyword,
                        "Can't return a value from an initializer.",
                    ))
                } else {
                    self.visit_expr(value)
                }
            }
        }
    }

    fn visit_var(&mut self, name: &Token, init: &Expr) -> Result<Self::Item, Error> {
        self.declare(name)?;
        self.visit_expr(init)?;
        self.define(name);
        Ok(())
    }
}

impl<'a> ExprVisitor for Resolver<'a> {
    type Item = ();

    fn visit_assign(&mut self, name: &Token, value: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(value)?;
        self.resolve_local(name);
        Ok(())
    }

    fn visit_binary(&mut self, left: &Expr, _: &Token, right: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_call(&mut self, callee: &Expr, _: &Token, args: &[Expr]) -> Result<Self::Item, Error> {
        self.visit_expr(callee)?;
        for arg in args {
            self.visit_expr(arg)?;
        }
        Ok(())
    }

    fn visit_get(&mut self, object: &Expr, _: &Token) -> Result<Self::Item, Error> {
        self.visit_expr(object)
    }

    fn visit_set(&mut self, object: &Expr, _: &Token, value: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(object)?;
        self.visit_expr(value)
    }

    fn visit_this(&mut self, keyword: &Token) -> Result<Self::Item, Error> {
        if let ClassType::None = self.current_cls {
            return Err(Error::runtime_error(
                keyword,
                "Can't use 'this' outside of a class.",
            ));
        }

        self.resolve_local(keyword);
        Ok(())
    }

    fn visit_super(&mut self, keyword: &Token, _method: &Token) -> Result<Self::Item, Error> {
        match self.current_cls {
            ClassType::None => Err(Error::runtime_error(
                keyword,
                "Can't use 'super' outside of a class.",
            )),
            ClassType::Class => Err(Error::runtime_error(
                keyword,
                "Can't use 'super' in a class with no superclass.",
            )),
            ClassType::Subclass => {
                self.resolve_local(keyword);
                Ok(())
            }
        }
    }

    fn visit_grouping(&mut self, expression: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(expression)
    }

    fn visit_literal(&mut self, _value: &Value) -> Result<Self::Item, Error> {
        Ok(())
    }

    fn visit_logical(&mut self, left: &Expr, _: &Token, right: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_unary(&mut self, _: &Token, right: &Expr) -> Result<Self::Item, Error> {
        self.visit_expr(right)
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Self::Item, Error> {
        // Check if variable is being accessed in its own initializer,
        // which means the variables is defined, but value not bound yet
        if let Some(last) = self.scopes.last() {
            if let Some(State::Declared) = last.get(&name.lexeme) {
                return Err(Error::runtime_error(
                    name,
                    "Can't read local variable in its own initializer.",
                ));
            }
        }

        self.resolve_local(name);
        Ok(())
    }

    fn visit_empty(&mut self) -> Result<Self::Item, Error> {
        Ok(())
    }
}
