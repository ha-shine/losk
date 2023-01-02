use crate::ast::{Expr, ExprVisitor, Stmt, StmtVisitor};
use crate::errors::LoskError;
use crate::interpreter::Interpreter;
use crate::parser::StmtStream;
use crate::token::{Literal, Token};
use std::collections::HashMap;

enum State {
    Defined,
    Declared,
}

#[derive(Copy, Clone)]
enum FunctionType {
    None,
    Function,
}

pub(crate) struct Resolver<'a> {
    scopes: Vec<HashMap<String, State>>,
    interpreter: &'a mut Interpreter,
    current: FunctionType,
}

pub struct ResolvedStmts(pub(crate) Vec<Stmt>);

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            current: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: StmtStream) -> Result<ResolvedStmts, LoskError> {
        self.resolve_stmts(&stmts.0)?;
        Ok(ResolvedStmts(stmts.0))
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(), LoskError> {
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

    fn declare(&mut self, token: &Token) -> Result<(), LoskError> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let last = self.scopes.last_mut().unwrap();
        if last.contains_key(&token.lexeme) {
            Err(LoskError::runtime_error(
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

    fn resolve_local(&mut self, expr: &Expr, token: &Token) {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&token.lexeme) {
                self.interpreter.resolve(token, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
        ty: FunctionType,
    ) -> Result<(), LoskError> {
        let enclosing = self.current;
        self.current = ty;

        self.begin_scope();
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve_stmts(body)?;
        self.end_scope();

        self.current = enclosing;
        Ok(())
    }
}

impl<'a> StmtVisitor for Resolver<'a> {
    type Item = ();

    fn visit_block(&mut self, stmt: &Stmt, statements: &[Stmt]) -> Result<Self::Item, LoskError> {
        self.begin_scope();
        self.resolve_stmts(statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression(&mut self, _: &Stmt, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)
    }

    fn visit_function(
        &mut self,
        _: &Stmt,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, LoskError> {
        self.declare(name)?;
        self.define(name);
        self.resolve_function(name, params, body, FunctionType::Function)
    }

    fn visit_class(
        &mut self,
        _: &Stmt,
        name: &Token,
        methods: &[Stmt],
    ) -> Result<Self::Item, LoskError> {
        self.declare(name)?;
        self.define(name);
        Ok(())
    }

    fn visit_if(
        &mut self,
        _: &Stmt,
        expression: &Expr,
        _: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)?;
        self.visit_stmt(then_branch)?;
        self.visit_stmt(else_branch)
    }

    fn visit_while(
        &mut self,
        _: &Stmt,
        condition: &Expr,
        body: &Stmt,
        _: &Token,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(condition)?;
        self.visit_stmt(body)
    }

    fn visit_print(&mut self, _: &Stmt, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)
    }

    fn visit_return(&mut self, _: &Stmt, _: &Token, value: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(value)
    }

    fn visit_var(&mut self, _: &Stmt, name: &Token, init: &Expr) -> Result<Self::Item, LoskError> {
        self.declare(name)?;
        self.visit_expr(init)?;
        self.define(name);
        Ok(())
    }
}

impl<'a> ExprVisitor for Resolver<'a> {
    type Item = ();

    fn visit_assign(
        &mut self,
        expr: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(value)?;
        self.resolve_local(expr, name);
        Ok(())
    }

    fn visit_binary(
        &mut self,
        _: &Expr,
        left: &Expr,
        _: &Token,
        right: &Expr,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_call(
        &mut self,
        _: &Expr,
        callee: &Expr,
        _: &Token,
        args: &[Expr],
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(callee)?;
        for arg in args {
            self.visit_expr(arg)?;
        }
        Ok(())
    }

    fn visit_get(&mut self, _: &Expr, object: &Expr, _: &Token) -> Result<Self::Item, LoskError> {
        self.visit_expr(object)
    }

    fn visit_set(
        &mut self,
        _: &Expr,
        object: &Expr,
        _: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(object)?;
        self.visit_expr(value)
    }

    fn visit_grouping(&mut self, expr: &Expr, expression: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(expression)
    }

    fn visit_literal(&mut self, expr: &Expr, value: &Literal) -> Result<Self::Item, LoskError> {
        Ok(())
    }

    fn visit_logical(
        &mut self,
        _: &Expr,
        left: &Expr,
        _: &Token,
        right: &Expr,
    ) -> Result<Self::Item, LoskError> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_unary(&mut self, _: &Expr, _: &Token, right: &Expr) -> Result<Self::Item, LoskError> {
        self.visit_expr(right)
    }

    fn visit_variable(&mut self, expr: &Expr, name: &Token) -> Result<Self::Item, LoskError> {
        // Check if variable is being accessed in its own initializer,
        // which means the variables is defined, but value not bound yet
        if let Some(last) = self.scopes.last() {
            if let Some(State::Declared) = last.get(&name.lexeme) {
                return Err(LoskError::runtime_error(
                    name,
                    "Can't read local variable in its own initializer.",
                ));
            }
        }

        self.resolve_local(expr, name);
        Ok(())
    }
}
