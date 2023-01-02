use crate::errors::LoskError;
use crate::token::{Literal, Token};
use std::rc::Rc;

// Currently tokens are cloned in every creation (stmt or expr) because they are not that
// expensive to do so, and the cloning are done during parsing stage only.
// I can try to use references instead of cloning if it starts showing inefficiency.

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

pub(crate) trait ExprVisitor {
    type Item;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Item, LoskError> {
        match expr {
            Expr::Assign { name, value } => self.visit_assign(expr, name, value),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.visit_binary(expr, left, operator, right),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.visit_call(expr, callee, paren, args),
            Expr::Get { object, name } => self.visit_get(expr, object, name),
            Expr::Set { object, name, value } => self.visit_set(expr, object, name, value),
            Expr::Grouping { expression } => self.visit_grouping(expr, expression),
            Expr::Literal { value } => self.visit_literal(expr, value),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_logical(expr, left, operator, right),
            Expr::Unary { operator, right } => self.visit_unary(expr, operator, right),
            Expr::Variable { name } => self.visit_variable(expr, name),
        }
    }
    fn visit_assign(
        &mut self,
        expr: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_binary(
        &mut self,
        expr: &Expr,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_call(
        &mut self,
        expr: &Expr,
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Self::Item, LoskError>;
    fn visit_get(
        &mut self,
        expr: &Expr,
        object: &Expr,
        name: &Token,
    ) -> Result<Self::Item, LoskError>;
    fn visit_set(
        &mut self,
        expr: &Expr,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_grouping(&mut self, expr: &Expr, expression: &Expr) -> Result<Self::Item, LoskError>;
    fn visit_literal(&mut self, expr: &Expr, value: &Literal) -> Result<Self::Item, LoskError>;
    fn visit_logical(
        &mut self,
        expr: &Expr,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_unary(
        &mut self,
        expr: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_variable(&mut self, expr: &Expr, name: &Token) -> Result<Self::Item, LoskError>;
}

#[allow(dead_code)]
impl Expr {
    pub(crate) fn nil() -> Self {
        Expr::literal(Literal::Nil)
    }

    // Creator methods, these could most likely be written as a proc-macro, but I will need
    // a separate crate. So here they go.
    pub(crate) fn assign(name: Token, value: Expr) -> Self {
        Expr::Assign {
            name,
            value: Box::new(value),
        }
    }

    pub(crate) fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub(crate) fn call(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Expr::Call {
            callee: Box::new(callee),
            paren,
            args,
        }
    }

    pub(crate) fn get(object: Expr, name: Token) -> Self {
        Expr::Get {
            object: Box::new(object),
            name,
        }
    }

    pub(crate) fn grouping(expression: Expr) -> Self {
        Expr::Grouping {
            expression: Box::new(expression),
        }
    }

    pub(crate) fn literal<T>(value: T) -> Self
    where
        Literal: From<T>,
    {
        Expr::Literal {
            value: Literal::from(value),
        }
    }

    pub(crate) fn logical(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub(crate) fn unary(operator: Token, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub(crate) fn variable(name: Token) -> Self {
        Expr::Variable { name }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expression: Rc<Expr>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>, // only Functions are allowed here
    },
    If {
        expression: Rc<Expr>,
        token: Token,
        then_branch: Rc<Stmt>,
        else_branch: Rc<Stmt>,
    },
    While {
        condition: Rc<Expr>,
        body: Rc<Stmt>,
        token: Token,
    },
    Print {
        expression: Rc<Expr>,
    },
    Return {
        keyword: Token,
        value: Rc<Expr>,
    },
    Var {
        name: Token,
        init: Rc<Expr>,
    },
}

pub(crate) trait StmtVisitor {
    type Item;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Self::Item, LoskError> {
        match stmt {
            Stmt::Expression { expression } => self.visit_expression(stmt, expression),
            Stmt::Block { statements } => self.visit_block(stmt, statements),
            Stmt::Function { name, params, body } => self.visit_function(stmt, name, params, body),
            Stmt::Class { name, methods } => self.visit_class(stmt, name, methods),
            Stmt::If {
                expression,
                token,
                then_branch,
                else_branch,
            } => self.visit_if(stmt, expression, token, then_branch, else_branch),
            Stmt::While {
                condition,
                body,
                token,
            } => self.visit_while(stmt, condition, body, token),
            Stmt::Print { expression } => self.visit_print(stmt, expression),
            Stmt::Return { keyword, value } => self.visit_return(stmt, keyword, value),
            Stmt::Var { name, init } => self.visit_var(stmt, name, init),
        }
    }

    fn visit_block(&mut self, stmt: &Stmt, statements: &[Stmt]) -> Result<Self::Item, LoskError>;
    fn visit_expression(&mut self, stmt: &Stmt, expression: &Expr)
        -> Result<Self::Item, LoskError>;
    fn visit_function(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, LoskError>;
    fn visit_class(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        methods: &[Stmt],
    ) -> Result<Self::Item, LoskError>;
    fn visit_if(
        &mut self,
        stmt: &Stmt,
        expression: &Expr,
        token: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<Self::Item, LoskError>;
    fn visit_while(
        &mut self,
        stmt: &Stmt,
        condition: &Expr,
        body: &Stmt,
        token: &Token,
    ) -> Result<Self::Item, LoskError>;
    fn visit_print(&mut self, stmt: &Stmt, expression: &Expr) -> Result<Self::Item, LoskError>;
    fn visit_return(
        &mut self,
        stmt: &Stmt,
        keyword: &Token,
        value: &Expr,
    ) -> Result<Self::Item, LoskError>;
    fn visit_var(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        init: &Expr,
    ) -> Result<Self::Item, LoskError>;
}

impl Stmt {
    pub(crate) fn block(statements: Vec<Stmt>) -> Self {
        Stmt::Block { statements }
    }

    pub(crate) fn expression(expression: Expr) -> Self {
        Stmt::Expression {
            expression: Rc::new(expression),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn function(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Stmt::Function { name, params, body }
    }

    #[allow(dead_code)]
    pub(crate) fn class(name: Token, methods: Vec<Stmt>) -> Self {
        Stmt::Class { name, methods }
    }

    pub(crate) fn if_(
        expression: Expr,
        token: Token,
        then_branch: Stmt,
        else_branch: Stmt,
    ) -> Self {
        Stmt::If {
            expression: Rc::new(expression),
            token,
            then_branch: Rc::new(then_branch),
            else_branch: Rc::new(else_branch),
        }
    }

    pub(crate) fn while_(condition: Expr, body: Stmt, token: Token) -> Self {
        Stmt::While {
            condition: Rc::new(condition),
            body: Rc::new(body),
            token,
        }
    }

    pub(crate) fn print(expression: Expr) -> Self {
        Stmt::Print {
            expression: Rc::new(expression),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn return_(keyword: Token, value: Expr) -> Self {
        Stmt::Return {
            keyword,
            value: Rc::new(value),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn var(name: Token, init: Expr) -> Self {
        Stmt::Var {
            name,
            init: Rc::new(init),
        }
    }
}
