use crate::error::Error;
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
    This {
        keyword: Token,
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
    Super {
        keyword: Token,
        method: Token,
    },
    Empty,
}

pub(crate) trait ExprVisitor {
    type Item;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Item, Error> {
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
            Expr::Set {
                object,
                name,
                value,
            } => self.visit_set(object, name, value),
            Expr::Grouping { expression } => self.visit_grouping(expression),
            Expr::Literal { value } => self.visit_literal(value),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_logical(left, operator, right),
            Expr::Unary { operator, right } => self.visit_unary(operator, right),
            Expr::Variable { name } => self.visit_variable(name),
            Expr::This { keyword } => self.visit_this(keyword),
            Expr::Super { keyword, method } => self.visit_super(keyword, method),
            Expr::Empty => self.visit_empty(),
        }
    }
    fn visit_assign(
        &mut self,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Self::Item, Error>;
    fn visit_get(
        &mut self,
        object: &Expr,
        name: &Token,
    ) -> Result<Self::Item, Error>;
    fn visit_set(
        &mut self,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_this(&mut self, keyword: &Token) -> Result<Self::Item, Error>;
    fn visit_super(
        &mut self,
        keyword: &Token,
        method: &Token,
    ) -> Result<Self::Item, Error>;
    fn visit_grouping(&mut self, expression: &Expr) -> Result<Self::Item, Error>;
    fn visit_literal(&mut self, value: &Literal) -> Result<Self::Item, Error>;
    fn visit_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_unary(
        &mut self,
        operator: &Token,
        right: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_variable(&mut self, name: &Token) -> Result<Self::Item, Error>;
    fn visit_empty(&mut self) -> Result<Self::Item, Error>;
}

#[allow(dead_code)]
impl Expr {
    pub(crate) fn empty() -> Self {
        Expr::Empty
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

    pub(crate) fn this(keyword: Token) -> Self {
        Expr::This { keyword }
    }

    pub(crate) fn super_(keyword: Token, method: Token) -> Self {
        Expr::Super { keyword, method }
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
        superclass: Rc<Expr>,
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

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Self::Item, Error> {
        match stmt {
            Stmt::Expression { expression } => self.visit_expression(expression),
            Stmt::Block { statements } => self.visit_block(statements),
            Stmt::Function { name, params, body } => self.visit_function(name, params, body),
            Stmt::Class {
                name,
                superclass,
                methods,
            } => self.visit_class(name, superclass, methods),
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

    fn visit_block(&mut self, statements: &[Stmt]) -> Result<Self::Item, Error>;
    fn visit_expression(&mut self, expression: &Expr)
        -> Result<Self::Item, Error>;
    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<Self::Item, Error>;
    fn visit_class(
        &mut self,
        name: &Token,
        superclass: &Expr,
        methods: &[Stmt],
    ) -> Result<Self::Item, Error>;
    fn visit_if(
        &mut self,
        expression: &Expr,
        token: &Token,
        then_branch: &Stmt,
        else_branch: &Stmt,
    ) -> Result<Self::Item, Error>;
    fn visit_while(
        &mut self,
        condition: &Expr,
        body: &Stmt,
        token: &Token,
    ) -> Result<Self::Item, Error>;
    fn visit_print(&mut self, expression: &Expr) -> Result<Self::Item, Error>;
    fn visit_return(
        &mut self,
        keyword: &Token,
        value: &Expr,
    ) -> Result<Self::Item, Error>;
    fn visit_var(
        &mut self,
        name: &Token,
        init: &Expr,
    ) -> Result<Self::Item, Error>;
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
    pub(crate) fn class(name: Token, superclass: Expr, methods: Vec<Stmt>) -> Self {
        Stmt::Class {
            name,
            superclass: Rc::new(superclass),
            methods,
        }
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
