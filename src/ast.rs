use crate::token::{Literal, Token};

// Currently tokens are cloned in every creation (stmt or expr) because they are not that
// expensive to do so, and the cloning are done during parsing stage only.
// I can try to use references instead of cloning if it starts showing inefficiency.

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expression: Box<Expr>,
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
        expression: Box<Expr>,
        token: Token,
        then_branch: Box<Stmt>,
        else_branch: Box<Stmt>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
        token: Token,
    },
    Print {
        expression: Box<Expr>,
    },
    Return {
        keyword: Token,
        value: Box<Expr>,
    },
    Var {
        name: Token,
        init: Box<Expr>,
    },
}

impl Stmt {
    pub(crate) fn block(statements: Vec<Stmt>) -> Self {
        Stmt::Block { statements }
    }

    pub(crate) fn expression(expression: Expr) -> Self {
        Stmt::Expression {
            expression: Box::new(expression),
        }
    }

    pub(crate) fn function(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Stmt::Function { name, params, body }
    }

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
            expression: Box::new(expression),
            token,
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    pub(crate) fn while_(condition: Expr, body: Stmt, token: Token) -> Self {
        Stmt::While {
            condition: Box::new(condition),
            body: Box::new(body),
            token,
        }
    }

    pub(crate) fn print(expression: Expr) -> Self {
        Stmt::Print {
            expression: Box::new(expression),
        }
    }

    pub(crate) fn return_(keyword: Token, value: Expr) -> Self {
        Stmt::Return {
            keyword,
            value: Box::new(value),
        }
    }

    pub(crate) fn var(name: Token, init: Expr) -> Self {
        Stmt::Var {
            name,
            init: Box::new(init),
        }
    }
}
