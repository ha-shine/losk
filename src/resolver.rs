use crate::ast::{Expr, Stmt};
use crate::parser::StmtStream;
use std::collections::HashMap;

pub(crate) struct Resolver;

pub struct ResolvedStmts<'a> {
    pub(crate) stmts: Vec<Stmt>,
    pub(crate) locals: HashMap<&'a Expr, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver
    }

    pub fn resolve(&self, stmts: StmtStream) -> ResolvedStmts {
        let mut locals = HashMap::new();

        ResolvedStmts {
            stmts: stmts.0,
            locals,
        }
    }
}
