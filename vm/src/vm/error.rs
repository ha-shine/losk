use std::fmt;
use std::fmt::{Display, Formatter};

use thiserror::Error;

#[derive(Error, Debug)]
#[error("Runtime error: {msg}\n{}", .stack_trace)]
pub struct RuntimeError {
    msg: String,
    stack_trace: StackTrace,
}

impl RuntimeError {
    pub(super) fn new(msg: fmt::Arguments, stack_trace: StackTrace) -> Box<RuntimeError> {
        Box::new(RuntimeError {
            msg: format!("{}", msg),
            stack_trace,
        })
    }
}

#[derive(Debug, Error, PartialEq)]
pub struct StackTrace(pub(super) Vec<StackData>);

#[derive(Debug, Error, PartialEq)]
pub(super) struct StackData {
    line: usize,
    fname: String,
}

impl StackData {
    pub(super) fn new(line: usize, fname: String) -> Self {
        StackData { line, fname }
    }
}

impl Display for StackData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] in {}", self.line, self.fname)
    }
}

impl Display for StackTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for sd in &self.0 {
            writeln!(f, "{}", sd)?;
        }
        Ok(())
    }
}
