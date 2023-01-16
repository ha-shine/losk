use losk_core::Error as CoreError;
use std::fmt::{Display, Formatter};
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Error {
    #[error("[line {line}] scanner error: {}", .source)]
    ScannerError { line: usize, source: CoreError },

    #[error("[line {line:?}] compile error: {msg:?}")]
    CompileError { line: usize, msg: String },

    #[error("[line {line:?}] runtime error: {msg:?}\n{}", .stack_trace)]
    RuntimeError {
        line: usize,
        msg: String,
        stack_trace: StackTrace,
    },
}

#[derive(Debug, Error, PartialEq)]
pub(crate) struct StackTrace(pub(crate) Vec<StackData>);

#[derive(Debug, Error, PartialEq)]
pub(crate) struct StackData {
    line: usize,
    fname: String,
}

impl StackData {
    pub(crate) fn new(line: usize, fname: String) -> Self {
        StackData { line, fname }
    }
}

impl Display for StackData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] in {}", self.line, self.fname)
    }
}

impl Display for StackTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for sd in &self.0 {
            write!(f, "{}", sd)?;
        }
        Ok(())
    }
}

impl Error {
    pub(crate) fn runtime(line: usize, msg: &str, stack_trace: StackTrace) -> Error {
        Error::RuntimeError {
            line,
            msg: msg.to_string(),
            stack_trace,
        }
    }

    pub(crate) fn compile(line: usize, msg: &str) -> Error {
        Error::CompileError {
            line,
            msg: msg.to_string(),
        }
    }
}

impl From<CoreError> for Error {
    fn from(value: CoreError) -> Self {
        Error::ScannerError {
            line: value.line(),
            source: value,
        }
    }
}

pub(crate) type VmResult<T> = Result<T, Error>;
pub(crate) type CompilerResult<T> = Result<T, Error>;
