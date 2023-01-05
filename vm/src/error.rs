use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Error {
    #[error("[line {line:?}] compile error: {msg:?}")]
    CompileError { line: usize, msg: Cow<'static, str> },

    #[error("[line {line:?}] runtime error: {msg:?}")]
    RuntimeError { line: usize, msg: Cow<'static, str> },
}

pub(crate) type Result<T> = std::result::Result<T, Error>;
