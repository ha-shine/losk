use crate::value::Value;
use core::Error as CoreError;
use core::Token;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub enum Error {
    #[error("[line {line}] scanner error: {}", .source)]
    ScannerError { line: usize, source: CoreError },

    #[error("[line {line:?}] parser error: {msg:?}")]
    ParserError {
        token: Token,

        // line is copied from token, this is required because thiserror doesn't support field
        // access, e.g {token.line:?}, in error strings
        line: usize,
        msg: &'static str,
    },

    #[error("{:?}", msg)]
    RuntimeError { token: Token, msg: String },

    #[error("return value")]
    Return(ReturnValue),
}

impl From<CoreError> for Error {
    fn from(value: CoreError) -> Self {
        Error::ScannerError {
            line: value.line(),
            source: value,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnValue {
    pub(crate) value: Value,
}

impl Error {
    pub(crate) fn parser_error(token: &Token, msg: &'static str) -> Self {
        Error::ParserError {
            token: token.clone(),
            line: token.line,
            msg,
        }
    }

    pub(crate) fn runtime_error(token: &Token, msg: &str) -> Self {
        Error::RuntimeError {
            token: token.clone(),
            msg: String::from(msg),
        }
    }

    pub(crate) fn return_value(value: Value) -> Self {
        Error::Return(ReturnValue { value })
    }
}
