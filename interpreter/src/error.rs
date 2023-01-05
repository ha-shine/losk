use crate::token::{Literal, Token};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum Error {
    #[error("[line {line:?}] scanner error: {msg:?}")]
    ScannerError { line: usize, msg: String },

    #[error("[line {line:?}] parser error: {msg:?}")]
    ParserError {
        token: Token,

        // line is copied from token, this is required because thiserror doesn't support field
        // access, e.g {token.line:?}, in error strings
        line: usize,
        msg: String,
    },

    #[error("{:?}", msg)]
    RuntimeError { token: Token, msg: String },

    #[error("return value")]
    Return(ReturnValue),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub(crate) value: Literal,
}

impl Error {
    pub(crate) fn parser_error(token: &Token, msg: &str) -> Self {
        Error::ParserError {
            token: token.clone(),
            line: token.line,
            msg: String::from(msg),
        }
    }

    pub(crate) fn runtime_error(token: &Token, msg: &str) -> Self {
        Error::RuntimeError {
            token: token.clone(),
            msg: String::from(msg),
        }
    }

    pub(crate) fn return_value(value: Literal) -> Self {
        Error::Return(ReturnValue { value })
    }
}
