use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum LoskError {
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
}

impl LoskError {
    pub(crate) fn parser_error(token: &Token, msg: &str) -> Self {
        LoskError::ParserError {
            token: token.clone(),
            line: token.line,
            msg: String::from(msg),
        }
    }

    pub(crate) fn runtime_error(token: &Token, msg: &str) -> Self {
        LoskError::RuntimeError {
            token: token.clone(),
            msg: String::from(msg),
        }
    }
}
