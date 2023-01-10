use losk_core::Error as CoreError;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Error {
    #[error("[line {line}] scanner error: {}", .source)]
    ScannerError { line: usize, source: CoreError },

    #[error("[line {line:?}] compile error: {msg:?}")]
    CompileError { line: usize, msg: String },

    #[error("[line {line:?}] runtime error: {msg:?}")]
    RuntimeError { line: usize, msg: String },
}

impl Error {
    pub(crate) fn runtime(line: usize, msg: &str) -> Error {
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
