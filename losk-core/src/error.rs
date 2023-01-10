use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum Error {
    #[error("unterminated block comment")]
    UnterminatedBlockComment { line: usize },

    #[error("unterminated string")]
    UnterminatedString { line: usize },

    #[error("unexpected character")]
    UnexpectedCharacter { ch: char, line: usize },
}

impl Error {
    pub fn line(&self) -> usize {
        match self {
            Error::UnterminatedBlockComment { line } => *line,
            Error::UnterminatedString { line } => *line,
            Error::UnexpectedCharacter { line, .. } => *line,
        }
    }
}
