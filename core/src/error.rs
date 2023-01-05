use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub enum Error {
    #[error("unterminated block comment")]
    UnterminatedBlockComment { line: usize },

    #[error("unterminated string")]
    UnterminatedString { line: usize },

    #[error("unexpected character")]
    UnexpectedCharacter { ch: char, line: usize },
}
