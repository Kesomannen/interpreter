use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("unexpected character: {0:?}")]
    UnexpectedChar(char, usize),
}

pub type Result<T> = std::result::Result<T, Error>;
