use std::num::ParseIntError;

use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("unexpected character: '{0}'")]
    UnexpectedChar(char, usize),

    #[error("invalid number literal '{0}': {1}")]
    InvalidNumber(String, ParseIntError),
}

pub type Result<T> = std::result::Result<T, Error>;
