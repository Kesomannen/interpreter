use thiserror::Error;

use crate::span::Span;

use super::{Type, Value};

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("undefined function: {0}")]
    UndefinedFunction(String, Span),

    #[error("type mismatch: expected a value of type {expected:?}, got {actual:?}")]
    TypeMismatch { expected: Type, actual: Value },

    #[error("argument mismatch: expected {expected} argument(s), got {actual}")]
    ArgumentMismatch { expected: usize, actual: usize },
}

pub type Result<T> = std::result::Result<T, Error>;
