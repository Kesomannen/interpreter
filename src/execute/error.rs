use thiserror::Error;

use crate::{span::Span, tokenize::BinOperator};

use super::{Type, Value};

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("undefined function: {0}")]
    UndefinedFunction(String, Span),

    #[error("undefined variable: {0}")]
    UndefinedVariable(String, Span),

    #[error("type mismatch: expected a value of type {expected}, got '{actual}'")]
    TypeMismatch { expected: Type, actual: Value },

    #[error("argument mismatch: expected {expected} argument(s), got {actual}")]
    ArgumentMismatch { expected: usize, actual: usize },

    #[error("unsupported operation: cannot {operator} {lhs} and {rhs}")]
    UnsupportedOp {
        lhs: Type,
        rhs: Type,
        operator: BinOperator,
        span: Span,
    },
}

pub type Result<T> = std::result::Result<T, Error>;
