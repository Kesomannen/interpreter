use super::{Type, Value};
use crate::{span::Span, tokenize::BinOperator};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("undefined function: {0}")]
    UndefinedFunction(String, Span),

    #[error("undefined variable: {0}")]
    UndefinedVariable(String, Span),

    #[error("type mismatch: expected a value of type {expected}, got {}", actual.ty())]
    TypeMismatch { expected: Type, actual: Value },

    #[error(
        "argument mismatch: expected {expected} {}, got {actual}",
        if *expected == 1 { "argument" } else { "arguments" }
    )]
    ArgumentMismatch { expected: usize, actual: usize },

    #[error("unsupported operation: cannot {} {lhs} and {rhs}", operator.verb())]
    UnsupportedOp {
        lhs: Type,
        rhs: Type,
        operator: BinOperator,
        span: Span,
    },

    #[error("process exited")]
    Exit,
}

pub type Result<T> = std::result::Result<T, Error>;
