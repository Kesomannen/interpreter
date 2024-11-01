use super::Type;
use crate::{
    span::Span,
    tokenize::{BinOperator, UnOperator},
};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("type mismatch: expected a value of type {expected}, got {actual}")]
    TypeMismatch { expected: Type, actual: Type },

    #[error(
        "argument mismatch: expected {expected} {}, got {actual}",
        if *expected == 1 { "argument" } else { "arguments" }
    )]
    ArgumentMismatch { expected: usize, actual: usize },

    #[error("unsupported operation: cannot {} {lhs} and {rhs}", operator.verb())]
    UnsupportedBinOp {
        lhs: Type,
        rhs: Type,
        operator: BinOperator,
        span: Span,
    },

    #[error("unsupported operation: cannot {} {operand}", operator.verb())]
    UnsupportedUnOp {
        operand: Type,
        operator: UnOperator,
        span: Span,
    },

    #[error("tried to invoke a value of type {_0}")]
    CannotInvoke(Type),

    #[error("process exited")]
    Exit,
}

pub type Result<T> = std::result::Result<T, Error>;
