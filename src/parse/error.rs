use std::borrow::Cow;

use thiserror::Error;

use crate::tokenize::{self, Token};

#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token: expected {expected}, got '{actual}'")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: Token,
    },

    #[error(transparent)]
    Tokenize(#[from] tokenize::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
