use std::time::Instant;

use parse::Parser;
use thiserror::Error;
use tokenize::tokenize;

mod execute;
mod parse;
mod span;
mod tokenize;

pub use execute::{Executor, Value};

pub enum EvalMode {
    Expr,
    File,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct Error(ErrorKind);

impl<T> From<T> for Error
where
    T: Into<ErrorKind>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

#[derive(Debug, Error)]
enum ErrorKind {
    #[error(transparent)]
    Parse(#[from] parse::Error),

    #[error(transparent)]
    Execute(#[from] execute::Error),
}

type Result<T> = std::result::Result<T, Error>;

impl Executor {
    pub fn eval(&mut self, src: &str) -> Result<Value> {
        let expr = Parser::new(tokenize(src)).parse_expr()?;
        let value = self.eval_expr(&expr)?;
        Ok(value)
    }

    pub fn exec(&mut self, src: &str) -> Result<()> {
        let start = Instant::now();

        let exprs = Parser::new(tokenize(src)).parse_file()?;

        let parse_done = Instant::now();

        for expr in &exprs {
            self.eval_expr(expr)?;
        }

        println!(
            "\nprogram finished in {:?} ({:?} parse)",
            start.elapsed(),
            parse_done - start
        );

        Ok(())
    }
}
