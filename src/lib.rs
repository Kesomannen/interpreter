mod execute;
mod parse;
mod span;
mod tokenize;

use anyhow::Context;
use parse::parse;
use tokenize::tokenize;

pub use execute::{Executor, Value};

impl Executor {
    pub fn eval(&mut self, src: &str) -> anyhow::Result<Value> {
        let tokens = tokenize(src);
        let expr = parse(tokens).context("syntax error")?;
        let value = self.eval_expr(expr)?;
        Ok(value)
    }
}
