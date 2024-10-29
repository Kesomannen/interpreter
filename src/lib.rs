mod execute;
mod parse;
mod span;
mod tokenize;

use parse::Parser;
use tokenize::tokenize;

pub use execute::{Executor, Value};

impl Executor {
    pub fn eval(&mut self, src: &str) -> anyhow::Result<Value> {
        let tokens = tokenize(src);
        let expr = Parser::new(tokens).parse_expr()?;
        let value = self.eval_expr(expr)?;
        Ok(value)
    }
}
