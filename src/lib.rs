mod execute;
mod parse;
mod span;
mod tokenize;

use parse::Parser;
use tokenize::tokenize;

pub use execute::{Executor, Value};

pub enum EvalMode {
    Expr,
    File,
}

impl Executor {
    pub fn eval(&mut self, src: &str) -> anyhow::Result<Value> {
        let expr = Parser::new(tokenize(src)).parse_expr()?;
        let value = self.eval_expr(&expr)?;
        Ok(value)
    }

    pub fn exec(&mut self, src: &str) -> anyhow::Result<()> {
        let exprs = Parser::new(tokenize(src)).parse_file()?;

        for expr in &exprs {
            self.eval_expr(expr)?;
        }

        Ok(())
    }
}
