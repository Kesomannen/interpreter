mod error;

use derive_more::derive::Display;
pub use error::{Error, Result};

use crate::{
    parse::ast::{self, Call},
    span::Span,
};

#[cfg(test)]
mod tests;

pub struct Executor {}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {}
    }

    pub(crate) fn evaluate_expr(&mut self, expr: ast::Expr) -> Result<Value> {
        let ast::Expr { span, kind, .. } = expr;

        match kind {
            ast::ExprKind::String(str) => Ok(Value::String(str)),
            ast::ExprKind::Call(call) => self.evaluate_call(call, span),
        }
    }

    fn evaluate_call(&mut self, call: Call, span: Span) -> Result<Value> {
        match call.name.as_str() {
            "print" => {
                let arg = call
                    .args
                    .into_iter()
                    .nth(0)
                    .ok_or(Error::ArgumentMismatch {
                        expected: 1,
                        actual: 0,
                    })?;

                let text = match self.evaluate_expr(arg)? {
                    Value::String(str) => str,
                    value => {
                        return Err(Error::TypeMismatch {
                            expected: Type::String,
                            actual: value,
                        })
                    }
                };

                println!("{text}");
                Ok(Value::Void)
            }
            _ => Err(Error::UndefinedFunction(call.name, span)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Void,
    String,
}

#[derive(Debug, Clone, Display)]
pub enum Value {
    Void,
    #[display("\"{_0}\"")]
    String(String),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::String(_) => Type::String,
        }
    }
}
