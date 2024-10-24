use derive_more::derive::Display;
use std::collections::HashMap;

mod error;
pub use error::{Error, Result};

use crate::{parse::ast::*, span::Span, tokenize::BinOperator};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Executor {
    vars: HashMap<String, Value>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub(crate) fn eval_expr(&mut self, expr: Expr) -> Result<Value> {
        let Expr { span, kind, .. } = expr;

        match kind {
            ExprKind::String(str) => Ok(Value::String(str)),
            ExprKind::Int(int) => Ok(Value::Int(int)),
            ExprKind::Call(call) => self.eval_call(call, span),
            ExprKind::BinOp(bin_op) => self.eval_bin_op(bin_op, span),
            ExprKind::Assign(assign) => self.eval_assign(assign),
            ExprKind::Var(name) => self.eval_var(name, span),
        }
    }

    fn eval_call(&mut self, call: Call, span: Span) -> Result<Value> {
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

                match self.eval_expr(arg)? {
                    Value::String(str) => println!("{str}"),
                    Value::Int(int) => println!("{int}"),
                    value => {
                        return Err(Error::TypeMismatch {
                            expected: Type::String,
                            actual: value,
                        })
                    }
                };

                Ok(Value::Void)
            }
            _ => Err(Error::UndefinedFunction(call.name, span)),
        }
    }

    fn eval_bin_op(&mut self, bin_op: BinOp, span: Span) -> Result<Value> {
        let BinOp { operator, lhs, rhs } = bin_op;

        let lhs = self.eval_expr(*lhs)?;
        let rhs = self.eval_expr(*rhs)?;

        let value = match (operator, lhs, rhs) {
            (operator, Value::Int(lhs), Value::Int(rhs)) => Value::Int(match operator {
                BinOperator::Add => lhs + rhs,
                BinOperator::Sub => lhs - rhs,
                BinOperator::Mul => lhs * rhs,
                BinOperator::Div => lhs / rhs,
            }),
            (BinOperator::Add, Value::String(lhs), Value::String(rhs)) => Value::String(lhs + &rhs),
            (operator, lhs, rhs) => {
                return Err(Error::UnsupportedOp {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    operator,
                    span,
                })
            }
        };

        Ok(value)
    }

    fn eval_assign(&mut self, assign: Assign) -> Result<Value> {
        let value = self.eval_expr(*assign.value)?;
        self.vars.insert(assign.name, value);
        Ok(Value::Void)
    }

    fn eval_var(&mut self, name: String, span: Span) -> Result<Value> {
        self.vars
            .get(&name)
            .cloned()
            .ok_or(Error::UndefinedVariable(name, span))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum Type {
    #[display("void")]
    Void,
    #[display("string")]
    String,
    #[display("int")]
    Int,
}

#[derive(Debug, Clone, Display, PartialEq)]
pub enum Value {
    Void,
    #[display("\"{_0}\"")]
    String(String),
    Int(i32),
}

impl Value {
    pub(crate) fn ty(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::String(_) => Type::String,
            Value::Int(_) => Type::Int,
        }
    }
}
