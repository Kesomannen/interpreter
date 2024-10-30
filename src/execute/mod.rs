use crate::{parse::ast::*, span::Span};
use derive_more::derive::Display;
use std::collections::HashMap;

mod error;
mod func;
pub use error::{Error, Result};

#[cfg(test)]
mod tests;

#[derive(Debug, Default)]
pub struct Executor {
    vars: HashMap<String, Value>,
}

impl Executor {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        let Expr { span, kind, .. } = expr;
        let span = span.clone();

        match kind {
            ExprKind::String(str) => Ok(Value::String(str.clone())),
            ExprKind::Int(int) => Ok(Value::Int(*int)),
            ExprKind::Bool(bool) => Ok(Value::Bool(*bool)),
            ExprKind::Call(call) => self.eval_call(call, span),
            ExprKind::BinOp(bin_op) => self.eval_bin_op(bin_op, span),
            ExprKind::Assign(assign) => self.eval_assign(assign),
            ExprKind::Var(name) => self.eval_var(name, span),
            ExprKind::If(_if) => self.eval_if(_if, span),
            ExprKind::Block(block) => self.eval_block(block, span),
        }
    }

    fn eval_bin_op(&mut self, bin_op: &BinOp, span: Span) -> Result<Value> {
        use crate::tokenize::BinOperator::*;
        use Value::*;

        let BinOp { operator, lhs, rhs } = bin_op;

        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        let value = match (operator, lhs, rhs) {
            (Eq, lhs, rhs) => Bool(lhs == rhs),
            (Ne, lhs, rhs) => Bool(lhs != rhs),
            (Add, Int(lhs), Int(rhs)) => Int(lhs + rhs),
            (Sub, Int(lhs), Int(rhs)) => Int(lhs - rhs),
            (Mul, Int(lhs), Int(rhs)) => Int(lhs * rhs),
            (Div, Int(lhs), Int(rhs)) => Int(lhs / rhs),
            (Gt, Int(lhs), Int(rhs)) => Bool(lhs > rhs),
            (Gte, Int(lhs), Int(rhs)) => Bool(lhs >= rhs),
            (Lt, Int(lhs), Int(rhs)) => Bool(lhs < rhs),
            (Lte, Int(lhs), Int(rhs)) => Bool(lhs <= rhs),
            (And, Bool(lhs), Bool(rhs)) => Bool(lhs && rhs),
            (Or, Bool(lhs), Bool(rhs)) => Bool(lhs || rhs),
            (Add, String(lhs), String(rhs)) => String(lhs + &rhs),
            (op, lhs, rhs) => {
                return Err(Error::UnsupportedOp {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    operator: *op,
                    span,
                })
            }
        };

        Ok(value)
    }

    fn eval_assign(&mut self, assign: &Assign) -> Result<Value> {
        let value = self.eval_expr(&assign.value)?;
        self.vars.insert(assign.name.clone(), value);
        Ok(Value::Void)
    }

    fn eval_var(&mut self, name: &str, span: Span) -> Result<Value> {
        self.vars
            .get(name)
            .cloned()
            .ok_or(Error::UndefinedVariable(name.to_owned(), span))
    }

    fn eval_if(&mut self, _if: &If, _span: Span) -> Result<Value> {
        match self.eval_expr(&_if.cond)? {
            Value::Bool(true) => self.eval_expr(&_if.body),
            Value::Bool(false) => match _if.branch.as_deref() {
                Some(IfBranch::IfElse(_if)) => self.eval_if(_if, _span),
                Some(IfBranch::Else(expr)) => self.eval_expr(expr),
                None => Ok(Value::Void),
            },
            value => Err(Error::TypeMismatch {
                expected: Type::Bool,
                actual: value,
            }),
        }
    }

    fn eval_block(&mut self, block: &Block, _span: Span) -> Result<Value> {
        let len = block.0.len();
        for (i, expr) in block.0.iter().enumerate() {
            let result = self.eval_expr(expr)?;

            if i == len - 1 {
                return Ok(result);
            }
        }

        // block was empty
        Ok(Value::Void)
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
    #[display("bool")]
    Bool,
}

#[derive(Debug, Clone, Display, PartialEq)]
pub enum Value {
    Void,
    String(String),
    Int(i32),
    Bool(bool),
}

impl Value {
    pub(crate) fn ty(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::String(_) => Type::String,
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
        }
    }
}
