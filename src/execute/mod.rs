use crate::{parse::ast::*, span::Span};
use derive_more::derive::Display;
use func::RuntimeFunc;
use std::{collections::HashMap, sync::Arc};

mod error;
mod func;
pub use error::{Error, Result};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Executor {
    scopes: Vec<Scope>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
struct Scope {
    vars: HashMap<String, Value>,
}

impl Scope {
    fn builtin() -> Self {
        let vars = func::builtins()
            .into_iter()
            .map(|builtin| {
                (
                    builtin.name().to_owned(),
                    Value::Func(RuntimeFunc::Builtin(builtin)),
                )
            })
            .collect();

        Self { vars }
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::builtin(), Scope::default()],
        }
    }

    #[allow(unused)]
    fn curr_scope(&self) -> &Scope {
        self.scopes
            .last()
            .expect("the global scope should always be present")
    }

    fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("the global scope should always be present")
    }

    pub(crate) fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        let Expr { span, kind, .. } = expr;
        let span = span.clone();

        match kind {
            ExprKind::String(str) => Ok(Value::String(str.clone())),
            ExprKind::Int(int) => Ok(Value::Int(*int)),
            ExprKind::Bool(bool) => Ok(Value::Bool(*bool)),
            ExprKind::Void => Ok(Value::Void),
            ExprKind::Call(call) => self.eval_call(call, span),
            ExprKind::BinOp(bin_op) => self.eval_bin_op(bin_op, span),
            ExprKind::UnOp(un_op) => self.eval_un_op(un_op, span),
            ExprKind::Assign(assign) => self.eval_assign(assign),
            ExprKind::Ident(name) => self.eval_var(name).cloned(),
            ExprKind::If(_if) => self.eval_if(_if, span),
            ExprKind::While(_while) => self.eval_while(_while, span),
            ExprKind::Block(block) => self.eval_block(block, span),
            ExprKind::Func(func) => self.eval_func(func, span),
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
                return Err(Error::UnsupportedBinOp {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    operator: *op,
                    span,
                })
            }
        };

        Ok(value)
    }

    fn eval_un_op(&mut self, un_op: &UnOp, span: Span) -> Result<Value> {
        use crate::tokenize::UnOperator::*;
        use Value::*;

        let UnOp { operator, operand } = un_op;

        let operand = self.eval_expr(operand)?;

        let value = match (operator, operand) {
            (Not, Bool(bool)) => Bool(!bool),
            (Neg, Int(int)) => Int(-int),
            (op, operand) => {
                return Err(Error::UnsupportedUnOp {
                    operand: operand.ty(),
                    operator: *op,
                    span,
                })
            }
        };

        Ok(value)
    }

    fn eval_assign(&mut self, assign: &Assign) -> Result<Value> {
        let value = self.eval_expr(&assign.value)?;
        self.curr_scope_mut()
            .vars
            .insert(assign.name.clone(), value);

        Ok(Value::Void)
    }

    fn eval_var<'a>(&'a mut self, name: &str) -> Result<&'a Value> {
        self.scopes
            .iter()
            .filter_map(|scope| scope.vars.get(name))
            .last() // prioritize closer scopes
            .ok_or(Error::UndefinedVariable(name.to_owned()))
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
                actual: value.ty(),
            }),
        }
    }

    fn eval_while(&mut self, _while: &While, _span: Span) -> Result<Value> {
        loop {
            match self.eval_expr(&_while.cond)? {
                Value::Bool(true) => (),
                Value::Bool(false) => break,
                value => {
                    return Err(Error::TypeMismatch {
                        expected: Type::Bool,
                        actual: value.ty(),
                    })
                }
            };

            self.eval_expr(&_while.body)?;
        }

        Ok(Value::Void)
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

    fn eval_func(&mut self, func: &Arc<Func>, _span: Span) -> Result<Value> {
        let mut captures = Vec::new();

        self.visit(&func.body, func, &mut captures);

        Ok(Value::Func(RuntimeFunc::User {
            func: func.clone(),
            captures: captures.into(),
        }))
    }

    fn visit(&mut self, expr: &Expr, func: &Func, captures: &mut Vec<(String, Value)>) {
        match &expr.kind {
            ExprKind::Call(call) => {
                self.visit(&call.func, func, captures);
                for arg in &call.args {
                    self.visit(arg, func, captures);
                }
            }
            ExprKind::BinOp(bin_op) => {
                self.visit(&bin_op.lhs, func, captures);
                self.visit(&bin_op.rhs, func, captures);
            }
            ExprKind::Assign(assign) => {
                self.visit(&assign.value, func, captures);
            }
            ExprKind::Ident(name) => {
                if func.args.iter().any(|arg| arg == name) {
                    return; // variable is an argument
                }

                if let Ok(var) = self.eval_var(name) {
                    captures.push((name.to_owned(), var.clone()));
                }
            }
            ExprKind::If(_if) => {
                self.visit(&_if.cond, func, captures);
                self.visit(&_if.body, func, captures);
            }
            ExprKind::Block(block) => {
                for expr in &block.0 {
                    self.visit(expr, func, captures);
                }
            }
            ExprKind::Func(_) => (), // higher depth-capturing is not supported
            _ => (),
        }
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
    #[display("func")]
    Func,
}

#[derive(Debug, Clone, Display, PartialEq)]
pub enum Value {
    #[display("void")]
    Void,
    String(String),
    Int(i32),
    Bool(bool),
    Func(RuntimeFunc),
}

impl Value {
    pub(crate) fn ty(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::String(_) => Type::String,
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::Func { .. } => Type::Func,
        }
    }
}
