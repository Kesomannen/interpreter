use std::sync::Arc;

use derive_more::derive::Display;

use crate::{span::Span, tokenize::BinOperator};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NodeId(pub usize);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    String(String),
    Int(i32),
    Bool(bool),
    Call(Call),
    BinOp(BinOp),
    Assign(Assign),
    Ident(String),
    If(If),
    Block(Block),
    Func(Arc<Func>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinOp {
    pub operator: BinOperator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub body: Box<Expr>,
    pub branch: Option<Box<IfBranch>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IfBranch {
    IfElse(If),
    Else(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block(pub Vec<Expr>);

#[derive(Debug, PartialEq, Eq, Display, Clone)]
#[display("<func>")]
pub struct Func {
    pub args: Vec<String>,
    pub body: Box<Expr>,
}
