use crate::{span::Span, tokenize::BinOperator};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NodeId(pub usize);

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    String(String),
    Int(i32),
    Call(Call),
    BinOp(BinOp),
    Assign(Assign),
    Var(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinOp {
    pub operator: BinOperator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}
