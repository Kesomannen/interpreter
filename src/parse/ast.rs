use crate::span::Span;

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
    Call(Call),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expr>,
}
