use super::*;
use crate::tokenize::BinOperator;

#[test]
fn check_eval_string() {
    assert_eval(
        Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::String("sus".into()),
        },
        Value::String("sus".into()),
    );
}

#[test]
fn check_eval_int() {
    assert_eval(
        Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::Int(95500),
        },
        Value::Int(95500),
    );
}

#[test]
fn check_eval_bool() {
    assert_eval(
        Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::Bool(true),
        },
        Value::Bool(true),
    );

    assert_eval(
        Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::Bool(false),
        },
        Value::Bool(false),
    );
}

#[test]
fn check_eval_int_binop() {
    assert_eval(
        Expr {
            id: NodeId(4),
            span: Span::new(0, 7),
            kind: ExprKind::BinOp(BinOp {
                operator: BinOperator::Sub,
                lhs: Box::new(Expr {
                    id: NodeId(0),
                    span: Span::new(0, 2),
                    kind: ExprKind::Int(20),
                }),
                rhs: Box::new(Expr {
                    id: NodeId(3),
                    span: Span::new(3, 7),
                    kind: ExprKind::BinOp(BinOp {
                        operator: BinOperator::Div,
                        lhs: Box::new(Expr {
                            id: NodeId(1),
                            span: Span::new(3, 5),
                            kind: ExprKind::Int(15),
                        }),
                        rhs: Box::new(Expr {
                            id: NodeId(2),
                            span: Span::new(6, 7),
                            kind: ExprKind::Int(3),
                        }),
                    }),
                }),
            }),
        },
        Value::Int(15),
    );
}

#[test]
fn check_eval_str_binop() {
    assert_eval(
        Expr {
            id: NodeId(2),
            span: Span::new(0, 6),
            kind: ExprKind::BinOp(BinOp {
                operator: BinOperator::Add,
                lhs: Box::new(Expr {
                    id: NodeId(0),
                    span: Span::new(0, 3),
                    kind: ExprKind::String("foo".into()),
                }),
                rhs: Box::new(Expr {
                    id: NodeId(1),
                    span: Span::new(3, 6),
                    kind: ExprKind::String(" bar".into()),
                }),
            }),
        },
        Value::String("foo bar".into()),
    );
}

#[test]
fn check_var() {
    let mut executor = Executor::new();
    let value = executor
        .eval_expr(&Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::Assign(Assign {
                name: "foo".into(),
                value: Box::new(Expr {
                    id: NodeId(1),
                    span: Span::new(4, 5),
                    kind: ExprKind::Int(7),
                }),
            }),
        })
        .unwrap();

    assert_eq!(value, Value::Void);

    assert_eq!(
        executor
            .eval_expr(&Expr {
                id: NodeId(0),
                span: Span::default(),
                kind: ExprKind::Var("foo".into()),
            })
            .unwrap(),
        Value::Int(7)
    );

    executor
        .eval_expr(&Expr {
            id: NodeId(0),
            span: Span::default(),
            kind: ExprKind::Assign(Assign {
                name: "foo".into(),
                value: Box::new(Expr {
                    id: NodeId(1),
                    span: Span::new(4, 5),
                    kind: ExprKind::Int(77),
                }),
            }),
        })
        .unwrap();

    assert_eq!(
        executor
            .eval_expr(&Expr {
                id: NodeId(0),
                span: Span::default(),
                kind: ExprKind::Var("foo".into()),
            })
            .unwrap(),
        Value::Int(77)
    );
}

fn assert_eval(expr: Expr, expected: Value) {
    assert_eq!(Executor::new().eval_expr(&expr).unwrap(), expected);
}
