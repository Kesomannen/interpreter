use crate::span::Span;
use std::fmt::Debug;
use tokenize::{BinOperator, Delim, Keyword, TokenKind};

use super::*;

#[test]
fn check_parse_string() {
    assert_parse(
        [Token {
            span: Span::new(0, 11),
            kind: TokenKind::String("hello, world".into()),
        }],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 11),
            kind: ExprKind::String("hello, world".into()),
        },
    );
}

#[test]
fn check_parse_int() {
    assert_parse(
        [Token {
            span: Span::new(0, 3),
            kind: TokenKind::Int(123),
        }],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 3),
            kind: ExprKind::Int(123),
        },
    );
}

#[test]
fn check_parse_bool() {
    assert_parse(
        [Token {
            span: Span::new(0, 4),
            kind: TokenKind::Keyword(Keyword::True),
        }],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 4),
            kind: ExprKind::Bool(true),
        },
    );

    assert_parse(
        [Token {
            span: Span::new(0, 5),
            kind: TokenKind::Keyword(Keyword::False),
        }],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 5),
            kind: ExprKind::Bool(false),
        },
    );
}

#[test]
fn check_parse_call() {
    assert_parse(
        [
            Token {
                span: Span::new(0, 5),
                kind: TokenKind::Ident("print".into()),
            },
            Token {
                span: Span::new(5, 6),
                kind: TokenKind::OpenDelim(Delim::Paren),
            },
            Token {
                span: Span::new(6, 17),
                kind: TokenKind::String("hello, world".into()),
            },
            Token {
                span: Span::new(17, 18),
                kind: TokenKind::CloseDelim(Delim::Paren),
            },
        ],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 18),
            kind: ExprKind::Call(Call {
                name: "print".into(),
                args: vec![Expr {
                    id: NodeId(1),
                    span: Span::new(6, 17),
                    kind: ExprKind::String("hello, world".into()),
                }],
            }),
        },
    );
}

#[test]
fn check_parse_assign() {
    assert_parse(
        [
            Token {
                span: Span::new(0, 3),
                kind: TokenKind::Ident("foo".into()),
            },
            Token {
                span: Span::new(3, 4),
                kind: TokenKind::Equals,
            },
            Token {
                span: Span::new(4, 5),
                kind: TokenKind::Int(7),
            },
        ],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 5),
            kind: ExprKind::Assign(Assign {
                name: "foo".into(),
                value: Box::new(Expr {
                    id: NodeId(1),
                    span: Span::new(4, 5),
                    kind: ExprKind::Int(7),
                }),
            }),
        },
    );
}

#[test]
fn check_parse_var() {
    assert_parse(
        [Token {
            span: Span::new(0, 7),
            kind: TokenKind::Ident("foo_bar".into()),
        }],
        |parser| parser.parse_expr(),
        Expr {
            id: NodeId(0),
            span: Span::new(0, 7),
            kind: ExprKind::Var("foo_bar".into()),
        },
    );
}

#[test]
fn check_parse_binop() {
    assert_parse(
        [
            Token {
                span: Span::new(0, 2),
                kind: TokenKind::Int(20),
            },
            Token {
                span: Span::new(2, 3),
                kind: TokenKind::BinOperator(BinOperator::Sub),
            },
            Token {
                span: Span::new(3, 5),
                kind: TokenKind::Int(15),
            },
            Token {
                span: Span::new(5, 6),
                kind: TokenKind::BinOperator(BinOperator::Div),
            },
            Token {
                span: Span::new(6, 7),
                kind: TokenKind::Int(3),
            },
        ],
        |parser| parser.parse_expr(),
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
    );
}

struct TestTokenStream<'a> {
    inner: Box<dyn Iterator<Item = Token> + 'a>,
}

impl<'a> Iterator for TestTokenStream<'a> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Ok)
    }
}

fn assert_parse<T, F, R>(tokens: T, f: F, expected: R)
where
    T: IntoIterator<Item = Token>,
    F: FnOnce(&mut Parser<TestTokenStream>) -> Result<R>,
    R: PartialEq + Debug,
{
    let tokens = TestTokenStream {
        inner: Box::new(tokens.into_iter()),
    };

    let mut parser = Parser {
        node_id: 0,
        tokens: tokens.peekable(),
    };

    assert_eq!(f(&mut parser).unwrap(), expected)
}
