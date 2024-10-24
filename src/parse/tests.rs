use crate::span::Span;
use std::fmt::Debug;
use tokenize::TokenKind;

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
fn check_parse_call() {
    assert_parse(
        [
            Token {
                span: Span::new(0, 5),
                kind: TokenKind::Ident("print".into()),
            },
            Token {
                span: Span::new(5, 6),
                kind: TokenKind::OpenParen,
            },
            Token {
                span: Span::new(6, 17),
                kind: TokenKind::String("hello, world".into()),
            },
            Token {
                span: Span::new(17, 18),
                kind: TokenKind::CloseParen,
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
