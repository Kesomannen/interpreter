use std::iter::Peekable;

pub mod ast;
use ast::*;

mod error;
pub use error::{Error, Result};

use crate::{
    span::Span,
    tokenize::{self, Token, TokenKind},
};

#[cfg(test)]
mod tests;

type TokenResult = tokenize::Result<Token>;

pub fn parse<T>(tokens: T) -> Result<Expr>
where
    T: Iterator<Item = TokenResult>,
{
    let mut parser = Parser {
        node_id: 0,
        tokens: tokens.peekable(),
    };

    parser.parse_expr()
}

pub struct Parser<T>
where
    T: Iterator<Item = TokenResult>,
{
    node_id: usize,
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = TokenResult>,
{
    fn next_id(&mut self) -> NodeId {
        self.node_id += 1;
        NodeId(self.node_id - 1)
    }

    fn peek_ok(&mut self) -> Result<Option<&Token>> {
        self.tokens
            .peek()
            .map(|res| res.as_ref().map_err(|err| Error::Tokenize(err.clone())))
            .transpose()
    }

    fn peek(&mut self) -> Result<&Token> {
        self.peek_ok()
            .map(|token| token.ok_or(Error::UnexpectedEof))?
    }

    fn next_ok(&mut self) -> Result<Option<Token>> {
        self.tokens.next().transpose().map_err(Error::Tokenize)
    }

    fn next(&mut self) -> Result<Token> {
        self.next_ok()
            .map(|token| token.ok_or(Error::UnexpectedEof))?
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Token> {
        let next = self.next()?;
        if next.kind == *kind {
            Ok(next)
        } else {
            Err(Error::UnexpectedToken {
                expected: format!("{kind:?}").into(),
                actual: next,
            })
        }
    }

    fn parse_value_list<F, U>(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        end: TokenKind,
        mut parse: F,
    ) -> Result<(Vec<U>, Span)>
    where
        F: FnMut(&mut Self) -> Result<U>,
    {
        let start_loc = self.expect(&start)?.span.start;
        let mut values = Vec::new();

        let end_loc = loop {
            if self.peek()?.kind == end {
                break self.next()?.span.end;
            }

            if !values.is_empty() {
                self.expect(&sep)?;
            }

            values.push(parse(self)?);
        };

        let span = Span::new(start_loc, end_loc);
        Ok((values, span))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        let id = self.next_id();

        let Token {
            kind: token_kind,
            mut span,
        } = self.next()?;

        let kind = match token_kind {
            TokenKind::String(str) => ExprKind::String(str),
            TokenKind::Ident(name) => {
                let (args, args_span) = self.parse_value_list(
                    TokenKind::OpenParen,
                    TokenKind::Comma,
                    TokenKind::CloseParen,
                    |this| this.parse_expr(),
                )?;

                span.extend_with(args_span);

                ExprKind::Call(Call { name, args })
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "expression".into(),
                    actual: self.next().unwrap(),
                })
            }
        };

        Ok(Expr { id, span, kind })
    }
}
