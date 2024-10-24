use std::iter::Peekable;

pub mod ast;
use ast::*;

mod error;
pub use error::{Error, Result};

use crate::{
    span::Span,
    tokenize::{self, BinOperator, Delim, Token, TokenKind},
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
        delim: Delim,
        sep: TokenKind,
        mut parse: F,
    ) -> Result<(Vec<U>, Span)>
    where
        F: FnMut(&mut Self) -> Result<U>,
    {
        let start = self.expect(&TokenKind::OpenDelim(delim))?.span.start;
        let mut values = Vec::new();

        let end = loop {
            if self.peek()?.kind == TokenKind::CloseDelim(delim) {
                break self.next()?.span.end;
            }

            if !values.is_empty() {
                self.expect(&sep)?;
            }

            values.push(parse(self)?);
        };

        let span = Span::new(start, end);
        Ok((values, span))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self._parse_expr(0)
    }

    fn _parse_expr(&mut self, precedence: usize) -> Result<Expr> {
        let mut lhs = self.parse_simple_expr()?;

        loop {
            let Some(next) = self.peek_ok()? else { break };
            let TokenKind::BinOperator(operator) = next.kind else {
                break;
            };

            if operator.precedence() < precedence {
                break;
            }

            self.next()?;

            let rhs = self._parse_expr(operator.precedence() + 1)?;
            let span = Span::merge(lhs.span.clone(), rhs.span.clone());

            lhs = Expr {
                id: self.next_id(),
                span,
                kind: ExprKind::BinOp(BinOp {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
            };
        }

        Ok(lhs)
    }

    fn parse_simple_expr(&mut self) -> Result<Expr> {
        let id = self.next_id();

        let Token {
            kind: token_kind,
            mut span,
        } = self.next()?;

        let kind = match token_kind {
            TokenKind::String(str) => ExprKind::String(str),
            TokenKind::Int(int) => ExprKind::Int(int),
            TokenKind::Ident(name) => match self.peek_ok()?.map(|token| &token.kind) {
                Some(TokenKind::OpenDelim(Delim::Paren)) => {
                    let (args, args_span) =
                        self.parse_value_list(Delim::Paren, TokenKind::Comma, |this| {
                            this.parse_expr()
                        })?;

                    span.extend_with(&args_span);

                    ExprKind::Call(Call { name, args })
                }
                Some(TokenKind::Equals) => {
                    self.next().unwrap();

                    let value = self.parse_expr()?;
                    span.extend_with(&value.span);
                    ExprKind::Assign(Assign {
                        name,
                        value: Box::new(value),
                    })
                }
                _ => ExprKind::Var(name),
            },
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

impl BinOperator {
    pub fn precedence(&self) -> usize {
        match self {
            Self::Add | Self::Sub => 1,
            Self::Mul | Self::Div => 2,
        }
    }
}
