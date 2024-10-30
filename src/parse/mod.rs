use std::iter::Peekable;

pub mod ast;
use ast::*;

mod error;
pub use error::{Error, Result};

use crate::{
    span::Span,
    tokenize::{self, BinOperator, Delim, Keyword, Token, TokenKind},
};

#[cfg(test)]
mod tests;

type TokenResult = tokenize::Result<Token>;

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
    pub fn new(tokens: T) -> Self {
        Parser {
            node_id: 0,
            tokens: tokens.peekable(),
        }
    }

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

    fn eat(&mut self) {
        self.next()
            .map_err(|err| {
                eprintln!("a token error was eaten: {err}");
            })
            .ok();
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Token> {
        let next = self.next()?;
        if next.kind == *kind {
            Ok(next)
        } else {
            Err(Error::UnexpectedToken {
                expected: format!("'{kind}'").into(),
                actual: next,
            })
        }
    }

    fn parse_list<F, U>(
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
            if values.is_empty() {
                if self.peek()?.kind == TokenKind::CloseDelim(delim) {
                    break self.next().unwrap().span.end;
                }
            } else {
                let next = self.next()?;

                if next.kind == TokenKind::CloseDelim(delim) {
                    break next.span.end;
                } else if next.kind == sep {
                    // allow trailing separators
                    if self.peek()?.kind == TokenKind::CloseDelim(delim) {
                        break self.next().unwrap().span.end;
                    }
                } else {
                    return Err(Error::UnexpectedToken {
                        expected: format!("'{}' or '{}'", delim.to_str(false), sep).into(),
                        actual: next,
                    });
                }
            }

            values.push(parse(self)?);
        };

        let span = Span::new(start, end);
        Ok((values, span))
    }

    pub fn parse_file(&mut self) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();

        while self.peek_ok()?.is_some() {
            exprs.push(self.parse_expr()?);
            self.expect(&TokenKind::Semicolon)?;
        }

        Ok(exprs)
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
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

        let Token { kind, span } = self.peek()?;
        let mut span = span.clone();

        enum Eat {
            Yes,
            No,
        }

        let (kind, eat) = match kind {
            TokenKind::Int(int) => (ExprKind::Int(*int), Eat::Yes),
            TokenKind::Keyword(Keyword::True) => (ExprKind::Bool(true), Eat::Yes),
            TokenKind::Keyword(Keyword::False) => (ExprKind::Bool(false), Eat::Yes),
            TokenKind::String(_) => match self.next().map(|token| token.kind) {
                Ok(TokenKind::String(str)) => (ExprKind::String(str), Eat::No),
                _ => unreachable!(),
            },
            TokenKind::Keyword(Keyword::If) => {
                let _if = self.parse_if()?;

                //span.extend_with(&body.span);

                (ExprKind::If(_if), Eat::No)
            }
            TokenKind::OpenDelim(Delim::Brace) => {
                let (block, block_span) = self.parse_block()?;

                span.extend_with(&block_span);

                (ExprKind::Block(block), Eat::No)
            }
            TokenKind::Ident(_) => {
                let name = match self.next().map(|token| token.kind) {
                    Ok(TokenKind::Ident(name)) => name,
                    _ => unreachable!(),
                };

                let kind = match self.peek_ok()?.map(|token| &token.kind) {
                    Some(TokenKind::OpenDelim(Delim::Paren)) => {
                        let (args, args_span) =
                            self.parse_list(Delim::Paren, TokenKind::Comma, |this| {
                                this.parse_expr()
                            })?;

                        span.extend_with(&args_span);

                        ExprKind::Call(Call { name, args })
                    }
                    Some(TokenKind::Equals) => {
                        self.eat();

                        let value = self.parse_expr()?;

                        span.extend_with(&value.span);

                        ExprKind::Assign(Assign {
                            name,
                            value: Box::new(value),
                        })
                    }
                    _ => ExprKind::Var(name),
                };

                (kind, Eat::No)
            }
            _ => {
                let kind = self.next().unwrap().kind;

                return Err(Error::UnexpectedToken {
                    expected: "expression".into(),
                    actual: Token { kind, span },
                });
            }
        };

        if let Eat::Yes = eat {
            self.next().ok();
        }

        Ok(Expr { id, span, kind })
    }

    fn parse_if(&mut self) -> Result<If> {
        self.expect(&TokenKind::Keyword(Keyword::If))?;
        self.expect(&TokenKind::OpenDelim(Delim::Paren))?;
        let cond = Box::new(self.parse_expr()?);
        self.expect(&TokenKind::CloseDelim(Delim::Paren))?;

        let body = Box::new(self.parse_expr()?);

        let branch = match self.peek_ok()?.map(|token| &token.kind) {
            Some(TokenKind::Keyword(Keyword::Else)) => {
                self.eat();

                let branch = match self.peek_ok()?.map(|token| &token.kind) {
                    Some(TokenKind::Keyword(Keyword::If)) => IfBranch::IfElse(self.parse_if()?),
                    _ => IfBranch::Else(Box::new(self.parse_expr()?)),
                };

                Some(Box::new(branch))
            }
            _ => None,
        };

        Ok(If { cond, body, branch })
    }

    fn parse_block(&mut self) -> Result<(Block, Span)> {
        let (exprs, span) =
            self.parse_list(Delim::Brace, TokenKind::Semicolon, |this| this.parse_expr())?;

        Ok((Block(exprs), span))
    }
}

impl BinOperator {
    pub fn precedence(&self) -> usize {
        match self {
            Self::Mul | Self::Div => 4,
            Self::Add | Self::Sub => 3,
            Self::Eq | Self::Ne | Self::Gt | Self::Gte | Self::Lt | Self::Lte => 2,
            Self::Or | Self::And => 1,
        }
    }
}
