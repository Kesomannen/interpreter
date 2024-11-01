use std::{iter::Peekable, sync::Arc};

pub mod ast;
use ast::*;

mod error;
pub use error::{Error, Result};

use crate::{
    span::Span,
    tokenize::{self, BinOperator, Delim, Keyword, Token, TokenKind, UnOperator},
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

    fn peek_kind(&mut self) -> Result<Option<&TokenKind>> {
        match self.peek_ok() {
            Ok(Some(token)) => Ok(Some(&token.kind)),
            Ok(None) => Ok(None),
            Err(err) => Err(err),
        }
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

    fn parse_ident(&mut self) -> Result<String> {
        let next = self.next()?;
        match next.kind {
            TokenKind::Ident(ident) => Ok(ident),
            _ => Err(Error::UnexpectedToken {
                expected: "identifier".into(),
                actual: next,
            }),
        }
    }

    fn parse_list<F, U>(
        &mut self,
        open: TokenKind,
        sep: TokenKind,
        close: TokenKind,
        mut parse: F,
    ) -> Result<(Vec<U>, Span)>
    where
        F: FnMut(&mut Self) -> Result<U>,
    {
        let start = self.expect(&open)?.span.start;
        let mut values = Vec::new();

        let end = loop {
            if values.is_empty() {
                if self.peek()?.kind == close {
                    break self.next().unwrap().span.end;
                }
            } else {
                let next = self.next()?;

                if next.kind == close {
                    break next.span.end;
                } else if next.kind == sep {
                    // allow trailing separators
                    if self.peek()?.kind == close {
                        break self.next().unwrap().span.end;
                    }
                } else {
                    return Err(Error::UnexpectedToken {
                        expected: format!("'{}' or '{}'", close, sep).into(),
                        actual: next,
                    });
                }
            }

            values.push(parse(self)?);
        };

        let span = Span::new(start, end);
        Ok((values, span))
    }

    fn parse_list_delim<F, U>(
        &mut self,
        delim: Delim,
        sep: TokenKind,
        parse: F,
    ) -> Result<(Vec<U>, Span)>
    where
        F: FnMut(&mut Self) -> Result<U>,
    {
        self.parse_list(
            TokenKind::OpenDelim(delim),
            sep,
            TokenKind::CloseDelim(delim),
            parse,
        )
    }

    pub fn parse_file(&mut self) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();

        while self.peek_ok()?.is_some() {
            exprs.push(self.parse_expr()?);
            self.expect(&TokenKind::Semicolon)?;
        }

        Ok(exprs)
    }

    fn parse_box_expr(&mut self) -> Result<Box<Expr>> {
        self.parse_expr().map(Box::new)
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        self._parse_expr(0)
    }

    fn _parse_expr(&mut self, precedence: usize) -> Result<Expr> {
        let mut lhs = self.parse_simple_expr()?;

        loop {
            let Some(next) = self.peek_ok()? else { break };
            let Some(operator) = BinOperator::from_token(&next.kind) else {
                break;
            };

            if operator.precedence() < precedence {
                break;
            }

            self.eat();

            let rhs = self._parse_expr(operator.precedence() + 1)?;
            let span = Span::merge(&lhs.span, &rhs.span);

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
        let mut un_ops = Vec::new();

        loop {
            let Some(next) = self.peek_kind()? else {
                break;
            };

            if let Some(op) = UnOperator::from_token(&next) {
                un_ops.push(op);
                self.eat();
                continue;
            }

            break;
        }

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
            TokenKind::Keyword(Keyword::Void) => (ExprKind::Void, Eat::Yes),
            TokenKind::String(_) => match self.next().map(|token| token.kind) {
                Ok(TokenKind::String(str)) => (ExprKind::String(str), Eat::No),
                _ => unreachable!(),
            },
            TokenKind::Keyword(Keyword::If) => {
                let _if = self.parse_if()?;

                span.extend_with(&_if.body.span);

                (ExprKind::If(_if), Eat::No)
            }
            TokenKind::Keyword(Keyword::While) => {
                let (cond, body) = self.parse_stmt(Keyword::While)?;

                span.extend_with(&body.span);

                (ExprKind::While(While { cond, body }), Eat::No)
            }
            TokenKind::OpenDelim(Delim::Brace) => {
                let (block, block_span) = self.parse_block()?;

                span.extend_with(&block_span);

                (ExprKind::Block(block), Eat::No)
            }
            TokenKind::Pipe => {
                let (args, _) =
                    self.parse_list(TokenKind::Pipe, TokenKind::Comma, TokenKind::Pipe, |this| {
                        this.parse_ident()
                    })?;

                let body = self.parse_box_expr()?;

                span.extend_with(&body.span);

                (ExprKind::Func(Arc::new(Func { args, body })), Eat::No)
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;

                let kind = match self.peek_kind()? {
                    Some(TokenKind::Equals) => {
                        self.eat();

                        let value = self.parse_box_expr()?;

                        span.extend_with(&value.span);

                        ExprKind::Assign(Assign { name, value })
                    }
                    _ => ExprKind::Ident(name),
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
            self.eat();
        }

        let mut expr = Expr {
            id: self.next_id(),
            span: span.clone(),
            kind,
        };

        while let Some(TokenKind::OpenDelim(Delim::Paren)) = self.peek_kind()? {
            let (args, args_span) =
                self.parse_list_delim(Delim::Paren, TokenKind::Comma, |this| this.parse_expr())?;

            let span = Span::merge(&expr.span, &args_span);

            expr = Expr {
                id: self.next_id(),
                span,
                kind: ExprKind::Call(Call {
                    func: Box::new(expr),
                    args,
                }),
            }
        }

        for un_op in un_ops.into_iter().rev() {
            expr = Expr {
                id: self.next_id(),
                span: span.clone(),
                kind: ExprKind::UnOp(UnOp {
                    operator: un_op,
                    operand: Box::new(expr),
                }),
            }
        }

        Ok(expr)
    }

    fn parse_if(&mut self) -> Result<If> {
        let (cond, body) = self.parse_stmt(Keyword::If)?;

        let branch = match self.peek_kind()? {
            Some(TokenKind::Keyword(Keyword::Else)) => {
                self.eat();

                let branch = match self.peek_kind()? {
                    Some(TokenKind::Keyword(Keyword::If)) => IfBranch::IfElse(self.parse_if()?),
                    _ => IfBranch::Else(self.parse_expr()?),
                };

                Some(Box::new(branch))
            }
            _ => None,
        };

        Ok(If { cond, body, branch })
    }

    fn parse_stmt(&mut self, keyword: Keyword) -> Result<(Box<Expr>, Box<Expr>)> {
        self.expect(&TokenKind::Keyword(keyword))?;

        self.expect(&TokenKind::OpenDelim(Delim::Paren))?;

        let cond = self.parse_box_expr()?;

        self.expect(&TokenKind::CloseDelim(Delim::Paren))?;

        let body = self.parse_box_expr()?;

        Ok((cond, body))
    }

    fn parse_block(&mut self) -> Result<(Block, Span)> {
        let (exprs, span) =
            self.parse_list_delim(Delim::Brace, TokenKind::Semicolon, |this| this.parse_expr())?;

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
