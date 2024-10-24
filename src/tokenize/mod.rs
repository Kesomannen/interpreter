use std::{iter::Peekable, str::Chars};

use crate::span::Span;

mod error;
pub use error::{Error, Result};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Ident(String),
    String(String),
    OpenParen,
    CloseParen,
    Semicolon,
    Comma,
}

pub struct Tokenizer<'a> {
    loc: usize,
    chars: Peekable<Chars<'a>>,
}

pub fn tokenize(src: &str) -> Tokenizer<'_> {
    Tokenizer {
        loc: 0,
        chars: src.chars().peekable(),
    }
}

impl<'a> Tokenizer<'a> {
    fn next(&mut self) -> Option<char> {
        self.chars.next().inspect(|_| {
            self.loc += 1;
        })
    }

    fn collect_while<P>(&mut self, first: Option<char>, mut pred: P) -> String
    where
        P: FnMut(char) -> bool,
    {
        let mut buf = first.map(String::from).unwrap_or_default();

        loop {
            let Some(next) = self.chars.peek() else { break };
            if !pred(*next) {
                break;
            }

            buf.push(self.next().unwrap());
        }

        buf
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.loc;

        let next = self.next()?;

        let kind = match next {
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '"' => {
                let str = self.collect_while(None, |c| c != '"');
                self.next(); // eat the close "
                TokenKind::String(str)
            }
            c if is_valid_ident(c) => {
                let ident = self.collect_while(Some(c), is_valid_ident);
                TokenKind::Ident(ident)
            }
            _ => return Some(Err(Error::UnexpectedChar(next, start))),
        };

        let span = Span::new(start, self.loc);
        Some(Ok(Token { kind, span }))
    }
}

fn is_valid_ident(c: char) -> bool {
    !matches!(c, '(' | ')' | ';' | '"')
}
