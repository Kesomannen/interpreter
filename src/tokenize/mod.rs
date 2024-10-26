use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use crate::span::Span;

mod error;
use derive_more::derive::Display;
pub use error::{Error, Result};
use strum_macros::EnumString;

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
    Int(i32),
    Keyword(Keyword),
    OpenDelim(Delim),
    CloseDelim(Delim),
    BinOperator(BinOperator),
    Semicolon,
    Equals,
    Comma,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Delim {
    Paren,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
pub enum BinOperator {
    #[display("add")]
    Add,
    #[display("subtract")]
    Sub,
    #[display("multiply")]
    Mul,
    #[display("divide")]
    Div,
    #[display("or")]
    Or,
    #[display("and")]
    And,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum Keyword {
    True,
    False,
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

        loop {
            if !self.chars.peek()?.is_whitespace() {
                break;
            }

            self.next().unwrap();
        }

        let kind = match self.next().unwrap() {
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '=' => TokenKind::Equals,
            '(' => TokenKind::OpenDelim(Delim::Paren),
            ')' => TokenKind::CloseDelim(Delim::Paren),
            '+' => TokenKind::BinOperator(BinOperator::Add),
            '-' => TokenKind::BinOperator(BinOperator::Sub),
            '*' => TokenKind::BinOperator(BinOperator::Mul),
            '/' => TokenKind::BinOperator(BinOperator::Div),
            '&' => {}
            '"' => {
                let str = self.collect_while(None, |c| c != '"');
                self.next(); // eat the close "
                TokenKind::String(str)
            }
            c if c.is_numeric() => {
                let str = self.collect_while(Some(c), |c| c.is_numeric());
                let int = match str.parse() {
                    Ok(int) => int,
                    Err(err) => return Some(Err(Error::InvalidNumber(str, err))),
                };
                TokenKind::Int(int)
            }
            c if is_valid_ident(c) => {
                let ident = self.collect_while(Some(c), is_valid_ident);

                match Keyword::from_str(&ident) {
                    Ok(keyword) => TokenKind::Keyword(keyword),
                    Err(_) => TokenKind::Ident(ident),
                }
            }
            c => return Some(Err(Error::UnexpectedChar(c, start))),
        };

        let span = Span::new(start, self.loc);
        Some(Ok(Token { kind, span }))
    }
}

fn is_valid_ident(c: char) -> bool {
    !c.is_whitespace() && (c == '_' || !c.is_ascii_punctuation())
}
