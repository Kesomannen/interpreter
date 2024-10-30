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

#[derive(Debug, PartialEq, Eq, Clone, Display)]
#[display("{}", kind)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Display)]
pub enum TokenKind {
    Ident(String),
    String(String),
    Int(i32),
    Keyword(Keyword),
    #[display("{}", _0.to_str(true))]
    OpenDelim(Delim),
    #[display("{}", _0.to_str(false))]
    CloseDelim(Delim),
    BinOperator(BinOperator),
    #[display(";")]
    Semicolon,
    #[display("=")]
    Equals,
    #[display(",")]
    Comma,
    #[display("|")]
    Pipe,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Delim {
    Paren,
    Brace,
}

impl Delim {
    pub fn to_str(self, open: bool) -> &'static str {
        match (self, open) {
            (Delim::Paren, true) => "(",
            (Delim::Paren, false) => ")",
            (Delim::Brace, true) => "{",
            (Delim::Brace, false) => "}",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
pub enum BinOperator {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("||")]
    Or,
    #[display("&&")]
    And,
    #[display("==")]
    Eq,
    #[display("!=")]
    Ne,
    #[display(">")]
    Gt,
    #[display(">=")]
    Gte,
    #[display("<")]
    Lt,
    #[display("<=")]
    Lte,
}

impl BinOperator {
    pub fn verb(&self) -> &'static str {
        match self {
            BinOperator::Add => "add",
            BinOperator::Sub => "subtract",
            BinOperator::Mul => "multiply",
            BinOperator::Div => "divide",
            BinOperator::Or => "or",
            BinOperator::And => "and",
            BinOperator::Eq
            | BinOperator::Ne
            | BinOperator::Gt
            | BinOperator::Gte
            | BinOperator::Lt
            | BinOperator::Lte => "compare",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, strum_macros::Display)]
#[strum(serialize_all = "camelCase")]
pub enum Keyword {
    True,
    False,
    If,
    Else,
    Return,
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
    fn next_char(&mut self) -> Option<char> {
        self.chars.next().inspect(|c| {
            self.loc += c.len_utf8();
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

            buf.push(self.next_char().unwrap());
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

            self.next_char().unwrap();
        }

        let kind = match (self.next_char().unwrap(), self.chars.peek()) {
            ('/', Some('/')) => {
                self.collect_while(None, |c| c != '\n');
                return self.next();
            }
            (';', _) => TokenKind::Semicolon,
            (',', _) => TokenKind::Comma,
            ('(', _) => TokenKind::OpenDelim(Delim::Paren),
            (')', _) => TokenKind::CloseDelim(Delim::Paren),
            ('{', _) => TokenKind::OpenDelim(Delim::Brace),
            ('}', _) => TokenKind::CloseDelim(Delim::Brace),
            ('+', _) => TokenKind::BinOperator(BinOperator::Add),
            ('-', _) => TokenKind::BinOperator(BinOperator::Sub),
            ('*', _) => TokenKind::BinOperator(BinOperator::Mul),
            ('/', _) => TokenKind::BinOperator(BinOperator::Div),
            ('!', Some('=')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::Ne)
            }
            ('=', Some('=')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::Eq)
            }
            ('=', _) => TokenKind::Equals,
            ('<', Some('=')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::Lte)
            }
            ('<', _) => TokenKind::BinOperator(BinOperator::Lt),
            ('>', Some('=')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::Gte)
            }
            ('>', _) => TokenKind::BinOperator(BinOperator::Gt),
            ('&', Some('&')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::And)
            }
            ('|', Some('|')) => {
                self.next_char();
                TokenKind::BinOperator(BinOperator::Or)
            }
            ('|', _) => TokenKind::Pipe,
            ('"', _) => {
                let str = self.collect_while(None, |c| c != '"');
                self.next_char(); // eat the close "
                TokenKind::String(str)
            }
            (c, _) if c.is_numeric() => {
                let str = self.collect_while(Some(c), |c| c.is_numeric());
                let int = match str.parse() {
                    Ok(int) => int,
                    Err(err) => return Some(Err(Error::InvalidNumber(str, err))),
                };
                TokenKind::Int(int)
            }
            (c, _) if is_valid_ident(c) => {
                let ident = self.collect_while(Some(c), is_valid_ident);

                match Keyword::from_str(&ident) {
                    Ok(keyword) => TokenKind::Keyword(keyword),
                    Err(_) => TokenKind::Ident(ident),
                }
            }
            (c, _) => return Some(Err(Error::UnexpectedChar(c, start))),
        };

        let span = Span::new(start, self.loc);
        Some(Ok(Token { kind, span }))
    }
}

fn is_valid_ident(c: char) -> bool {
    !c.is_whitespace() && (c == '_' || !c.is_ascii_punctuation())
}
