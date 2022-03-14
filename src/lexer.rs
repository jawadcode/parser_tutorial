use derive_more::Display;
use logos::{Logos, SpannedIter};
use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Display, Clone, Copy, Logos, PartialEq)]
pub enum TK {
    #[token("let")]
    #[display(fmt = "let")]
    Let,
    #[token("in")]
    #[display(fmt = "in")]
    In,
    #[token("fun")]
    #[display(fmt = "fun")]
    Fun,
    #[regex(r"([A-Za-z]|_)([A-Za-z]|_|\d)*")]
    #[display(fmt = "identifier")]
    Ident,

    #[token("=")]
    #[display(fmt = "=")]
    Assign,
    #[token("=>")]
    #[display(fmt = "=>")]
    Arrow,
    #[token("(")]
    #[display(fmt = "(")]
    LParen,
    #[token(")")]
    #[display(fmt = ")")]
    RParen,
    #[token(";")]
    #[display(fmt = ";")]
    Semicolon,

    #[token("+")]
    #[display(fmt = "+")]
    Add,
    #[token("-")]
    #[display(fmt = "-")]
    Sub,
    #[token("*")]
    #[display(fmt = "*")]
    Mul,
    #[token("/")]
    #[display(fmt = "/")]
    Div,

    #[token("<")]
    #[display(fmt = "<")]
    Less,
    #[token("<=")]
    #[display(fmt = "<=")]
    LessEq,
    #[token(">")]
    #[display(fmt = ">")]
    Greater,
    #[token(">=")]
    #[display(fmt = ">=")]
    GreatEq,
    #[token("==")]
    #[display(fmt = "==")]
    Eq,
    #[token("!=")]
    #[display(fmt = "!=")]
    NotEq,

    #[token("!")]
    #[display(fmt = "!")]
    Not,
    #[token("and")]
    #[display(fmt = "and")]
    And,
    #[token("or")]
    #[display(fmt = "or")]
    Or,

    #[regex(r"[-+]?([0-9]*[.])?[0-9]+([eE][-+]?\d+)?")]
    #[display(fmt = "numeric literal")]
    NumLit,
    #[regex(r#""([^"]|\\.)*""#)]
    #[display(fmt = "string literal")]
    StringLit,
    #[token("true")]
    #[display(fmt = "true")]
    True,
    #[token("false")]
    #[display(fmt = "false")]
    False,

    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[display(fmt = "invalid token")]
    #[error]
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TK,
    pub span: Range<usize>,
}

impl<'input> Token {
    #[inline]
    pub fn text(&self, input: &'input str) -> &'input str {
        &input[self.span.start..self.span.end]
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

pub struct Lexer<'input> {
    logos: SpannedIter<'input, TK>,
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.logos.next().map(|tok| Token {
            kind: tok.0,
            span: tok.1,
        })
    }
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            logos: TK::lexer(input).spanned(),
        }
    }
}
