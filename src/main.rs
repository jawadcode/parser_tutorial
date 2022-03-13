use parser::Parser;
use std::{
    env, fs,
    io::{self, Write},
    path::Path,
};

mod lexer {
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
}

mod ast {
    use derive_more::Display;

    use crate::lexer::TK;

    type Ident = String;
    type Boxpr = Box<Expr>;

    #[derive(Debug, Display, Clone)]
    pub enum Expr {
        #[display(fmt = "{}", _0)]
        Lit(Lit),
        #[display(fmt = "{}", _0)]
        Ident(Ident),
        #[display(fmt = "(let :vars [{}] :in {})", "fmt_vars(&vars)", body)]
        Let {
            vars: Vec<(Ident, Expr)>,
            body: Boxpr,
        },
        #[display(fmt = "(fun :args [{}] :body {})", "args.join(\" \")", body)]
        Fun { args: Vec<String>, body: Boxpr },
        #[display(
            fmt = "(call :fun {} :args [{}])",
            fun,
            "args.iter().map(ToString::to_string).collect::<Vec<_>>().join(\" \")"
        )]
        Call { fun: Boxpr, args: Vec<Expr> },
        #[display(fmt = "({} {} {})", op, lhs, rhs)]
        BinOp { op: TK, lhs: Boxpr, rhs: Boxpr },
        #[display(fmt = "({} {})", _0, _1)]
        UnOp(TK, Boxpr),
    }

    #[derive(Debug, Display, Clone)]
    pub enum Lit {
        #[display(fmt = "{}", _0)]
        String(String),
        #[display(fmt = "{}", _0)]
        Number(f64),
        #[display(fmt = "{}", _0)]
        Bool(bool),
    }

    fn fmt_vars(vars: &[(String, Expr)]) -> String {
        vars.iter()
            .map(|(ident, expr)| format!("({ident} {expr})"))
            .collect::<Vec<_>>()
            .join(" ")
    }
}

mod parser {
    use crate::ast::{Expr, Lit};

    use super::lexer::*;
    use derive_more::Display;
    use std::iter::Peekable;

    pub struct Parser<'input> {
        input: &'input str,
        lexer: Peekable<Lexer<'input>>,
    }

    #[derive(Debug, Display)]
    pub enum SyntaxError {
        #[display(fmt = "SyntaxError: Unexpected EOF")]
        UnexpectedEOF,
        #[display(fmt = "SyntaxError: Unexpected token, expected {} and got {}", _0, _1)]
        ExpectedGot(/* expected */ String, /* got */ TK),
        #[display(fmt = "SyntaxError: Invalid escape sequence '\\{}'", _0)]
        InvalidEscSeq(char),
        #[display(fmt = "SyntaxError: Invalid literal '{}'", _0)]
        InvalidLit(String),
    }

    pub type ParseResult<T> = Result<T, SyntaxError>;

    trait Operator {
        fn prefix_binding_power(&self) -> Option<((), u8)>;
        fn infix_binding_power(&self) -> Option<(u8, u8)>;
        fn postfix_binding_power(&self) -> Option<(u8, ())>;
    }

    const BASIC_EXPR_TOKENS: [TK; 6] = [
        TK::Ident,
        TK::StringLit,
        TK::NumLit,
        TK::True,
        TK::False,
        TK::LParen,
    ];

    impl Operator for TK {
        fn prefix_binding_power(&self) -> Option<((), u8)> {
            Some(match self {
                TK::Sub => ((), 51),
                TK::Not => ((), 101),
                _ => return None,
            })
        }

        fn infix_binding_power(&self) -> Option<(u8, u8)> {
            use TK::*;
            Some(match self {
                Or => (1, 2),
                And => (3, 4),
                Eq | NotEq => (5, 6),
                Less | LessEq | Greater | GreatEq => (7, 8),
                Add | Sub => (9, 10),
                Mul | Div => (11, 12),
                _ => return None,
            })
        }

        fn postfix_binding_power(&self) -> Option<(u8, ())> {
            None
        }
    }

    impl<'input> Parser<'input> {
        pub fn new(input: &'input str) -> Self {
            Self {
                input,
                lexer: Lexer::new(input).peekable(),
            }
        }

        #[inline]
        fn peek(&mut self) -> ParseResult<TK> {
            self.lexer
                .peek()
                .map(|tok| tok.kind)
                .ok_or(SyntaxError::UnexpectedEOF)
        }

        #[inline]
        fn at(&mut self, kind: TK) -> ParseResult<bool> {
            Ok(self.peek()? == kind)
        }

        #[inline]
        fn at_any<const NUM_KINDS: usize>(&mut self, kinds: [TK; NUM_KINDS]) -> ParseResult<bool> {
            Ok(kinds.contains(&self.peek()?))
        }

        #[inline]
        fn next(&mut self) -> ParseResult<Token> {
            self.lexer.next().ok_or(SyntaxError::UnexpectedEOF)
        }

        #[inline]
        fn consume(&mut self, kind: TK) -> ParseResult<()> {
            let token = self.next()?;
            if token.kind != kind {
                Err(SyntaxError::ExpectedGot(kind.to_string(), token.kind))
            } else {
                Ok(())
            }
        }

        #[inline]
        fn text(&mut self) -> &'input str {
            let token = self.next().unwrap();
            token.text(self.input)
        }

        fn parse_expr(&mut self, binding_power: u8) -> ParseResult<Expr> {
            let mut lhs = match self.peek()? {
                TK::Let => self.parse_let(),
                TK::Fun => self.parse_fun(),

                TK::Ident => self.parse_ident(),
                TK::StringLit => self.parse_str(),
                TK::NumLit => self.parse_num(),
                b @ TK::True | b @ TK::False => self.parse_bool(b),
                op @ TK::Not | op @ TK::Sub => self.parse_prefix_op(op),

                TK::LParen => self.parse_grouping(),
                err => return Err(SyntaxError::ExpectedGot("expression".to_string(), err)),
            }?;

            match self.at_any(BASIC_EXPR_TOKENS) {
                Ok(true) => lhs = self.parse_fun_call(lhs)?,
                Ok(false) => (),
                Err(_) => (),
            }

            loop {
                use TK::*;
                let op = match self.peek() {
                    Ok(tok) => match tok {
                        op @ Add
                        | op @ Sub
                        | op @ Mul
                        | op @ Div
                        | op @ Less
                        | op @ LessEq
                        | op @ Greater
                        | op @ GreatEq
                        | op @ Eq
                        | op @ NotEq
                        | op @ And
                        | op @ Or => op,
                        TK::RParen | TK::In | TK::Semicolon => break,
                        tok => {
                            return Err(SyntaxError::ExpectedGot(
                                "operator, ')', 'in', or ';'".to_string(),
                                tok,
                            ))
                        }
                    },
                    Err(_) => break,
                };

                if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                    if left_binding_power < binding_power {
                        break;
                    }

                    // We can unwrap because there is guaranteed to be a token
                    self.next().unwrap();

                    lhs = Expr::UnOp(op, Box::new(lhs));
                    continue;
                }

                if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                    if left_binding_power < binding_power {
                        break;
                    }

                    // We can unwrap because there is guaranteed to be a token
                    self.next().unwrap();

                    let rhs = self.parse_expr(right_binding_power)?;
                    lhs = Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    continue;
                }
                break;
            }

            Ok(lhs)
        }

        #[inline]
        pub fn expr(&mut self) -> ParseResult<Expr> {
            self.parse_expr(0)
        }

        fn parse_let(&mut self) -> ParseResult<Expr> {
            self.next().unwrap();

            let mut vars = vec![];
            while self.at(TK::Ident)? {
                let ident = self.next().unwrap().text(self.input);
                self.consume(TK::Assign)?;
                let expr = self.expr()?;
                vars.push((ident.to_string(), expr));

                if !self.at(TK::Semicolon)? {
                    break;
                } else {
                    // We can `.unwrap()` because there is guaranteed to be a token
                    self.next().unwrap();
                }
            }

            self.consume(TK::In)?;
            let body = Box::new(self.expr()?);

            Ok(Expr::Let { vars, body })
        }

        fn parse_fun(&mut self) -> ParseResult<Expr> {
            // We can `.unwrap()` because there is guaranteed to be a token
            self.next().unwrap();

            let mut args = vec![];
            while self.at(TK::Ident)? {
                args.push({
                    let token = self.next().unwrap();
                    token.text(self.input).to_string()
                });
            }

            self.consume(TK::Arrow)?;
            let body = Box::new(self.expr()?);

            Ok(Expr::Fun { args, body })
        }

        fn parse_ident(&mut self) -> ParseResult<Expr> {
            Ok(Expr::Ident(self.text().to_string()))
        }

        fn parse_str(&mut self) -> ParseResult<Expr> {
            let text = self.text();

            let mut buf = vec![];
            let mut backslash = false;
            for byte in text[1..(text.len() - 1)].bytes() {
                if backslash {
                    match byte {
                        b't' => buf.push(b'\t'),
                        b'n' => buf.push(b'\n'),
                        b @ b'"' | b @ b'\\' => buf.push(b),
                        b => return Err(SyntaxError::InvalidEscSeq(b.into())),
                    };
                    backslash = false;
                } else {
                    if byte == b'\\' {
                        backslash = true;
                    } else {
                        buf.push(byte);
                    }
                }
            }

            Ok(Expr::Lit(Lit::String(String::from_utf8(buf).unwrap())))
        }

        fn parse_num(&mut self) -> ParseResult<Expr> {
            let text = self.text();
            let num: f64 =
                lexical::parse(text).map_err(|_| SyntaxError::InvalidLit(text.to_string()))?;
            Ok(Expr::Lit(Lit::Number(num)))
        }

        fn parse_bool(&mut self, b: TK) -> ParseResult<Expr> {
            Ok(Expr::Lit(Lit::Bool(b == TK::True)))
        }

        fn parse_prefix_op(&mut self, op: TK) -> ParseResult<Expr> {
            self.next().unwrap();
            let expr = Box::new(self.expr()?);
            Ok(Expr::UnOp(op, expr))
        }

        fn parse_grouping(&mut self) -> ParseResult<Expr> {
            self.next().unwrap();
            let expr = self.expr()?;
            self.consume(TK::RParen)?;
            Ok(expr)
        }

        fn parse_fun_call(&mut self, lhs: Expr) -> ParseResult<Expr> {
            let mut args = vec![];
            loop {
                let token = match self.peek() {
                    Ok(tok) => tok,
                    Err(_) => break,
                };
                if BASIC_EXPR_TOKENS.contains(&token) {
                    args.push(match token {
                        TK::Ident => self.parse_ident(),
                        TK::StringLit => self.parse_str(),
                        TK::NumLit => self.parse_num(),
                        b @ TK::True | b @ TK::False => self.parse_bool(b),
                        TK::LParen => self.parse_grouping(),
                        _ => unreachable!(),
                    }?)
                } else {
                    break;
                }
            }

            Ok(Expr::Call {
                fun: Box::new(lhs),
                args,
            })
        }
    }
}

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();

    match args.next() {
        Some(arg) => match arg.as_str() {
            "--repl" | "-r" => repl(),
            "--file" | "-f" => {
                let path = args.next().unwrap();
                let path = Path::new(&path);
                file(path);
            }
            _ => help(&name),
        },
        _ => help(&name),
    }
}

fn help(name: &str) {
    println!(
        "Usage: {name} --repl or {name} --file path/to/file",
        name = name
    );
}

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        // println!("{:#?}", Lexer::new(&input).collect::<Vec<_>>());
        parse(&input);
    }
}

fn file(path: &Path) {
    let contents = fs::read_to_string(path).unwrap();
    parse(&contents);
}

fn parse(input: &str) {
    let mut parser = Parser::new(&input);
    match parser.expr() {
        Ok(expr) => println!("{expr}"),
        Err(err) => eprintln!("{err}"),
    }
}
