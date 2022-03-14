use derive_more::Display;

// use crate::lexer::TK;
use super::lexer::TK;

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
