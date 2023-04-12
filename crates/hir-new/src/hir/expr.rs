use crate::{hir::Name, hir::SortRef};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Ident(Name),
    Literal(Literal),
    TermVar(Name, Option<SortRef>),
    Unit,
}
