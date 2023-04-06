use la_arena::Idx;

use crate::{name::Name, sort_ref::SortRef};

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

pub type ExprId = Idx<Expr>;
