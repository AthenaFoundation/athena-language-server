use la_arena::Idx;

use crate::{name::Name, sort_ref::SortRef};

pub enum Literal {
    String(String),
    Char(char),
}

pub enum Expr {
    Ident(Name),
    Literal(Literal),
    Unit,
    TermVar(Name, Option<SortRef>),
}

pub type ExprId = Idx<Expr>;
