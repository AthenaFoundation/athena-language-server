use la_arena::Idx;

use crate::name::Name;

#[derive(PartialEq, Eq, Debug)]
pub struct Sort {
    pub kind: SortKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum SortKind {
    Var(Name),
    Ident(Name),
    Compound(Vec<SortId>),
}

pub type SortId = Idx<Sort>;
