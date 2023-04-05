use la_arena::Idx;

use crate::{name::Name, sort_ref::SortRef};

pub enum Pat {
    Ident(Name),
    Var(Name, Option<SortRef>),
}

pub type PatId = Idx<Pat>;
