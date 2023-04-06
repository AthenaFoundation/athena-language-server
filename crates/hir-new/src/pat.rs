use la_arena::Idx;
use std::marker::PhantomData;

use crate::{name::Name, sort_ref::SortRef};

use impl_tools::autoimpl;

#[autoimpl(Debug, Clone, Eq, PartialEq)]
pub enum PatKind<L> {
    Ident(Name),
    Var(Name, Option<SortRef>),
    List(Vec<Idx<Pat<L>>>),
}

impl<L> Pat<L> {
    pub fn walk_child_pats(&self, mut f: impl FnMut(Idx<Pat<L>>)) {
        match &self.kind {
            PatKind::Ident(_) => {}
            PatKind::Var(_, _) => {}
            PatKind::List(pats) => {
                for pat in pats {
                    f(*pat);
                }
            }
        }
    }
}

#[autoimpl(Debug, Clone, Eq, PartialEq)]
pub struct Pat<L> {
    pub kind: PatKind<L>,
    _phantom: PhantomData<L>,
}

pub struct Local;

pub type LocalPat = Pat<Local>;
pub type LocalPatId = Idx<LocalPat>;

pub struct StmtLevel;

pub type StmtPat = Pat<StmtLevel>;
pub type StmtPatId = Idx<StmtPat>;
