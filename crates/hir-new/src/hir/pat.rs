use la_arena::Idx;

use crate::hir::{Name, SortRef};

use impl_tools::autoimpl;

#[autoimpl(Debug, Clone, Eq, PartialEq)]
pub enum PatKind {
    Ident(Name),
    Var(Name, Option<SortRef>),
    List(Vec<Idx<Pat>>),
}

impl Pat {
    pub fn walk_child_pats(&self, mut f: impl FnMut(Idx<Pat>)) {
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
pub struct Pat {
    pub kind: PatKind,
}
