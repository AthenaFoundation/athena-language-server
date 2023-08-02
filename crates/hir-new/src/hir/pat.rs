use la_arena::Idx;

use crate::hir::Name;

use super::{expr::Literal, ExprId, NameOrWildcard, PatId, SortRefId};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatKind {
    Wildcard,
    Ident {
        name: Name,
        sort: Option<SortRefId>,
        op_arity: Option<Name>,
    },
    Var {
        name: Name,
        sort: Option<SortRefId>,
    },
    Symbol(Name),
    List(Vec<PatId>),
    Literal(Literal),
    Unit,
    Named {
        name: Name,
        pat: PatId,
    },
    ValOf(Name),
    ListOf(PatId, PatId),
    Split(Vec<PatId>),
    Compound(Vec<PatId>),
    Where(PatId, ExprId),
    Some(Thing, NameOrWildcard),
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum Thing {
    Var,
    SentCon,
    Quant,
    Term,
    Atom,
    Sentence,
    List,
    Cell,
    Vector,
    Proc,
    Method,
    Symbol,
    Table,
    Map,
    Sub,
    Char,
}

impl Pat {
    pub fn walk_child_pats(&self, mut f: impl FnMut(Idx<Pat>)) {
        match &self.kind {
            PatKind::Ident { .. } => {}
            PatKind::Var { .. } => {}
            PatKind::Symbol(_) => {}
            PatKind::Literal(_) => {}
            PatKind::List(pats) => {
                for pat in pats {
                    f(*pat);
                }
            }
            PatKind::Unit => {}
            PatKind::Named { pat, .. } => {
                f(*pat);
            }
            PatKind::ValOf(_) => {}
            PatKind::ListOf(pat1, pat2) => {
                f(*pat1);
                f(*pat2);
            }
            PatKind::Split(pats) => {
                for pat in pats {
                    f(*pat);
                }
            }
            PatKind::Compound(pats) => {
                for pat in pats {
                    f(*pat);
                }
            }
            PatKind::Wildcard => {}
            PatKind::Where(pat, _) => {
                f(*pat);
            }
            PatKind::Some(_, _) => {}
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Pat {
    pub kind: PatKind,
}

impl super::DebugDump for Pat {
    fn debug_dump(&self, _dd: &mut super::DebugDumper) -> core::fmt::Result {
        todo!()
    }
}
