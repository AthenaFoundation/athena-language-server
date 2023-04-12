use crate::hir::Name;

use super::SortRefId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortRef {
    Var(Name),
    Ident(Name),
    Apply(SortRefId, Vec<SortRefId>),
}
