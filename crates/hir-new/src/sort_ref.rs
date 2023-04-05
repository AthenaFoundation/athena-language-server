use crate::name::Name;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortRef {
    Var(Name),
    Ident(Name),
    Apply(Box<SortRef>, Vec<SortRef>),
}
