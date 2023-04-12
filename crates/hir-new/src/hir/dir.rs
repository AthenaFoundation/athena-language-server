use crate::hir::{ExprId, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirKind {
    SetPrecedence(Vec<Name>, ExprId),
    SetAssociativity(Name, Associativity),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dir {
    pub kind: DirKind,
}
