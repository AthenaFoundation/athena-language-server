use util::impl_from;

use crate::hir::{Ded, DedId, Expr, ExprId};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PhraseId {
    Expr(ExprId),
    Ded(DedId),
}

impl_from!(Expr(ExprId), Ded(DedId) for PhraseId);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Phrase {
    Expr(Expr),
    Ded(Ded),
}

impl_from!(Expr, Ded for Phrase);
