use util::impl_from;

use crate::hir::{Ded, DedId, Expr, ExprId};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PhraseRef<'a> {
    Expr(&'a Expr),
    Ded(&'a Ded),
}

impl<'a> From<&'a Expr> for PhraseRef<'a> {
    fn from(expr: &'a Expr) -> Self {
        Self::Expr(expr)
    }
}

impl<'a> From<&'a Ded> for PhraseRef<'a> {
    fn from(ded: &'a Ded) -> Self {
        Self::Ded(ded)
    }
}

impl_from!(Expr, Ded for Phrase);

use super::{impl_debug_dump, DebugDump, DebugDumper};

impl_debug_dump!(delegate Expr, Ded for Phrase);

impl super::Get for PhraseId {
    type Output<'a> = PhraseRef<'a>;

    fn get(self, hir: &super::FileHir) -> Self::Output<'_> {
        match self {
            Self::Expr(expr) => PhraseRef::Expr(&hir[expr]),
            Self::Ded(ded) => PhraseRef::Ded(&hir[ded]),
        }
    }
}

impl<'a> DebugDump for PhraseRef<'a> {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        match self {
            Self::Expr(expr) => expr.debug_dump(dd),
            Self::Ded(ded) => ded.debug_dump(dd),
        }
    }
}
