use la_arena::Idx;

use crate::{
    ded::{Ded, DedId},
    expr::{Expr, ExprId},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PhraseId {
    Expr(ExprId),
    Ded(DedId),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Phrase {
    Expr(Expr),
    Ded(Ded),
}
