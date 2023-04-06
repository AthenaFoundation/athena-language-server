use la_arena::Idx;

use crate::{expr::ExprId, phrase::PhraseId};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ded {
    MethodCall(ExprId, PhraseId),
}

pub type DedId = Idx<Ded>;
