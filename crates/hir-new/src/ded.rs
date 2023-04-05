use la_arena::Idx;

use crate::{expr::ExprId, phrase::PhraseId};

pub enum Ded {
    MethodCall(ExprId, PhraseId),
}

pub type DedId = Idx<Ded>;
