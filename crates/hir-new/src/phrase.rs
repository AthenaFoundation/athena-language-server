use la_arena::Idx;

use crate::{ded::DedId, expr::ExprId};

pub enum Phrase {
    Expr(ExprId),
    Ded(DedId),
}

pub type PhraseId = Idx<Phrase>;
