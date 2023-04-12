use la_arena::Idx;

use super::{ExprId, PhraseId};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ded {
    MethodCall(ExprId, PhraseId),
}
