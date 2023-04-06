mod lower;

use la_arena::{Arena, Idx};

use crate::{ded::Ded, expr::Expr, pat::LocalPat, phrase::PhraseId};

#[derive(Debug, Clone, Default, Eq, PartialEq)]
struct BodyData {
    patterns: Arena<LocalPat>,
    deductions: Arena<Ded>,
    expressions: Arena<Expr>,
}

#[derive(Default, Clone, Debug, Eq, PartialEq)]
pub struct Body {
    root: Option<PhraseId>,
    data: BodyData,
}

pub type BodyId = Idx<Body>;
