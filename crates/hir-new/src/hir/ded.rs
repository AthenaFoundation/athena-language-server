use super::{debug_dump::DebugDumper, DebugDump, ExprId, PhraseId};
use core::fmt::Write;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ded {
    MethodCall(ExprId, PhraseId),
}

impl DebugDump for Ded {
    fn debug_dump(&self, dd: &mut DebugDumper<'_>) -> core::fmt::Result {
        match self {
            Ded::MethodCall(expr, phrase) => {
                write!(dd, "Ded::MethodCall(")?;
                dd.dump(expr)?;
                write!(dd, ", ")?;
                dd.dump(phrase)?;
                write!(dd, ")")
            }
        }
    }
}
