use crate::hir::Name;
use core::fmt::Write;

use super::{DebugDump, SortRefId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortRef {
    Var(Name),
    Ident(Name),
    Apply(SortRefId, Vec<SortRefId>),
}

impl DebugDump for SortRef {
    fn debug_dump(&self, dd: &mut super::DebugDumper) -> core::fmt::Result {
        match self {
            SortRef::Var(name) => write!(dd, "<{}>", name),
            SortRef::Ident(name) => write!(dd, "<{}>", name),
            SortRef::Apply(sort, args) => {
                dd.dump(sort)?;
                write!(dd, "<")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(dd, ", ")?;
                    }
                    dd.dump(arg)?;
                }
                write!(dd, ">")?;
                Ok(())
            }
        }
    }
}
