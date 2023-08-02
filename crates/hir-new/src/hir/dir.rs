use crate::hir::{ExprId, Name};

use super::{DebugDump, DebugDumper};
use core::fmt::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirKind {
    SetPrecedence(Vec<Name>, ExprId),
    SetAssociativity(Name, Associativity),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dir {
    pub kind: DirKind,
}

impl DebugDump for Dir {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        match &self.kind {
            DirKind::SetPrecedence(names, val) => {
                writeln!(dd, "set-precedence {:?}", names)?;
                dd.dump(val)
            }
            DirKind::SetAssociativity(name, assoc) => {
                writeln!(dd, "set-assoc {} {:?}", name, assoc)
            }
        }
    }
}
