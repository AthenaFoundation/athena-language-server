use core::fmt::{self, Write};

use crate::{hir::Name, hir::SortRef};

use super::DebugDump;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Ident(Name),
    Literal(Literal),
    TermVar(Name, Option<SortRef>),
    Unit,
}

impl DebugDump for Expr {
    fn debug_dump(&self, dd: &mut super::DebugDumper) -> fmt::Result {
        match self {
            Expr::Ident(name) => write!(dd, "{}", name),
            Expr::Literal(lit) => write!(dd, "{:?}", lit),
            Expr::TermVar(name, sort) => {
                if let Some(sort) = sort {
                    write!(dd, "({}: ", name,)?;
                    dd.debug_dump(sort)?;
                    write!(dd, ")")
                } else {
                    write!(dd, "{}", name)
                }
            }
            Expr::Unit => write!(dd, "()"),
        }
    }
}
