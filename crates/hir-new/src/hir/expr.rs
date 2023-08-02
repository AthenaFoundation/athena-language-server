use core::fmt::{self, Write};

use crate::hir::Name;

use super::{DebugDump, ExprId, PhraseId, SortRefId};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
    Symbol(Name),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Ident(Name),
    Literal(Literal),
    TermVar(Name, Option<SortRefId>),
    Unit,
    Check(Vec<ExprCheckArm>),
}

pub type ExprCheckArm = CheckArm<ExprId>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CheckArm<R> {
    pub condition: PhraseId,
    pub result: R,
}

impl DebugDump for Expr {
    fn debug_dump(&self, dd: &mut super::DebugDumper) -> fmt::Result {
        match self {
            Expr::Ident(name) => write!(dd, "{}", name),
            Expr::Literal(lit) => write!(dd, "{:?}", lit),
            Expr::TermVar(name, sort) => {
                if let Some(sort) = sort {
                    write!(dd, "({}: ", name,)?;
                    dd.debug_dump(dd.get(sort))?;
                    write!(dd, ")")
                } else {
                    write!(dd, "{}", name)
                }
            }
            Expr::Unit => write!(dd, "()"),
            Expr::Check(arms) => {
                writeln!(dd, "check {{")?;
                dd.indented(|dd| {
                    for arm in arms {
                        dd.debug_dump(dd.get(arm.condition))?;
                        writeln!(dd, " => ")?;
                        dd.debug_dump(dd.get(arm.result))?;
                    }
                    Ok(())
                })?;
                writeln!(dd, "}}")?;
                Ok(())
            }
        }
    }
}
