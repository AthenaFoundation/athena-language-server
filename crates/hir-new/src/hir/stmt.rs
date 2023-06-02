use base_db::FileId;

use crate::hir::{ExprId, Name, NameOrWildcard, PatId, PhraseId, SortRefId};

use super::DebugDump;
use super::DebugDumper;
use core::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: Name,
    pub stmts: Vec<super::ModuleStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExtension {
    pub name: Name,
    pub stmts: Vec<super::ModuleStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub names: Vec<Name>,
    pub sort_vars: Vec<Name>,
    pub arg_sorts: Vec<SortRefId>,
    pub ret_sort: SortRefId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SortAlias {
    pub name: Name,
    pub sort_ref: SortRefId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileImport {
    pub file: FileId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImport {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SortDeclaration {
    pub name: Name,
    pub sort_args: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DomainDeclaration {
    pub domains: Vec<SortDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assertion {
    pub name: Option<Name>,
    pub facts: Vec<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosedAssertion {
    pub name: Option<Name>,
    pub fact: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Overload {
    pub target: PhraseId,
    pub overload: PhraseId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    pub kind: DefKind,
    pub body: PhraseId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefKind {
    Procedure(Name, Vec<Param>),

    Value(Name),

    PatternValue(PatId),

    NamedPatternValue(Name, PatId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: NameOrWildcard,
    pub sort: Option<SortRefId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveMethod {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhraseStmt {
    pub phrase: PhraseId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Datatypes {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structures {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrecedenceDeclaration {
    pub names: Vec<Name>,
    pub value: ExprId,
}

impl DebugDump for Module {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        writeln!(dd, "module {} {{", self.name)?;
        dd.indented(|dd| {
            for stmt in &self.stmts {
                stmt.debug_dump(dd)?;
            }
            Ok(())
        })?;
        writeln!(dd, "}}")
    }
}

impl DebugDump for ModuleExtension {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        writeln!(dd, "extend-module {} {{", self.name)?;
        for stmt in &self.stmts {
            dd.tab();
            stmt.debug_dump(dd)?;
        }
        writeln!(dd, "}}")
    }
}

impl DebugDump for FunctionSymbol {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        write!(dd, "declare ")?;
        dd.comma_separated(&self.names, |dd, name| write!(dd, "{}", name))?;
        write!(dd, ": (")?;
        dd.comma_separated(&self.sort_vars, |dd, sort| write!(dd, "{sort:?}"))?;
        write!(dd, ")")?;
        write!(dd, " [")?;
        dd.comma_separated(&self.arg_sorts, |dd, arg| dd.hir[*arg].debug_dump(dd))?;
        write!(dd, "] -> ")?;
        dd.hir[self.ret_sort].debug_dump(dd)?;
        writeln!(dd, "")
    }
}

impl DebugDump for SortAlias {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for FileImport {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        writeln!(dd, "load {{")?;
        dd.indented(|dd| {
            dd.dump_file(self.file)?;
            Ok(())
        })?;
        writeln!(dd, "}}")
    }
}

impl DebugDump for ModuleImport {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for SortDeclaration {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for DomainDeclaration {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for Assertion {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for ClosedAssertion {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for Overload {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for Definition {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for PhraseStmt {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for Datatypes {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for Structures {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}

impl DebugDump for PrecedenceDeclaration {
    fn debug_dump(&self, dd: &mut DebugDumper) -> core::fmt::Result {
        todo!()
    }
}
