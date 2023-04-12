use base_db::FileId;

use crate::hir::{ExprId, Name, NameOrWildcard, PatId, PhraseId, SortRefId};

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
