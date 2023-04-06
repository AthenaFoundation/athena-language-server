mod lower;

use base_db::FileId;
use impl_tools::autoimpl;
use la_arena::{Arena, Idx};
use std::{ops::Index, sync::Arc};
use syntax::{ast, AstNode};

use crate::{
    ast_map::FileAstId,
    body::{Body, BodyId},
    db::HirNewDatabase,
    ded::Ded,
    dir::Dir,
    expr::Expr,
    name::Name,
    pat::{StmtPat, StmtPatId},
    phrase::Phrase,
    sort_ref::SortRef,
};

#[derive(Debug, Default, Eq, PartialEq)]
pub struct StmtTree {
    top_level: Vec<ModuleStmt>,
    data: StmtTreeData,
}

impl StmtTree {
    fn data(&self) -> &StmtTreeData {
        &self.data
    }

    pub(crate) fn file_stmt_tree_query(db: &dyn HirNewDatabase, file_id: FileId) -> Arc<StmtTree> {
        let file = db.parse(file_id).tree();

        let ctx = lower::Ctx::new(db, file_id);

        Arc::new(ctx.lower_file(file))
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
struct StmtTreeData {
    modules: Arena<Module>,
    module_exts: Arena<ModuleExtension>,
    func_symbols: Arena<FunctionSymbol>,
    sort_aliases: Arena<SortAlias>,
    file_imports: Arena<FileImport>,
    module_imports: Arena<ModuleImport>,
    domains: Arena<DomainDeclaration>,
    assertions: Arena<Assertion>,
    closed_assertions: Arena<ClosedAssertion>,
    dirs: Arena<Dir>,
    bodies: Arena<Body>,
    pats: Arena<StmtPat>,
}

pub trait StmtTreeNode: Clone {
    type Source: AstNode + Into<ast::Stmt>;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    fn lookup(tree: &StmtTree, index: Idx<Self>) -> &Self;

    fn id_from_module_stmt(mod_stmt: ModuleStmt) -> Option<FileStmtTreeId<Self>>;

    fn id_to_module_stmt(id: FileStmtTreeId<Self>) -> ModuleStmt;
}

#[autoimpl(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct StmtTreeId<N: StmtTreeNode> {
    file: FileId,
    pub value: FileStmtTreeId<N>,
}

#[autoimpl(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileStmtTreeId<N: StmtTreeNode>(Idx<N>);

macro_rules! module_stmts {
    ( $( $typ:ident in $fld:ident -> $ast:ty ),+ $(,)? ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum ModuleStmt {
            $(
                $typ(FileStmtTreeId<$typ>),
            )+
        }

        $(
            impl From<FileStmtTreeId<$typ>> for ModuleStmt {
                fn from(id: FileStmtTreeId<$typ>) -> ModuleStmt {
                    ModuleStmt::$typ(id)
                }
            }
        )+

        $(
            impl StmtTreeNode for $typ {
                type Source = $ast;

                fn ast_id(&self) -> FileAstId<Self::Source> {
                    self.ast_id
                }

                fn lookup(tree: &StmtTree, index: Idx<Self>) -> &Self {
                    &tree.data().$fld[index]
                }

                fn id_from_module_stmt(mod_stmt: ModuleStmt) -> Option<FileStmtTreeId<Self>> {
                    match mod_stmt {
                        ModuleStmt::$typ(id) => Some(id),
                        _ => None,
                    }
                }

                fn id_to_module_stmt(id: FileStmtTreeId<Self>) -> ModuleStmt {
                    ModuleStmt::$typ(id)
                }
            }

            impl Index<Idx<$typ>> for StmtTree {
                type Output = $typ;

                fn index(&self, index: Idx<$typ>) -> &Self::Output {
                    &self.data().$fld[index]
                }
            }
        )+
    };
}

module_stmts! {
    Module in modules -> ast::ModuleDir,
    ModuleExtension in module_exts -> ast::ExtendModuleDir,
    FunctionSymbol in func_symbols -> ast::TermSymbol,
    SortAlias in sort_aliases -> ast::DefineSortDir,
    FileImport in file_imports -> ast::LoadDir,
    ModuleImport in module_imports -> ast::OpenDir,
    DomainDeclaration in domains -> ast::Domain,
    Assertion in assertions -> ast::AssertDir,
    ClosedAssertion in closed_assertions -> ast::AssertClosedDir,
    Dir in dirs -> ast::Dir,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: Name,
    pub stmts: Vec<ModuleStmt>,
    pub ast_id: FileAstId<ast::ModuleDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExtension {
    pub name: Name,
    pub ast_id: FileAstId<ast::ExtendModuleDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub names: Vec<Name>,
    pub arg_sorts: Vec<SortRef>,
    pub ret_sort: SortRef,
    pub ast_id: FileAstId<ast::TermSymbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SortAlias {
    pub name: Name,
    pub sort_ref: SortRef,
    pub ast_id: FileAstId<ast::DefineSortDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileImport {
    pub file: FileId,
    pub ast_id: FileAstId<ast::LoadDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImport {
    pub name: Name,
    pub ast_id: FileAstId<ast::OpenDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Domain {
    pub name: Name,
    pub sort_args: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DomainDeclaration {
    pub domains: Vec<Domain>,
    pub ast_id: FileAstId<ast::Domain>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assertion {
    pub name: Option<Name>,
    pub bodies: Vec<BodyId>,
    pub ast_id: FileAstId<ast::AssertDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosedAssertion {
    pub name: Option<Name>,
    pub body: BodyId,
    pub ast_id: FileAstId<ast::AssertClosedDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Overload {
    pub ast_id: FileAstId<ast::OverloadDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    pub kind: DefKind,
    pub body: BodyId,
    pub ast_id: FileAstId<ast::DefineDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefKind {
    Procedure(Name, Vec<Param>),

    Value(Name),

    PatternValue(StmtPatId),

    NamedPatternValue(Name, StmtPatId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Name,
    pub sort: Option<SortRef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveMethod {
    pub ast_id: FileAstId<ast::RuleDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhraseStmt {
    pub body: BodyId,
    pub ast_id: FileAstId<ast::PhraseStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Datatypes {
    pub ast_id: FileAstId<ast::DatatypeOrDatatypes>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structures {
    pub ast_id: FileAstId<ast::StructureOrStructures>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrecedenceDeclaration {
    pub names: Vec<Name>,
    pub value: BodyId,
    pub ast_id: FileAstId<ast::SetPrecedenceDir>,
}
