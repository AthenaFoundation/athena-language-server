mod lower;

use base_db::FileId;
use derivative::Derivative;
use la_arena::{Arena, Idx};
use std::{ops::Index, sync::Arc};
use syntax::{ast, AstNode};

use crate::{ast_map::FileAstId, db::HirNewDatabase, name::Name, sort_ref::SortRef};

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
    constant_symbols: Arena<ConstantSymbol>,
    sort_aliases: Arena<SortAlias>,
    file_imports: Arena<FileImport>,
    module_imports: Arena<ModuleImport>,
    domains: Arena<DomainDeclaration>,
    assoc: Arena<AssociativityDeclaration>,
    assertions: Arena<Assertion>,
    closed_assertions: Arena<ClosedAssertion>,
}

pub trait StmtTreeNode: Clone {
    type Source: AstNode + Into<ast::Stmt>;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    fn lookup(tree: &StmtTree, index: Idx<Self>) -> &Self;

    fn id_from_module_stmt(mod_stmt: ModuleStmt) -> Option<FileStmtTreeId<Self>>;

    fn id_to_module_stmt(id: FileStmtTreeId<Self>) -> ModuleStmt;
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    Copy(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = "")
)]
pub struct StmtTreeId<N: StmtTreeNode> {
    file: FileId,
    pub value: FileStmtTreeId<N>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    Copy(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = "")
)]
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
    FunctionSymbol in func_symbols -> ast::DeclareDir,
    ConstantSymbol in constant_symbols -> ast::ConstantDeclareDir,
    SortAlias in sort_aliases -> ast::DefineSortDir,
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
    pub name: Name,
    pub ast_id: FileAstId<ast::DeclareDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantSymbol {
    pub name: Name,
    pub ast_id: FileAstId<ast::ConstantDeclareDir>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssociativityDeclaration {
    pub associativity: Associativity,
    pub target: Name,
    pub ast_id: FileAstId<ast::AssociativityDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assertion {
    pub name: Option<Name>,
    pub ast_id: FileAstId<ast::AssertDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosedAssertion {
    pub name: Option<Name>,
    pub ast_id: FileAstId<ast::AssertClosedDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Overload {
    pub ast_id: FileAstId<ast::OverloadDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    pub ast_id: FileAstId<ast::DefineDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveMethod {
    pub ast_id: FileAstId<ast::RuleDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetPrecedence {
    pub names: Vec<Name>,
    pub ast_id: FileAstId<ast::SetPrecedenceDir>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhraseStmt {
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
    pub ast_id: FileAstId<ast::SetPrecedenceDir>,
}
