use std::sync::Arc;

use base_db::FileId;
use la_arena::Idx;
use syntax::ast::{self, HasName};

use crate::{ast_map::AstIdMap, db::HirNewDatabase, name::AsName, sort_ref::SortRef, StmtTree};

use super::{FileStmtTreeId, FunctionSymbol, Module, ModuleStmt, StmtTreeData, StmtTreeNode};

pub(super) struct Ctx<'db> {
    db: &'db dyn HirNewDatabase,
    tree: StmtTree,
    ast_id_map: Arc<AstIdMap>,
}

fn id<N: StmtTreeNode>(index: Idx<N>) -> FileStmtTreeId<N> {
    FileStmtTreeId(index)
}

fn into<T, S>(val: T) -> S
where
    T: Into<S>,
{
    val.into()
}

impl<'db> Ctx<'db> {
    fn data(&mut self) -> &mut StmtTreeData {
        &mut self.tree.data
    }
    pub(super) fn new(db: &'db dyn HirNewDatabase, file_id: FileId) -> Self {
        Self {
            db,
            ast_id_map: db.ast_id_map(file_id),
            tree: StmtTree::default(),
        }
    }

    pub(super) fn lower_file(mut self, file: ast::SourceFile) -> StmtTree {
        self.tree.top_level = file
            .stmts()
            .filter_map(|stmt| self.lower_module_stmt(&stmt))
            .collect();

        self.tree
    }

    fn lower_module_stmt(&mut self, stmt: &ast::Stmt) -> Option<ModuleStmt> {
        match stmt {
            ast::Stmt::Dir(ast::Dir::ModuleDir(module)) => self.lower_module_dir(module).map(into),
            ast::Stmt::Dir(ast::Dir::ExtendModuleDir(module_ext)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DeclareDir(func_declare)) => {
                let sym = func_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::ConstantDeclareDir(constant_declare)) => {
                let sym = constant_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::DefineSortDir(alias_def)) => todo!(),
            ast::Stmt::Dir(ast::Dir::LoadDir(load)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DomainDir(domain)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DomainsDir(domains)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssociativityDir(associativity)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssertDir(assert)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssertClosedDir(assert)) => todo!(),
            ast::Stmt::Dir(ast::Dir::OpenDir(open)) => todo!(),
            ast::Stmt::Dir(ast::Dir::OverloadDir(overload)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DefineDir(define)) => todo!(),
            ast::Stmt::Dir(ast::Dir::RuleDir(rule)) => todo!(),
            ast::Stmt::Dir(ast::Dir::SetPrecedenceDir(set_prec)) => todo!(),
            ast::Stmt::PhraseStmt(_) => todo!(),
            ast::Stmt::DatatypeStmt(_) => todo!(),
            ast::Stmt::StructureStmt(_) => todo!(),
            ast::Stmt::DatatypesStmt(_) => todo!(),
            ast::Stmt::StructuresStmt(_) => todo!(),
        }
    }

    fn lower_module_dir(&mut self, module: &ast::ModuleDir) -> Option<FileStmtTreeId<Module>> {
        let name = module.name()?.as_name();
        let stmts = module
            .stmts()
            .filter_map(|stmt| self.lower_module_stmt(&stmt))
            .collect();
        let ast_id = self.ast_id_map.ast_id(module);
        let module = self.data().modules.alloc(Module {
            name,
            stmts,
            ast_id,
        });
        Some(id(module))
    }

    fn lower_term_symbol(
        &mut self,
        term_symbol: &ast::TermSymbol,
    ) -> Option<FileStmtTreeId<FunctionSymbol>> {
        match term_symbol {
            ast::TermSymbol::Function(func) => match func {
                ast::DeclareDir::PrefixDeclareDir(_) => todo!(),
                ast::DeclareDir::InfixDeclareDir(declare) => {
                    let names = declare.names().map(|name| name.as_name()).collect();
                    let arg_sorts = declare
                        .func_sorts()?
                        .sorts()
                        .filter_map(SortRef::from_ast)
                        .collect();
                    let ret_sort = SortRef::from_ast(declare.return_sort()?)?;
                    let ast_id = self.ast_id_map.ast_id(term_symbol);
                    let func = self.data().func_symbols.alloc(FunctionSymbol {
                        names,
                        arg_sorts,
                        ret_sort,
                        ast_id,
                    });
                    Some(id(func))
                }
            },
            ast::TermSymbol::Constant(constant) => match constant {
                ast::ConstantDeclareDir::PrefixConstantDeclare(_) => todo!(),
                ast::ConstantDeclareDir::InfixConstantDeclare(declare) => {
                    let names = declare.names().map(|name| name.as_name()).collect();
                    let ret_sort = SortRef::from_ast(declare.sort()?)?;
                    let ast_id = self.ast_id_map.ast_id(term_symbol);
                    let func = self.data().func_symbols.alloc(FunctionSymbol {
                        names,
                        arg_sorts: Vec::new(),
                        ret_sort,
                        ast_id,
                    });
                    Some(id(func))
                }
            },
        }
    }
}
