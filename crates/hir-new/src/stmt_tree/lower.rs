use std::sync::Arc;

use base_db::FileId;
use syntax::ast;

use crate::{ast_map::AstIdMap, db::HirNewDatabase, StmtTree};

use super::ModuleStmt;

pub(super) struct Ctx<'db> {
    db: &'db dyn HirNewDatabase,
    tree: StmtTree,
    ast_id_map: Arc<AstIdMap>,
}

impl<'db> Ctx<'db> {
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
            ast::Stmt::Dir(ast::Dir::ModuleDir(module)) => todo!(),
            ast::Stmt::Dir(ast::Dir::ExtendModuleDir(module_ext)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DeclareDir(func_declare)) => todo!(),
            ast::Stmt::Dir(ast::Dir::ConstantDeclareDir(constant_declare)) => todo!(),
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

    fn lower_module_dir(&mut self, module: ast::ModuleDir) {
        todo!()
    }
}
