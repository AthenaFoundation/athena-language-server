use std::sync::Arc;

use base_db::{FileId, Upcast};

use crate::{ast_map::AstDatabase, stmt_tree::StmtTree};
use base_db::salsa;

#[salsa::query_group(HirNewDatabaseStorage)]
pub trait HirNewDatabase: AstDatabase + Upcast<dyn AstDatabase> {
    #[salsa::invoke(StmtTree::file_stmt_tree_query)]
    fn file_stmt_tree(&self, file_id: FileId) -> Arc<StmtTree>;
}
