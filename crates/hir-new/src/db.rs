use std::sync::Arc;

use base_db::{FileId, Upcast};

use crate::{
    ast_map::AstDatabase,
    hir::{FileHir, SourceMap},
};
use base_db::salsa;

#[salsa::query_group(HirNewDatabaseStorage)]
pub trait HirNewDatabase: AstDatabase + Upcast<dyn AstDatabase> {
    #[salsa::invoke(FileHir::file_hir_with_source_map_query)]
    fn file_hir_with_source_map(&self, file_id: FileId) -> (Arc<FileHir>, Arc<SourceMap>);

    fn file_hir(&self, file_id: FileId) -> Arc<FileHir>;

    fn source_map(&self, file_id: FileId) -> Arc<SourceMap>;
}

fn file_hir(db: &dyn HirNewDatabase, file_id: FileId) -> Arc<FileHir> {
    db.file_hir_with_source_map(file_id).0
}

fn source_map(db: &dyn HirNewDatabase, file_id: FileId) -> Arc<SourceMap> {
    db.file_hir_with_source_map(file_id).1
}
