pub mod line_index;

mod apply_change;

use core::fmt;
use std::sync::Arc;

pub use base_db;

use base_db::{salsa, FileContentsQuery, FileId, FileWatcher, VirtualFileDatabase};
use line_index::LineIndex;
use rustc_hash::FxHashMap;

#[salsa::database(
    base_db::SourceDatabaseStorage,
    base_db::VirtualFileDatabaseStorage,
    LineIndexDatabaseStorage,
    hir::HirDatabaseStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
    in_mem_files: Arc<FxHashMap<base_db::AbsPathBuf, Arc<String>>>,
    virtual_files: Arc<FxHashMap<base_db::VirtualFilePathBuf, Arc<String>>>,
}

impl salsa::Database for RootDatabase {}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish()
    }
}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
            in_mem_files: self.in_mem_files.clone(),
            virtual_files: self.virtual_files.clone(),
        })
    }
}

impl RootDatabase {
    pub fn new() -> RootDatabase {
        RootDatabase {
            storage: salsa::Storage::default(),
            in_mem_files: Default::default(),
            virtual_files: Default::default(),
        }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl FileWatcher for RootDatabase {
    #[tracing::instrument(skip(self))]
    fn did_change_file(&mut self, file_id: FileId) {
        tracing::debug!("acquiring lock maybe?");
        FileContentsQuery.in_db_mut(self).invalidate(&file_id)
    }

    fn in_mem_contents(&self, path: &base_db::AbsPath) -> Option<Arc<String>> {
        self.in_mem_files.get(path).cloned()
    }

    fn set_in_mem_contents(&mut self, path: base_db::AbsPathBuf, contents: Arc<String>) {
        Arc::make_mut(&mut self.in_mem_files).insert(path, contents);
    }

    fn add_virtual_file(&mut self, path: base_db::VirtualFilePathBuf, contents: Arc<String>) {
        Arc::make_mut(&mut self.virtual_files).insert(path.clone(), contents.clone());
        self.set_virtual_file_contents(path, contents);
    }

    fn get_virtual_file(&self, path: base_db::VirtualFilePathBuf) -> Option<Arc<String>> {
        self.virtual_files.get(&path).cloned()
    }
}

#[salsa::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: base_db::SourceDatabase {
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_contents(file_id);
    Arc::new(LineIndex::new(&text))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolKind {
    FnSym,
    Value,
    Sort,
    Func,
    Module,
    Const,
}
