use core::fmt;

use crate::{ast_map::AstDatabase, db::HirNewDatabase};
use base_db::{salsa, FileWatcher, Upcast, VirtualFileDatabase, VirtualFilePathBuf};
use rustc_hash::FxHashSet;

#[derive(Default)]
#[salsa::database(
    base_db::SourceDatabaseStorage,
    crate::db::HirNewDatabaseStorage,
    base_db::VirtualFileDatabaseStorage,
    crate::ast_map::AstDatabaseStorage
)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,

    virtual_files: FxHashSet<VirtualFilePathBuf>,
}

impl salsa::Database for TestDB {}

impl FileWatcher for TestDB {
    fn did_change_file(&mut self, _file_id: base_db::FileId) {}

    fn in_mem_contents(&self, _path: &base_db::AbsPath) -> Option<std::sync::Arc<String>> {
        None
    }

    fn set_in_mem_contents(
        &mut self,
        _path: base_db::AbsPathBuf,
        _contents: std::sync::Arc<String>,
    ) {
    }

    fn add_virtual_file(&mut self, path: VirtualFilePathBuf, contents: std::sync::Arc<String>) {
        self.virtual_files.insert(dbg!(path.clone()));
        self.set_virtual_file_contents(path, contents);
    }

    fn get_virtual_file(&self, path: VirtualFilePathBuf) -> Option<std::sync::Arc<String>> {
        if self.virtual_files.contains(dbg!(&path)) {
            Some(self.virtual_file_contents(path.into()))
        } else {
            None
        }
    }
}

impl Upcast<dyn HirNewDatabase> for TestDB {
    fn upcast(&self) -> &(dyn HirNewDatabase + 'static) {
        self
    }
}

impl Upcast<dyn AstDatabase> for TestDB {
    fn upcast(&self) -> &(dyn AstDatabase + 'static) {
        self
    }
}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}
