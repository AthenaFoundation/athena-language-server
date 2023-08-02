use core::fmt;

use crate::db::HirDatabase;
use base_db::{salsa, FileWatcher, Upcast, VirtualFileDatabase, VirtualFilePathBuf};
use rustc_hash::FxHashSet;

#[derive(Default)]
#[salsa::database(
    base_db::SourceDatabaseStorage,
    crate::db::HirDatabaseStorage,
    base_db::VirtualFileDatabaseStorage
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
        self.virtual_files.insert(path.clone());
        self.set_virtual_file_contents(path, contents);
    }

    fn get_virtual_file(&self, path: VirtualFilePathBuf) -> Option<std::sync::Arc<String>> {
        if self.virtual_files.contains(&path) {
            Some(self.virtual_file_contents(path))
        } else {
            None
        }
    }
}

impl Upcast<dyn HirDatabase> for TestDB {
    fn upcast(&self) -> &(dyn HirDatabase + 'static) {
        self
    }
}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}
