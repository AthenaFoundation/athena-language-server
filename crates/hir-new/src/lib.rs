mod ast_map;
pub mod db;
pub mod hir;

use base_db::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> Self {
        Self { file_id, value }
    }
}
