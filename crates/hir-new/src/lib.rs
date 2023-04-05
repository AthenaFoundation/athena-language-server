mod ast_map;
mod ded;
mod dir;
mod expr;
mod name;
mod pat;
mod phrase;
mod sort_ref;
mod stmt_tree;

pub mod db;

pub use stmt_tree::StmtTree;

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
