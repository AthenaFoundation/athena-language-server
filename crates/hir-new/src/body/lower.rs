use std::sync::Arc;

use base_db::FileId;
use syntax::ast;

use crate::{ast_map::AstIdMap, db::HirNewDatabase, phrase::PhraseId};

use super::Body;

pub(super) struct Ctx<'db> {
    db: &'db dyn HirNewDatabase,
    body: Body,
    ast_id_map: Arc<AstIdMap>,
}

impl<'db> Ctx<'db> {
    pub(super) fn new(db: &'db dyn HirNewDatabase, file_id: FileId) -> Self {
        Self {
            db,
            ast_id_map: db.ast_id_map(file_id),
            body: Body::default(),
        }
    }

    pub(super) fn lower_body(mut self, phrase: &ast::Phrase) -> Body {
        self.body.root = self.lower_phrase(phrase);
        self.body
    }

    fn lower_phrase(&mut self, phrase: &ast::Phrase) -> Option<PhraseId> {
        todo!()
    }
}
