//! Utilities for creating `Analysis` instances for tests.

#![allow(dead_code)]

use base_db::{fixture::ChangeFixture, FileId};

use crate::test_db::TestDB;

/// Creates analysis for a single file.
pub(crate) fn file(ra_fixture: &str) -> (TestDB, FileId) {
    let mut db = TestDB::default();
    let mut change_fixture = ChangeFixture::parse(ra_fixture);
    change_fixture.apply(&mut db);
    (db, change_fixture.files[0])
}
