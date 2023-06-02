use crate::{db::HirNewDatabase, fixture};

#[test]
fn foo() {
    use insta::assert_snapshot;
    let (db, file_id) = fixture::file(
        r#"
        //- /foo.ath
        module my-module {
            declare Foo: [int] -> int

            load "virtual:/bar.ath"
        }
        //- /bar.ath
        module beasty {
            declare Bar: [int] -> int
        }
	"#,
    );
    let dump = db.debug_dump(file_id);
    assert_snapshot!(dump);
}
