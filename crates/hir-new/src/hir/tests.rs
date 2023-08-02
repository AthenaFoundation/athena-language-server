use insta::assert_snapshot;

use crate::{db::HirNewDatabase, fixture};

macro_rules! function {
    () => {{
        // man this is ugly: https://stackoverflow.com/questions/38088067/equivalent-of-func-or-function-in-rust
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);

        // Find and cut the rest of the path
        match &name[..name.len() - 3].rfind(':') {
            Some(pos) => &name[pos + 1..name.len() - 3],
            None => &name[..name.len() - 3],
        }
    }};
}

#[track_caller]
fn check(name: &str, ra_fixture: &str) {
    let (db, file_id) = fixture::file(ra_fixture);
    let dump = db.debug_dump(file_id);
    insta::with_settings!({description => ra_fixture, omit_expression => true }, { assert_snapshot!(name, dump) });
}

macro_rules! test {
    ($input: literal) => {
        check(function!(), $input)
    };
}

#[test]
fn load_lowers() {
    test!(
        r#"
        //- /foo.ath
        module my-module {
            declare Foo: [int] -> int

            load "virtual:/bar.ath"
        }
        //- /bar.ath
        module barry {
            declare Bar: (A, B) [int (List A) (List B)] -> int
        }
        "#
    );
}

#[test]
fn define_lowers() {
    test!(
        r#"
        //- /foo.ath
        module my-module {
            define Foo := 1;
        }
        "#
    );
}
