use crate::grammar::identifier;
use crate::grammar::patterns::pat;
use crate::grammar::phrases::phrase;
use crate::grammar::sorts::{sort_decl, SORT_DECL_START};
use crate::grammar::statements::stmt;
use crate::parser::Parser;
use crate::token_set::TokenSet;
use crate::{
    SyntaxKind::{self, IDENT},
    T,
};

fn module_dir(p: &mut Parser) {
    assert!(p.at(T![module]));

    let m = p.start();
    p.bump(T![module]);

    if !p.at(IDENT) {
        p.error("expected module name");
    } else {
        identifier(p);
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(dir) module_empty
        // module foo { }
        p.error("expected module body");
    }

    // test_err(dir) module_no_rbrace
    // module foo {
    while !p.at(T!['}']) && !p.at_end() {
        stmt(p);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::MODULE_DIR);
}

// test(dir) domain_directive
// domain Bar

// test(dir) domain_complex_sort
// domain (List T)
fn domain_dir(p: &mut Parser) {
    assert!(p.at(T![domain]));

    let m = p.start();
    p.bump(T![domain]);

    if !sort_decl(p) {
        // test_err(dir) domain_empty
        // domain
        p.error("expected domain sort declaration");
    }

    m.complete(p, SyntaxKind::DOMAIN_DIR);
}

// test(dir) domains_directive
// domains Bar, Baz

// test(dir) domains_complex_sort
// domains (List T), (Set U)
fn domains_dir(p: &mut Parser) {
    assert!(p.at(T![domains]));

    let m = p.start();
    p.bump(T![domains]);

    if !sort_decl(p) {
        // test_err(dir) domains_empty
        // domains
        p.error("expected at least one sort declaration in domains directive");
    }

    while p.at(T![,]) || p.at_one_of(SORT_DECL_START) {
        p.expect(T![,]);
        if !sort_decl(p) {
            p.error("expected a sort declaration");
        }
    }

    m.complete(p, SyntaxKind::DOMAINS_DIR);
}

fn define_dir(p: &mut Parser) {
    assert!(p.at(T![define]));

    let m = p.start();
    p.bump(T![define]);

    if !p.at(IDENT) {
        // test_err(dir) define_no_name
        // define := true
        p.error("expected definition name");
    } else {
        identifier(p);
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_empty
        // define foo :=
        p.error("expected definition value");
    }

    m.complete(p, SyntaxKind::DEFINE_DIR);
}

// test(dir) define_proc
// define (foo a b) := lambda () b
fn define_proc_dir(p: &mut Parser) {
    assert!(p.at(T![define]) && p.peek_at(T!['(']));

    let m = p.start();

    p.bump(T![define]);

    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(dir) define_proc_no_name
        // define ( ) := lambda () true
        p.error("expected procedure name");
    } else {
        identifier(p);
    }

    // test_err(dir) define_proc_no_rparen
    // define (foo := lambda () true

    while !p.at(T![')']) && !p.at(T![:=]) {
        if p.at(IDENT) {
            identifier(p);
        } else {
            p.error("expected argument name");
        }
    }

    p.expect(T![')']);

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_proc_empty
        // define (foo) :=
        p.error("expected procedure body");
    }

    m.complete(p, SyntaxKind::DEFINE_PROC_DIR);
}

// test(dir) define_multi
// define [foo bar baz] := [true false true]
fn define_multi(p: &mut Parser) {
    assert!(p.at(T![define]) && p.peek_at(T!['[']));

    let m = p.start();
    p.bump(T![define]);

    p.bump(T!['[']);

    while !p.at(T![']']) && pat(p) {}

    p.expect(T![']']);

    p.expect(T![:=]);

    p.expect(T!['[']);

    while !p.at(T![']']) && phrase(p) {}

    p.expect(T![']']);

    m.complete(p, SyntaxKind::DEFINE_MULTI_DIR);
}

pub(crate) const DIR_START_SET: TokenSet =
    TokenSet::new(&[T![module], T![domain], T![domains], T![define], T![declare]]);

pub(crate) fn dir(p: &mut Parser) -> bool {
    if p.at(T![module]) {
        module_dir(p);
    } else if p.at(T![domain]) {
        domain_dir(p);
    } else if p.at(T![domains]) {
        domains_dir(p);
    } else if p.at(T![define]) {
        if p.peek_at(T!['(']) {
            define_proc_dir(p);
        } else if p.peek_at(T!['[']) {
            define_multi(p);
        } else {
            define_dir(p);
        }
    } else {
        return false;
    }

    true
}
