//! Generated by `sourcegen_ast`, do not edit by hand.

#![allow(bad_style, missing_docs, unreachable_pub)]
/// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    #[doc(hidden)]
    TOMBSTONE,
    #[doc(hidden)]
    EOF,
    L_PAREN,
    R_PAREN,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,
    QUESTION,
    UNDERSCORE,
    COLON,
    FAT_ARROW,
    BANG,
    AMP2,
    PIPE2,
    COLON_EQ,
    SINGLE_QUOTE,
    SEMI,
    THIN_ARROW,
    COMMA,
    PIPE_CURLY,
    CURLY_PIPE,
    WHILE_KW,
    LET_KW,
    LETREC_KW,
    TRY_KW,
    CHECK_KW,
    LAMBDA_KW,
    METHOD_KW,
    MATCH_KW,
    CELL_KW,
    SETBANG_KW,
    REF_KW,
    MAKE_VECTOR_KW,
    VECTOR_SUB_KW,
    VECTOR_SETBANG_KW,
    SEQ_KW,
    APPLY_METHOD_KW,
    CONCLUDE_KW,
    ASSUME_KW,
    SUPPOSE_ABSURD_KW,
    GENERALIZE_OVER_KW,
    PICK_ANY_KW,
    WITH_WITNESS_KW,
    PICK_WITNESS_KW,
    PICK_WITNESSES_KW,
    BY_INDUCTION_KW,
    DATATYPE_CASES_KW,
    SOME_VAR_KW,
    SOME_SENT_CON_KW,
    SOME_QUANT_KW,
    SOME_TERM_KW,
    SOME_ATOM_KW,
    SOME_SENTENCE_KW,
    SOME_LIST_KW,
    SOME_CELL_KW,
    SOME_VECTOR_KW,
    SOME_PROC_KW,
    SOME_METHOD_KW,
    SOME_SYMBOL_KW,
    SOME_TABLE_KW,
    SOME_MAP_KW,
    SOME_SUB_KW,
    SOME_CHAR_KW,
    SPLIT_KW,
    WHERE_KW,
    LIST_OF_KW,
    VAL_OF_KW,
    AS_KW,
    BIND_KW,
    FOR_KW,
    DEFINE_KW,
    MODULE_KW,
    DECLARE_KW,
    DOMAIN_KW,
    DOMAINS_KW,
    LOAD_KW,
    ASSERT_KW,
    ASSERT_STAR_KW,
    LEFT_ASSOC_KW,
    RIGHT_ASSOC_KW,
    DATATYPE_KW,
    STRUCTURE_KW,
    DATATYPES_KW,
    STRUCTURES_KW,
    EXTEND_MODULE_KW,
    PRIVATE_KW,
    PIPE,
    INT_NUMBER,
    STRING,
    CHAR,
    ERROR,
    IDENT,
    WHITESPACE,
    COMMENT,
    SOURCE_FILE,
    IDENTIFIER,
    LITERAL,
    META_IDENT,
    UNIT,
    IDENT_SORT,
    VAR_SORT,
    COMPOUND_SORT,
    EXPR_PHRASE,
    DED_PHRASE,
    IDENT_EXPR,
    LITERAL_EXPR,
    UNIT_EXPR,
    META_IDENT_EXPR,
    TERM_VAR_EXPR,
    CHECK_EXPR,
    LAMBDA_EXPR,
    APPLICATION_EXPR,
    LIST_EXPR,
    METHOD_EXPR,
    LET_EXPR,
    LET_REC_EXPR,
    MATCH_EXPR,
    TRY_EXPR,
    CELL_EXPR,
    SET_EXPR,
    REF_EXPR,
    WHILE_EXPR,
    MAKE_VECTOR_EXPR,
    VECTOR_SUB_EXPR,
    VECTOR_SET_EXPR,
    SEQ_EXPR,
    AND_EXPR,
    OR_EXPR,
    MAP_EXPR,
    MAP_BINDING,
    CONCLUDE_DED,
    METHOD_CALL_DED,
    BANG_METHOD_CALL_DED,
    ASSUME_DED,
    NAMED_ASSUME_DED,
    PROOF_BY_CONTRA_DED,
    GENERALIZE_OVER_DED,
    PICK_ANY_DED,
    WITH_WITNESS_DED,
    PICK_WITNESS_DED,
    PICK_WITNESSES_DED,
    INDUCT_DED,
    CASES_DED,
    CHECK_DED,
    MATCH_DED,
    LET_DED,
    LET_REC_DED,
    TRY_DED,
    TRY_DED_ARM,
    MATCH_DED_ARM,
    CHECK_DED_ARM,
    RESTRICTED_MATCH_DED,
    RESTRICTED_NAMED_PAT,
    RESTRICTED_APPLY_PAT,
    ASSUME_PART,
    IDENT_PAT,
    ANNOTATED_IDENT_PAT,
    VAR_PAT,
    META_IDENT_PAT,
    LITERAL_PAT,
    UNIT_PAT,
    WILDCARD_PAT,
    NAMED_PAT,
    VAL_OF_PAT,
    LIST_OF_PAT,
    SPLIT_PAT,
    LIST_PAT,
    COMPOUND_PAT,
    WHERE_PAT,
    SOME_THING_PAT,
    SOME_THING,
    MATCH_ARM,
    TRY_ARM,
    LET_PART,
    CHECK_ARM,
    LET_REC_PART,
    TYPED_PARAM,
    MAYBE_WILDCARD_TYPED_PARAM,
    FUNC_SORTS,
    SORT_VARS_DECL,
    COMPOUND_SORT_DECL,
    DEFINE_DIR,
    DEFINE_MULTI_DIR,
    DEFINE_PROC_DIR,
    DOMAIN_DIR,
    DOMAINS_DIR,
    DECLARE_DIR,
    MODULE_DIR,
    LOAD_DIR,
    ASSERT_DIR,
    ASSERT_CLOSED_DIR,
    DECLARE_ATTR,
    DECLARE_ATTRS,
    INPUT_TRANSFORM_DECL,
    EXTEND_MODULE_DIR,
    DEFINE_NAMED_PATTERN,
    DIR_STMT,
    PHRASE_STMT,
    STRUCTURE_NAME_DEF,
    CONSTANT_CONSTRUCTOR,
    COMPOUND_CONSTRUCTOR,
    MAYBE_TAGGED_SORT_DECL,
    STRUCTURE_DEF,
    DATATYPE_STMT,
    DATATYPES_STMT,
    STRUCTURE_STMT,
    STRUCTURES_STMT,
    #[doc(hidden)]
    __LAST,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    pub const LAST_TOKEN: SyntaxKind = SyntaxKind::COMMENT;
    pub fn is_keyword(self) -> bool {
        matches!(
            self, WHILE_KW | LET_KW | LETREC_KW | TRY_KW | CHECK_KW | LAMBDA_KW |
            METHOD_KW | MATCH_KW | CELL_KW | SETBANG_KW | REF_KW | MAKE_VECTOR_KW |
            VECTOR_SUB_KW | VECTOR_SETBANG_KW | SEQ_KW | APPLY_METHOD_KW | CONCLUDE_KW |
            ASSUME_KW | SUPPOSE_ABSURD_KW | GENERALIZE_OVER_KW | PICK_ANY_KW |
            WITH_WITNESS_KW | PICK_WITNESS_KW | PICK_WITNESSES_KW | BY_INDUCTION_KW |
            DATATYPE_CASES_KW | SOME_VAR_KW | SOME_SENT_CON_KW | SOME_QUANT_KW |
            SOME_TERM_KW | SOME_ATOM_KW | SOME_SENTENCE_KW | SOME_LIST_KW | SOME_CELL_KW
            | SOME_VECTOR_KW | SOME_PROC_KW | SOME_METHOD_KW | SOME_SYMBOL_KW |
            SOME_TABLE_KW | SOME_MAP_KW | SOME_SUB_KW | SOME_CHAR_KW | SPLIT_KW |
            WHERE_KW | LIST_OF_KW | VAL_OF_KW | AS_KW | BIND_KW | FOR_KW | DEFINE_KW |
            MODULE_KW | DECLARE_KW | DOMAIN_KW | DOMAINS_KW | LOAD_KW | ASSERT_KW |
            ASSERT_STAR_KW | LEFT_ASSOC_KW | RIGHT_ASSOC_KW | DATATYPE_KW | STRUCTURE_KW
            | DATATYPES_KW | STRUCTURES_KW | EXTEND_MODULE_KW | PRIVATE_KW | PIPE
        )
    }
    pub fn is_punct(self) -> bool {
        matches!(
            self, L_PAREN | R_PAREN | L_CURLY | R_CURLY | L_BRACK | R_BRACK | QUESTION |
            UNDERSCORE | COLON | FAT_ARROW | BANG | AMP2 | PIPE2 | COLON_EQ |
            SINGLE_QUOTE | SEMI | THIN_ARROW | COMMA | PIPE_CURLY | CURLY_PIPE
        )
    }
    pub fn is_literal(self) -> bool {
        matches!(self, INT_NUMBER | STRING | CHAR)
    }
    pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
        let kw = match ident {
            "while" => WHILE_KW,
            "let" => LET_KW,
            "letrec" => LETREC_KW,
            "try" => TRY_KW,
            "check" => CHECK_KW,
            "lambda" => LAMBDA_KW,
            "method" => METHOD_KW,
            "match" => MATCH_KW,
            "cell" => CELL_KW,
            "set!" => SETBANG_KW,
            "ref" => REF_KW,
            "make-vector" => MAKE_VECTOR_KW,
            "vector-sub" => VECTOR_SUB_KW,
            "vector-set!" => VECTOR_SETBANG_KW,
            "seq" => SEQ_KW,
            "apply-method" => APPLY_METHOD_KW,
            "conclude" => CONCLUDE_KW,
            "assume" => ASSUME_KW,
            "suppose-absurd" => SUPPOSE_ABSURD_KW,
            "generalize-over" => GENERALIZE_OVER_KW,
            "pick-any" => PICK_ANY_KW,
            "with-witness" => WITH_WITNESS_KW,
            "pick-witness" => PICK_WITNESS_KW,
            "pick-witnesses" => PICK_WITNESSES_KW,
            "by-induction" => BY_INDUCTION_KW,
            "datatype-cases" => DATATYPE_CASES_KW,
            "some-var" => SOME_VAR_KW,
            "some-sent-con" => SOME_SENT_CON_KW,
            "some-quant" => SOME_QUANT_KW,
            "some-term" => SOME_TERM_KW,
            "some-atom" => SOME_ATOM_KW,
            "some-sentence" => SOME_SENTENCE_KW,
            "some-list" => SOME_LIST_KW,
            "some-cell" => SOME_CELL_KW,
            "some-vector" => SOME_VECTOR_KW,
            "some-proc" => SOME_PROC_KW,
            "some-method" => SOME_METHOD_KW,
            "some-symbol" => SOME_SYMBOL_KW,
            "some-table" => SOME_TABLE_KW,
            "some-map" => SOME_MAP_KW,
            "some-sub" => SOME_SUB_KW,
            "some-char" => SOME_CHAR_KW,
            "split" => SPLIT_KW,
            "where" => WHERE_KW,
            "list-of" => LIST_OF_KW,
            "val-of" => VAL_OF_KW,
            "as" => AS_KW,
            "bind" => BIND_KW,
            "for" => FOR_KW,
            "define" => DEFINE_KW,
            "module" => MODULE_KW,
            "declare" => DECLARE_KW,
            "domain" => DOMAIN_KW,
            "domains" => DOMAINS_KW,
            "load" => LOAD_KW,
            "assert" => ASSERT_KW,
            "assert*" => ASSERT_STAR_KW,
            "left-assoc" => LEFT_ASSOC_KW,
            "right-assoc" => RIGHT_ASSOC_KW,
            "datatype" => DATATYPE_KW,
            "structure" => STRUCTURE_KW,
            "datatypes" => DATATYPES_KW,
            "structures" => STRUCTURES_KW,
            "extend-module" => EXTEND_MODULE_KW,
            "private" => PRIVATE_KW,
            _ => return None,
        };
        Some(kw)
    }
    pub fn from_contextual_keyword(ident: &str) -> Option<SyntaxKind> {
        #[allow(unused_variables, unreachable_code)]
        {
            let kw = match ident {
                "|" => PIPE,
                _ => return None,
            };
            Some(kw)
        }
    }
    pub fn from_char(c: char) -> Option<SyntaxKind> {
        let tok = match c {
            '(' => L_PAREN,
            ')' => R_PAREN,
            '{' => L_CURLY,
            '}' => R_CURLY,
            '[' => L_BRACK,
            ']' => R_BRACK,
            '?' => QUESTION,
            '_' => UNDERSCORE,
            ':' => COLON,
            '!' => BANG,
            '\'' => SINGLE_QUOTE,
            ';' => SEMI,
            ',' => COMMA,
            _ => return None,
        };
        Some(tok)
    }
}
#[macro_export]
macro_rules! T {
    ['('] => {
        $crate ::SyntaxKind::L_PAREN
    };
    [')'] => {
        $crate ::SyntaxKind::R_PAREN
    };
    ['{'] => {
        $crate ::SyntaxKind::L_CURLY
    };
    ['}'] => {
        $crate ::SyntaxKind::R_CURLY
    };
    ['['] => {
        $crate ::SyntaxKind::L_BRACK
    };
    [']'] => {
        $crate ::SyntaxKind::R_BRACK
    };
    [?] => {
        $crate ::SyntaxKind::QUESTION
    };
    [_] => {
        $crate ::SyntaxKind::UNDERSCORE
    };
    [:] => {
        $crate ::SyntaxKind::COLON
    };
    [=>] => {
        $crate ::SyntaxKind::FAT_ARROW
    };
    [!] => {
        $crate ::SyntaxKind::BANG
    };
    [&&] => {
        $crate ::SyntaxKind::AMP2
    };
    [||] => {
        $crate ::SyntaxKind::PIPE2
    };
    [:=] => {
        $crate ::SyntaxKind::COLON_EQ
    };
    ['\''] => {
        $crate ::SyntaxKind::SINGLE_QUOTE
    };
    [;] => {
        $crate ::SyntaxKind::SEMI
    };
    [->] => {
        $crate ::SyntaxKind::THIN_ARROW
    };
    [,] => {
        $crate ::SyntaxKind::COMMA
    };
    ["|{"] => {
        $crate ::SyntaxKind::PIPE_CURLY
    };
    ["}|"] => {
        $crate ::SyntaxKind::CURLY_PIPE
    };
    [while] => {
        $crate ::SyntaxKind::WHILE_KW
    };
    [let] => {
        $crate ::SyntaxKind::LET_KW
    };
    [letrec] => {
        $crate ::SyntaxKind::LETREC_KW
    };
    [try] => {
        $crate ::SyntaxKind::TRY_KW
    };
    [check] => {
        $crate ::SyntaxKind::CHECK_KW
    };
    [lambda] => {
        $crate ::SyntaxKind::LAMBDA_KW
    };
    [method] => {
        $crate ::SyntaxKind::METHOD_KW
    };
    [match] => {
        $crate ::SyntaxKind::MATCH_KW
    };
    [cell] => {
        $crate ::SyntaxKind::CELL_KW
    };
    [set!] => {
        $crate ::SyntaxKind::SETBANG_KW
    };
    [ref] => {
        $crate ::SyntaxKind::REF_KW
    };
    [make - vector] => {
        $crate ::SyntaxKind::MAKE_VECTOR_KW
    };
    [vector - sub] => {
        $crate ::SyntaxKind::VECTOR_SUB_KW
    };
    [vector - set!] => {
        $crate ::SyntaxKind::VECTOR_SETBANG_KW
    };
    [seq] => {
        $crate ::SyntaxKind::SEQ_KW
    };
    [apply - method] => {
        $crate ::SyntaxKind::APPLY_METHOD_KW
    };
    [conclude] => {
        $crate ::SyntaxKind::CONCLUDE_KW
    };
    [assume] => {
        $crate ::SyntaxKind::ASSUME_KW
    };
    [suppose - absurd] => {
        $crate ::SyntaxKind::SUPPOSE_ABSURD_KW
    };
    [generalize - over] => {
        $crate ::SyntaxKind::GENERALIZE_OVER_KW
    };
    [pick - any] => {
        $crate ::SyntaxKind::PICK_ANY_KW
    };
    [with - witness] => {
        $crate ::SyntaxKind::WITH_WITNESS_KW
    };
    [pick - witness] => {
        $crate ::SyntaxKind::PICK_WITNESS_KW
    };
    [pick - witnesses] => {
        $crate ::SyntaxKind::PICK_WITNESSES_KW
    };
    [by - induction] => {
        $crate ::SyntaxKind::BY_INDUCTION_KW
    };
    [datatype - cases] => {
        $crate ::SyntaxKind::DATATYPE_CASES_KW
    };
    [some - var] => {
        $crate ::SyntaxKind::SOME_VAR_KW
    };
    [some - sent - con] => {
        $crate ::SyntaxKind::SOME_SENT_CON_KW
    };
    [some - quant] => {
        $crate ::SyntaxKind::SOME_QUANT_KW
    };
    [some - term] => {
        $crate ::SyntaxKind::SOME_TERM_KW
    };
    [some - atom] => {
        $crate ::SyntaxKind::SOME_ATOM_KW
    };
    [some - sentence] => {
        $crate ::SyntaxKind::SOME_SENTENCE_KW
    };
    [some - list] => {
        $crate ::SyntaxKind::SOME_LIST_KW
    };
    [some - cell] => {
        $crate ::SyntaxKind::SOME_CELL_KW
    };
    [some - vector] => {
        $crate ::SyntaxKind::SOME_VECTOR_KW
    };
    [some - proc] => {
        $crate ::SyntaxKind::SOME_PROC_KW
    };
    [some - method] => {
        $crate ::SyntaxKind::SOME_METHOD_KW
    };
    [some - symbol] => {
        $crate ::SyntaxKind::SOME_SYMBOL_KW
    };
    [some - table] => {
        $crate ::SyntaxKind::SOME_TABLE_KW
    };
    [some - map] => {
        $crate ::SyntaxKind::SOME_MAP_KW
    };
    [some - sub] => {
        $crate ::SyntaxKind::SOME_SUB_KW
    };
    [some - char] => {
        $crate ::SyntaxKind::SOME_CHAR_KW
    };
    [split] => {
        $crate ::SyntaxKind::SPLIT_KW
    };
    [where] => {
        $crate ::SyntaxKind::WHERE_KW
    };
    [list - of] => {
        $crate ::SyntaxKind::LIST_OF_KW
    };
    [val - of] => {
        $crate ::SyntaxKind::VAL_OF_KW
    };
    [as] => {
        $crate ::SyntaxKind::AS_KW
    };
    [bind] => {
        $crate ::SyntaxKind::BIND_KW
    };
    [for] => {
        $crate ::SyntaxKind::FOR_KW
    };
    [define] => {
        $crate ::SyntaxKind::DEFINE_KW
    };
    [module] => {
        $crate ::SyntaxKind::MODULE_KW
    };
    [declare] => {
        $crate ::SyntaxKind::DECLARE_KW
    };
    [domain] => {
        $crate ::SyntaxKind::DOMAIN_KW
    };
    [domains] => {
        $crate ::SyntaxKind::DOMAINS_KW
    };
    [load] => {
        $crate ::SyntaxKind::LOAD_KW
    };
    [assert] => {
        $crate ::SyntaxKind::ASSERT_KW
    };
    [assert *] => {
        $crate ::SyntaxKind::ASSERT_STAR_KW
    };
    [left - assoc] => {
        $crate ::SyntaxKind::LEFT_ASSOC_KW
    };
    [right - assoc] => {
        $crate ::SyntaxKind::RIGHT_ASSOC_KW
    };
    [datatype] => {
        $crate ::SyntaxKind::DATATYPE_KW
    };
    [structure] => {
        $crate ::SyntaxKind::STRUCTURE_KW
    };
    [datatypes] => {
        $crate ::SyntaxKind::DATATYPES_KW
    };
    [structures] => {
        $crate ::SyntaxKind::STRUCTURES_KW
    };
    [extend - module] => {
        $crate ::SyntaxKind::EXTEND_MODULE_KW
    };
    [private] => {
        $crate ::SyntaxKind::PRIVATE_KW
    };
    [|] => {
        $crate ::SyntaxKind::PIPE
    };
    [lifetime_ident] => {
        $crate ::SyntaxKind::LIFETIME_IDENT
    };
    [ident] => {
        $crate ::SyntaxKind::IDENT
    };
    [shebang] => {
        $crate ::SyntaxKind::SHEBANG
    };
}
pub use T;
