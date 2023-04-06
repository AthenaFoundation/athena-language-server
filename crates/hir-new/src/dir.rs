use la_arena::Idx;
use syntax::ast;

use crate::{ast_map::FileAstId, expr::ExprId, name::Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirKind {
    SetPrecedence(Vec<Name>, ExprId),
    SetAssociativity(Name, Associativity),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dir {
    pub kind: DirKind,
    pub ast_id: FileAstId<ast::Dir>,
}

pub type DirId = Idx<Dir>;
