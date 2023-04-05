use crate::{expr::ExprId, name::Name};

pub enum Associativity {
    Left,
    Right,
}

pub enum Dir {
    SetPrecedence(Vec<Name>, ExprId),
    SetAssociativity(Name, Associativity),
}
