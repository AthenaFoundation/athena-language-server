use crate::name::{AsName, Name};
use la_arena::Idx;
use syntax::{
    ast::{self, HasNameRef},
    AstToken,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortRef {
    Var(Name),
    Ident(Name),
    Apply(Box<SortRef>, Vec<SortRef>),
}

pub type SortRefId = Idx<SortRef>;

impl SortRef {
    pub fn from_ast(node: ast::Sort) -> Option<Self> {
        match node {
            ast::Sort::VarSort(v) => {
                let name = ast::Ident::cast(v.ident_token()?)?.as_name();
                Some(SortRef::Var(name))
            }
            ast::Sort::IdentSort(name) => Some(SortRef::Ident(name.name_ref()?.as_name())),
            ast::Sort::CompoundSort(compound) => {
                let mut sorts = compound.sorts();
                let first = sorts.next()?;
                let first = Self::from_ast(first)?;

                let rest = sorts.filter_map(Self::from_ast).collect::<Vec<_>>();

                Some(SortRef::Apply(Box::new(first), rest))
            }
        }
    }
}
