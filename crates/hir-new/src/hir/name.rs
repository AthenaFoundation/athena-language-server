use core::fmt;

use smol_str::SmolStr;
use syntax::{ast, AstToken};
use util::impl_from;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(SmolStr);

impl Name {
    pub fn new_trustme(text: impl Into<SmolStr>) -> Name {
        Name(text.into())
    }
}

pub trait AsName {
    fn as_name(&self) -> Name;
}

impl AsName for ast::Ident {
    fn as_name(&self) -> Name {
        Name(self.text().into())
    }
}

impl AsName for ast::Name {
    fn as_name(&self) -> Name {
        Name(self.text())
    }
}

impl AsName for ast::NameRef {
    fn as_name(&self) -> Name {
        Name(self.text())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NameOrWildcard {
    Name(Name),
    Wildcard,
}

impl_from!(Name for NameOrWildcard);
