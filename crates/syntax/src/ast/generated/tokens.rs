//! Generated by `sourcegen_ast`, do not edit by hand.

use crate::{
    SyntaxKind::{self, *},
    SyntaxToken, ast::AstToken,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Whitespace {
    pub(crate) syntax: SyntaxToken,
}
impl std::fmt::Display for Whitespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}
impl AstToken for Whitespace {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == WHITESPACE
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    pub(crate) syntax: SyntaxToken,
}
impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}
impl AstToken for Comment {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == COMMENT
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String {
    pub(crate) syntax: SyntaxToken,
}
impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}
impl AstToken for String {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == STRING
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub(crate) syntax: SyntaxToken,
}
impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}
impl AstToken for Ident {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == IDENT
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
