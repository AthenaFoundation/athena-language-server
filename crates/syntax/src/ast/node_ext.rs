use crate::{ast, AstNode, SyntaxNode};
use smol_str::SmolStr;

impl ast::Name {
    pub fn text(&self) -> SmolStr {
        text_of_first_token(self.syntax())
    }
}

impl ast::NameRef {
    pub fn text(&self) -> SmolStr {
        text_of_first_token(self.syntax())
    }
}

fn text_of_first_token(node: &SyntaxNode) -> SmolStr {
    node.green()
        .children()
        .next()
        .and_then(|it| it.into_token())
        .unwrap()
        .text()
        .into()
}

macro_rules! wrapper_enum {
    (@inner { @derive = { #[derive($($derive: tt)*)] }, @vis = $v:vis, @name = $enm:ident, @variants = [$(($variant:ident, $typ: ty)),*] }) => {
        #[derive($($derive)*)]
        $v enum $enm { $($variant($typ)),* }

        $(
            impl From<$typ> for $enm {
                fn from(t: $typ) -> Self {
                    $enm::$variant(t)
                }
            }
        )*

        impl $crate::ast::AstNode for $enm {
            fn can_cast(kind: $crate::SyntaxKind) -> bool {
                $(
                    <$typ as $crate::ast::AstNode>::can_cast(kind)
                )||*
            }

            fn cast(syntax: $crate::SyntaxNode) -> Option<Self> {
                $(
                    if let Some(it) = <$typ as $crate::ast::AstNode>::cast(syntax.clone()).map($enm::$variant) {
                        return Some(it);
                    }
                )*

                None
            }

            fn syntax(&self) -> &$crate::SyntaxNode {
                match self {
                    $(
                        $enm::$variant(it) => it.syntax(),
                    )*
                }
            }
        }
    };
    (#[derive($($derive:tt)*)] $v: vis enum $enm: ident { $($variant:ident ($typ:ty)),* $(,)?}) => {
        wrapper_enum! {
            @inner {
                @derive = { #[derive($($derive)*)] },
                @vis = $v,
                @name = $enm,
                @variants = [$(($variant, $typ)),*]
            }
        }
    };
    ($v: vis enum $enm: ident { $($variant:ident ($typ:ty)),* $(,)?}) => {
        wrapper_enum! {
            @inner {
                @derive = { #[derive(Debug, Clone)] },
                @vis = $v,
                @name = $enm,
                @variants = [$(($variant, $typ)),*]
            }
        }
    };
}

wrapper_enum! {
    #[derive(Debug, Clone)]
    pub enum NameOrNameRef {
        Name(ast::Name),
        NameRef(ast::NameRef),
    }
}

wrapper_enum! {
    #[derive(Debug, Clone)]
    pub enum SortLike {
        Sort(ast::Sort),
        SortDecl(ast::SortDecl),
        LimitedSort(ast::LimitedSort),
    }
}

wrapper_enum! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum DatatypeOrDatatypes {
        Datatype(ast::DatatypeStmt),
        Datatypes(ast::DatatypesStmt),
    }
}

wrapper_enum! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum StructureOrStructures {
        Structure(ast::StructureStmt),
        Structures(ast::StructuresStmt),
    }
}
