mod ded;
mod dir;
mod expr;
mod lower;
mod name;
mod pat;
mod phrase;
mod sort_ref;
mod stmt;

use std::{hash::BuildHasherDefault, sync::Arc};

use base_db::FileId;
use la_arena::{Arena, Idx};
use rustc_hash::FxHasher;
use syntax::{ast, AstNode, AstPtr};

use crate::{ast_map::FileAstId, db::HirNewDatabase, InFile};

pub use self::{
    ded::Ded,
    dir::Dir,
    expr::Expr,
    name::{AsName, Name, NameOrWildcard},
    pat::Pat,
    phrase::{Phrase, PhraseId},
    sort_ref::SortRef,
    stmt::{
        Assertion, ClosedAssertion, DefKind, Definition, DomainDeclaration, FileImport,
        FunctionSymbol, Module, ModuleExtension, ModuleImport, Param, SortAlias,
    },
};

pub type FxBiMap<L, R> = bimap::BiHashMap<L, R, BuildHasherDefault<FxHasher>>;

#[derive(Debug, Eq, PartialEq)]
pub struct FileHir {
    file_id: FileId,
    top_level: Vec<ModuleStmt>,
    data: HirData, // generated by hir_maps! below
}

impl FileHir {
    fn empty(file_id: FileId) -> Self {
        Self {
            file_id,
            top_level: Vec::new(),
            data: HirData::default(),
        }
    }

    pub(crate) fn file_hir_with_source_map_query(
        db: &dyn HirNewDatabase,
        file_id: FileId,
    ) -> (Arc<FileHir>, Arc<SourceMap>) {
        let file = db.parse(file_id).tree();
        let ctx = lower::Ctx::new(db, file_id);
        let (hir, map) = ctx.lower_file(file);
        (Arc::new(hir), Arc::new(map))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourceMap {
    file_id: FileId,
    ast_id_map: Arc<crate::ast_map::AstIdMap>,
    hir_map: HirAstMap, // generated by hir_maps! below
}

impl SourceMap {
    fn empty(file_id: FileId, ast_id_map: Arc<crate::ast_map::AstIdMap>) -> Self {
        Self {
            file_id,
            ast_id_map,
            hir_map: HirAstMap::default(),
        }
    }
}

pub trait HasHir {
    type Hir: HirNode;

    fn hir(&self, source_map: &SourceMap) -> Option<Idx<Self::Hir>>;

    fn file_id(&self) -> FileId;
}

pub type Source<N> = InFile<AstPtr<N>>;

pub trait HirNode: Sized {
    type Ast: AstNode;

    fn source(id: Idx<Self>, source_map: &SourceMap) -> Option<Source<Self::Ast>>;

    fn node(id: Idx<Self>, hir: &FileHir) -> &Self;

    fn alloc(self, hir: &mut FileHir) -> Idx<Self>;

    fn record_source(id: Idx<Self>, file_ast_id: FileAstId<Self::Ast>, source_map: &mut SourceMap);
}

macro_rules! hir_maps {
	(@inner $({ ast: $ast: ty, hir: $hir: ty, arena: $arena: ident}),*) => {
		#[derive(Default, Debug, Eq, PartialEq)]
		struct HirAstMap {
			$(
				$arena: FxBiMap<FileAstId<$ast>, Idx<$hir>>,
			)*
		}

		#[derive(Default, Debug, Eq, PartialEq)]
		struct HirData {
			$(
				$arena: Arena<$hir>,
			)*
		}

		$(
			impl HasHir for Source<$ast> {
				type Hir = $hir;

				fn hir(&self, source_map: &SourceMap) -> Option<Idx<Self::Hir>> {
					source_map
						.hir_map
						.$arena
						.get_by_left(&source_map.ast_id_map.ast_id_ptr(&self.value))
						.copied()
				}

				fn file_id(&self) -> FileId {
					self.file_id
				}
			}

			impl HirNode for $hir {
				type Ast = $ast;

				fn source(id: Idx<Self>, source_map: &SourceMap) -> Option<Source<$ast>> {
					let ast_id = source_map.hir_map.$arena.get_by_right(&id)?;
					Some(InFile::new(
						source_map.file_id,
						source_map.ast_id_map.get(*ast_id),
					))
				}

				fn node(id: Idx<Self>, hir: &FileHir) -> &Self {
					&hir.data.$arena[id]
				}

                fn alloc(self, hir: &mut FileHir) -> Idx<Self> {
                    hir.data.$arena.alloc(self)
                }

                fn record_source(id: Idx<Self>, file_ast_id: FileAstId<$ast>, source_map: &mut SourceMap) {
                    source_map.hir_map.$arena.insert(
                        file_ast_id,
                        id,
                    );
                }
			}


            paste::paste! {
                pub type [<$hir Id>] = ::la_arena::Idx<$hir>;
            }

		)*
	};
    ($(
		$ast:ty => $hir: ty | $arena: ident
	),* $(,)?) => {
		hir_maps!(@inner $({ ast: $ast, hir: $hir, arena: $arena }),*);
	};
}

hir_maps! {
    ast::ModuleDir          => Module               | modules,
    ast::ExtendModuleDir    => ModuleExtension      | module_exts,
    ast::TermSymbol         => FunctionSymbol       | func_symbols,
    ast::DefineSortDir      => SortAlias            | sort_aliases,
    ast::LoadDir            => FileImport           | file_imports,
    ast::OpenDir            => ModuleImport         | module_imports,
    ast::Domain             => DomainDeclaration    | domains,
    ast::AssertDir          => Assertion            | assertions,
    ast::AssertClosedDir    => ClosedAssertion      | closed_assertions,
    ast::Dir                => Dir                  | dirs,
    ast::Pat                => Pat                  | pats,
    ast::Sort               => SortRef              | sort_refs,
    ast::DefineDir          => Definition           | definitions,
    ast::Expr               => Expr                 | exprs,
    ast::Ded                => Ded                  | deds,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ModuleStmt {
    Module(Idx<Module>),
    ModuleExtension(Idx<ModuleExtension>),
    FunctionSymbol(Idx<FunctionSymbol>),
    SortAlias(Idx<SortAlias>),
    FileImport(Idx<FileImport>),
    ModuleImport(Idx<ModuleImport>),
    DomainDeclaration(Idx<DomainDeclaration>),
    Assertion(Idx<Assertion>),
    ClosedAssertion(Idx<ClosedAssertion>),
    Dir(Idx<Dir>),
    Definition(Idx<Definition>),
}

macro_rules! impl_from_idx {
    ($($v: ident),+ for $t: ty) => {
        $(
            impl From<Idx<$v>> for $t {
                fn from(idx: Idx<$v>) -> Self {
                    Self::$v(idx)
                }
            }
        )+
    };
}

impl_from_idx! {
    Module,
    ModuleExtension,
    FunctionSymbol,
    SortAlias,
    FileImport,
    ModuleImport,
    DomainDeclaration,
    Assertion,
    ClosedAssertion,
    Dir,
    Definition for ModuleStmt
}
