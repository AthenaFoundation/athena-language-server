use core::fmt;
use std::sync::Arc;

use base_db::{salsa, FileId, SourceDatabase};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::{ast, AstNode, AstPtr, SyntaxNode, SyntaxNodePtr};

use crate::InFile;

#[salsa::query_group(AstDatabaseStorage)]
pub trait AstDatabase: SourceDatabase {
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;
}

pub struct FileAstId<N: AstNode> {
    #[allow(dead_code)]
    raw: ErasedAstId,
    _type: std::marker::PhantomData<fn() -> N>,
}

impl<N: AstNode> Clone for FileAstId<N> {
    fn clone(&self) -> FileAstId<N> {
        *self
    }
}
impl<N: AstNode> Copy for FileAstId<N> {}

impl<N: AstNode> PartialEq for FileAstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N: AstNode> Eq for FileAstId<N> {}
impl<N: AstNode> std::hash::Hash for FileAstId<N> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.raw.hash(hasher);
    }
}

impl<N: AstNode> fmt::Debug for FileAstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FileAstId::<{}>({})",
            std::any::type_name::<N>(),
            self.raw.into_raw()
        )
    }
}

type ErasedAstId = Idx<SyntaxNodePtr>;

#[allow(dead_code)]
pub type AstId<N> = InFile<FileAstId<N>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    ptr_to_id: FxHashMap<SyntaxNodePtr, ErasedAstId>,
}

impl AstIdMap {
    pub(crate) fn from_source(node: &SyntaxNode) -> Self {
        let mut arena = Arena::default();

        bdfs(node, |node| {
            let kind = node.kind();
            if ast::Dir::can_cast(kind) || ast::Stmt::can_cast(kind) || ast::Sort::can_cast(kind) {
                arena.alloc(SyntaxNodePtr::new(&node));
                true
            } else if ast::Phrase::can_cast(kind) {
                arena.alloc(SyntaxNodePtr::new(&node));
                true
            } else if ast::Pat::can_cast(kind) {
                arena.alloc(SyntaxNodePtr::new(&node));
                true
            } else {
                false
            }
        });

        let ptr_to_id = arena
            .iter()
            .map(|(erased, ptr)| (ptr.clone(), erased))
            .collect();

        AstIdMap { arena, ptr_to_id }
    }

    pub fn ast_id<N: AstNode>(&self, node: &N) -> FileAstId<N> {
        let raw = self.erased_ast_id(node.syntax());
        FileAstId {
            raw,
            _type: std::marker::PhantomData,
        }
    }

    pub fn ast_id_ptr<N: AstNode>(&self, node: &AstPtr<N>) -> FileAstId<N> {
        let ptr = node.syntax_node_ptr();
        let raw = self.ptr_to_id.get(&ptr).copied().unwrap();
        FileAstId {
            raw,
            _type: std::marker::PhantomData,
        }
    }

    pub fn get<N: AstNode>(&self, id: FileAstId<N>) -> AstPtr<N> {
        AstPtr::try_from_raw(self.arena[id.raw].clone()).unwrap()
    }

    fn erased_ast_id(&self, node: &SyntaxNode) -> ErasedAstId {
        let ptr = SyntaxNodePtr::new(node);
        self.ptr_to_id.get(&ptr).copied().unwrap()
    }
}

fn bdfs(node: &SyntaxNode, mut f: impl FnMut(SyntaxNode) -> bool) {
    let mut current = vec![node.clone()];
    let mut next = Vec::new();

    while !current.is_empty() {
        for node in current.drain(..) {
            let mut preorder = node.preorder();

            while let Some(event) = preorder.next() {
                match event {
                    syntax::WalkEvent::Enter(node) => {
                        if f(node.clone()) {
                            next.extend(node.children());
                            preorder.skip_subtree();
                        }
                    }
                    syntax::WalkEvent::Leave(_) => {}
                }
            }
        }
        current.extend(next.drain(..));
    }
}

fn ast_id_map(db: &dyn AstDatabase, file_id: FileId) -> Arc<AstIdMap> {
    // let _p = profile::span("ast_id_map");
    let source_file = db.parse(file_id);
    Arc::new(AstIdMap::from_source(source_file.tree().syntax()))
}
