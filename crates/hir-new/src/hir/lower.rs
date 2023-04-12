use std::sync::Arc;

use base_db::FileId;
use la_arena::Idx;
use syntax::{
    ast::{self, HasDefineBody, HasDefineName, HasName, HasNameRef},
    AstNode, AstToken,
};

use crate::{
    ast_map::AstIdMap,
    db::HirNewDatabase,
    hir::{
        AsName, DedId, DefKind, Definition, ExprId, FileImport, ModuleExtension, NameOrWildcard,
        Param, PatId, PhraseId, SortRef, SortRefId,
    },
};

use super::{
    FileHir, FileImportId, FunctionSymbol, HasHir, HirData, HirNode, Module, ModuleStmt, SourceMap,
};

pub(super) struct Ctx<'db> {
    db: &'db dyn HirNewDatabase,
    hir: FileHir,
    ast_id_map: Arc<AstIdMap>,
    source_map: SourceMap,
}

fn into<T, S>(val: T) -> S
where
    T: Into<S>,
{
    val.into()
}

pub type Id<T> = Idx<T>;

impl<'db> Ctx<'db> {
    fn data(&mut self) -> &mut HirData {
        &mut self.hir.data
    }
    pub(super) fn new(db: &'db dyn HirNewDatabase, file_id: FileId) -> Self {
        let ast_id_map = db.ast_id_map(file_id);
        Self {
            db,
            hir: FileHir::empty(file_id),
            source_map: SourceMap::empty(file_id, ast_id_map.clone()),
            ast_id_map,
        }
    }

    fn alloc<H: HirNode>(&mut self, ast: &<H as HirNode>::Ast, node: H) -> Id<H> {
        let id = node.alloc(&mut self.hir);
        let ast_id = self.ast_id_map.ast_id(ast);
        H::record_source(id, ast_id, &mut self.source_map);
        id
    }

    pub(super) fn lower_file(mut self, file: ast::SourceFile) -> (FileHir, SourceMap) {
        self.hir.top_level = file
            .stmts()
            .filter_map(|stmt| self.lower_module_stmt(&stmt))
            .collect();

        (self.hir, self.source_map)
    }

    fn lower_module_stmt(&mut self, stmt: &ast::Stmt) -> Option<ModuleStmt> {
        match stmt {
            ast::Stmt::Dir(ast::Dir::ModuleDir(module)) => self.lower_module_dir(module).map(into),
            ast::Stmt::Dir(ast::Dir::ExtendModuleDir(module_ext)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DeclareDir(func_declare)) => {
                let sym = func_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::ConstantDeclareDir(constant_declare)) => {
                let sym = constant_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::DefineDir(define)) => self.lower_define_dir(define).map(into),
            ast::Stmt::Dir(ast::Dir::DefineSortDir(alias_def)) => todo!(),
            ast::Stmt::Dir(ast::Dir::LoadDir(load)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DomainDir(domain)) => todo!(),
            ast::Stmt::Dir(ast::Dir::DomainsDir(domains)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssociativityDir(associativity)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssertDir(assert)) => todo!(),
            ast::Stmt::Dir(ast::Dir::AssertClosedDir(assert)) => todo!(),
            ast::Stmt::Dir(ast::Dir::OpenDir(open)) => todo!(),
            ast::Stmt::Dir(ast::Dir::OverloadDir(overload)) => todo!(),
            ast::Stmt::Dir(ast::Dir::RuleDir(rule)) => todo!(),
            ast::Stmt::Dir(ast::Dir::SetPrecedenceDir(set_prec)) => todo!(),
            ast::Stmt::PhraseStmt(_) => todo!(),
            ast::Stmt::DatatypeStmt(_) => todo!(),
            ast::Stmt::StructureStmt(_) => todo!(),
            ast::Stmt::DatatypesStmt(_) => todo!(),
            ast::Stmt::StructuresStmt(_) => todo!(),
        }
    }

    fn lower_load_dir(&mut self, load: &ast::LoadDir) -> Option<FileImportId> {
        let path = load.file_path()?.string_token()?.text();
        todo!()
    }

    fn lower_phrase(&mut self, phrase: &ast::Phrase) -> Option<PhraseId> {
        match phrase {
            ast::Phrase::Expr(expr) => self.lower_expr(expr).map(into),
            ast::Phrase::Ded(ded) => self.lower_ded(ded).map(into),
        }
    }

    fn lower_sort_ref(&mut self, sort_ref: &ast::Sort) -> Option<SortRefId> {
        Some(match sort_ref {
            ast::Sort::VarSort(v) => {
                let name = ast::Ident::cast(v.ident_token()?)?.as_name();
                self.alloc(sort_ref, SortRef::Var(name))
            }
            ast::Sort::IdentSort(name) => {
                self.alloc(sort_ref, SortRef::Ident(name.name_ref()?.as_name()))
            }
            ast::Sort::CompoundSort(compound) => {
                let mut sorts = compound.sorts();
                let first_ast = sorts.next()?;
                let first = self.lower_sort_ref(&first_ast)?;

                let rest = sorts
                    .filter_map(|s| self.lower_sort_ref(&s))
                    .collect::<Vec<_>>();

                self.alloc(sort_ref, SortRef::Apply(first, rest))
            }
        })
    }

    fn lower_param(&mut self, param: &ast::MaybeWildcardTypedParam) -> Option<Param> {
        let (name, sort) = match param {
            ast::MaybeWildcardTypedParam::MaybeTypedParam(_) => todo!(),
            ast::MaybeWildcardTypedParam::Wildcard(_) => (NameOrWildcard::Wildcard, None),
        };
        Some(Param { name, sort })
    }

    fn lower_define_dir(&mut self, define: &ast::DefineDir) -> Option<Id<Definition>> {
        let name = define.define_name()?;
        let kind = match name {
            ast::DefineName::Name(name) => DefKind::Value(name.as_name()),
            ast::DefineName::DefineNamedPattern(_) => todo!(),
            ast::DefineName::DefineProc(proc) => {
                let name = proc.name()?.as_name();
                let args = proc.args();
                let params = args
                    .filter_map(|arg| self.lower_param(&arg))
                    .collect::<Vec<_>>();
                DefKind::Procedure(name, params)
            }
            ast::DefineName::ListPat(pat) => {
                let pat: ast::Pat = pat.clone().into();
                let pat = self.lower_pat(&pat)?;
                DefKind::PatternValue(pat)
            }
        };
        let body = define.define_body()?;
        let body = self.lower_phrase(&body)?;

        Some(self.alloc(define, Definition { kind, body }))
    }

    fn lower_pat(&mut self, pat: &ast::Pat) -> Option<PatId> {
        todo!()
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Option<ExprId> {
        todo!()
    }

    fn lower_ded(&mut self, ded: &ast::Ded) -> Option<DedId> {
        todo!()
    }

    fn lower_extend_module_dir(
        &mut self,
        extend_module: &ast::ExtendModuleDir,
    ) -> Option<Id<ModuleExtension>> {
        let name = extend_module.name()?.as_name();
        let stmts = extend_module
            .stmts()
            .filter_map(|stmt| self.lower_module_stmt(&stmt))
            .collect();
        let extension = self.alloc(extend_module, ModuleExtension { name, stmts });
        Some(extension)
    }

    fn lower_module_dir(&mut self, module: &ast::ModuleDir) -> Option<Id<Module>> {
        let name = module.name()?.as_name();
        let stmts = module
            .stmts()
            .filter_map(|stmt| self.lower_module_stmt(&stmt))
            .collect();
        let module = self.alloc(module, Module { name, stmts });
        Some(module)
    }

    fn lower_term_symbol(&mut self, term_symbol: &ast::TermSymbol) -> Option<Id<FunctionSymbol>> {
        match term_symbol {
            ast::TermSymbol::Function(func) => match func {
                ast::DeclareDir::PrefixDeclareDir(_) => todo!(),
                ast::DeclareDir::InfixDeclareDir(declare) => {
                    let names = declare.names().map(|name| name.as_name()).collect();
                    let arg_sorts = declare
                        .func_sorts()?
                        .sorts()
                        .filter_map(|s| self.lower_sort_ref(&s))
                        .collect();
                    let ret_sort = self.lower_sort_ref(&declare.return_sort()?)?;
                    let func = self.alloc(
                        term_symbol,
                        FunctionSymbol {
                            names,
                            arg_sorts,
                            ret_sort,
                        },
                    );
                    Some(func)
                }
            },
            ast::TermSymbol::Constant(constant) => match constant {
                ast::ConstantDeclareDir::PrefixConstantDeclare(_) => todo!(),
                ast::ConstantDeclareDir::InfixConstantDeclare(declare) => {
                    let names = declare.names().map(|name| name.as_name()).collect();
                    let ret_sort = self.lower_sort_ref(&declare.sort()?)?;
                    let func = self.alloc(
                        term_symbol,
                        FunctionSymbol {
                            names,
                            arg_sorts: Vec::new(),
                            ret_sort,
                        },
                    );
                    Some(func)
                }
            },
        }
    }
}
