use std::sync::Arc;

use base_db::FileId;
use hir::pat::PatKind;
use la_arena::Idx;
use syntax::{
    ast::{self, HasDefineBody, HasDefineName, HasName, HasNameRef},
    AstToken,
};

use crate::{
    ast_map::AstIdMap,
    db::HirNewDatabase,
    hir::{
        self, expr::Literal, AsName, DedId, DefKind, Definition, ExprId, FileImport,
        ModuleExtension, NameOrWildcard, Param, PatId, PhraseId, SortRef, SortRefId,
    },
};

use super::{
    dir::Associativity,
    stmt::{
        Constructor, ConstructorParam, Datatype, Datatypes, SortDeclaration, Structure,
        StructureDef, Structures,
    },
    Expr, FileHir, FileImportId, FunctionSymbol, HirNode, Module, ModuleStmt, Pat, SortAlias,
    SourceMap,
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

fn lower_name_ref(id: &impl HasNameRef) -> Option<hir::Name> {
    let name = id.name_ref()?;
    Some(name.as_name())
}

fn lower_name(id: &impl HasName) -> Option<hir::Name> {
    let name = id.name()?;
    Some(name.as_name())
}

fn lower_assoc(assoc: &ast::AssociativityDir) -> Option<Associativity> {
    if let Some(_) = assoc.left_assoc_token() {
        Some(Associativity::Left)
    } else if let Some(_) = assoc.right_assoc_token() {
        Some(Associativity::Right)
    } else {
        None
    }
}

fn sort_like(sort: impl Into<ast::SortLike>) -> ast::SortLike {
    sort.into()
}

fn lower_some_thing(thing: &ast::SomeThing) -> Option<hir::pat::Thing> {
    macro_rules! map_some_thing {
        (match $ast: ident { $($name: ident => $thing: ident),* $(,)? }) => {
            $(
                paste::paste! {
                    if let Some(_) = $ast.[<some_ $name _token >]() {
                        return Some(hir::pat::Thing::$thing);
                    }
                }
            )*
        };
    }

    map_some_thing!(match thing {
        var => Var,
        sent_con => SentCon,
        quant => Quant,
        term => Term,
        atom => Atom,
        sentence => Sentence,
        list => List,
        cell => Cell,
        vector => Vector,
        proc => Proc,
        method => Method,
        symbol => Symbol,
        table => Table,
        map => Map,
        sub => Sub,
        char => Char,
    });

    None
}

impl<'db> Ctx<'db> {
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
            ast::Stmt::Dir(ast::Dir::ExtendModuleDir(module_ext)) => {
                self.lower_extend_module_dir(module_ext).map(into)
            }
            ast::Stmt::Dir(ast::Dir::DeclareDir(func_declare)) => {
                let sym = func_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::ConstantDeclareDir(constant_declare)) => {
                let sym = constant_declare.clone().into();
                self.lower_term_symbol(&sym).map(into)
            }
            ast::Stmt::Dir(ast::Dir::DefineDir(define)) => self.lower_define_dir(define).map(into),
            ast::Stmt::Dir(ast::Dir::DefineSortDir(alias_def)) => {
                let name = lower_name(alias_def)?;
                let sort = self.lower_sort_ref(&alias_def.sort()?)?;
                Some(self.alloc(alias_def, SortAlias { name, sort }).into())
            }
            ast::Stmt::Dir(ast::Dir::LoadDir(load)) => self.lower_load_dir(load).map(into),
            ast::Stmt::Dir(ast::Dir::DomainDir(domain)) => {
                let sort_decl = self.lower_sort_decl(&domain.sort_decl()?)?;
                let domain = ast::Domain::from(domain.clone());
                Some(
                    self.alloc(
                        &domain,
                        hir::DomainDeclaration {
                            domains: vec![sort_decl],
                        },
                    )
                    .into(),
                )
            }
            ast::Stmt::Dir(ast::Dir::DomainsDir(domains)) => {
                let domain_ast = ast::Domain::from(domains.clone());
                let domains = domains
                    .sort_decls()
                    .filter_map(|domain| self.lower_sort_decl(&domain))
                    .collect();
                Some(
                    self.alloc(&domain_ast, hir::DomainDeclaration { domains })
                        .into(),
                )
            }
            ast::Stmt::Dir(dir @ ast::Dir::AssociativityDir(associativity)) => {
                let name = lower_name_ref(associativity)?;
                let assoc = lower_assoc(associativity)?;
                Some(
                    self.alloc(
                        dir,
                        hir::Dir {
                            kind: hir::dir::DirKind::SetAssociativity(name, assoc),
                        },
                    )
                    .into(),
                )
            }
            ast::Stmt::Dir(dir @ ast::Dir::SetPrecedenceDir(set_prec)) => {
                let names = set_prec
                    .name_refs()
                    .filter_map(|n| lower_name_ref(&n))
                    .collect();
                let prec = self.lower_expr(&set_prec.expr()?)?;
                Some(
                    self.alloc(
                        dir,
                        hir::Dir {
                            kind: hir::dir::DirKind::SetPrecedence(names, prec),
                        },
                    )
                    .into(),
                )
            }
            ast::Stmt::Dir(ast::Dir::AssertDir(assert)) => {
                let name = assert.name().as_ref().map(lower_name).flatten();
                let facts = match assert {
                    ast::AssertDir::InfixAssertDir(infix) => {
                        infix.exprs().filter_map(|e| self.lower_expr(&e)).collect()
                    }
                    ast::AssertDir::PrefixAssertDir(prefix) => {
                        prefix.exprs().filter_map(|e| self.lower_expr(&e)).collect()
                    }
                };
                Some(self.alloc(assert, hir::Assertion { name, facts }).into())
            }
            ast::Stmt::Dir(ast::Dir::AssertClosedDir(assert)) => {
                let name = assert.name().as_ref().map(lower_name).flatten();
                let fact = self.lower_expr(&assert.expr()?)?;
                Some(
                    self.alloc(assert, hir::ClosedAssertion { name, fact })
                        .into(),
                )
            }
            ast::Stmt::Dir(ast::Dir::OpenDir(open)) => {
                let names = open
                    .name_refs()
                    .filter_map(|n| lower_name_ref(&n))
                    .collect();
                Some(self.alloc(open, hir::ModuleImport { names }).into())
            }
            ast::Stmt::Dir(ast::Dir::OverloadDir(overload)) => {
                let overloads = match overload {
                    ast::OverloadDir::OverloadSingle(overload) => {
                        let target = self.lower_phrase(&overload.first_phrase()?)?;
                        let overload = self.lower_phrase(&overload.second_phrase()?)?;
                        vec![hir::Overload { target, overload }]
                    }
                    ast::OverloadDir::OverloadMulti(overloads) => overloads
                        .phrase_pairs()
                        .filter_map(|pair| {
                            let target = self.lower_phrase(&pair.first_phrase()?)?;
                            let overload = self.lower_phrase(&pair.second_phrase()?)?;
                            Some(hir::Overload { target, overload })
                        })
                        .collect(),
                };

                Some(self.alloc(overload, hir::Overloads { overloads }).into())
            }
            ast::Stmt::Dir(ast::Dir::RuleDir(_rule)) => todo!(),
            ast::Stmt::PhraseStmt(p) => self.lower_phrase(&p.phrase()).map(into),
            ast::Stmt::DatatypeStmt(datatype) => {
                let def = self.lower_structure_def(&datatype.structure_def()?)?;
                Some(
                    self.alloc(
                        &ast::DatatypeOrDatatypes::from(datatype.clone()),
                        Datatypes {
                            datatypes: vec![Datatype::from(def)],
                        },
                    )
                    .into(),
                )
            }
            ast::Stmt::DatatypesStmt(datatypes_stmt) => {
                let datatypes = datatypes_stmt
                    .structure_defs()
                    .filter_map(|def| self.lower_structure_def(&def))
                    .map(Datatype::from)
                    .collect();
                Some(
                    self.alloc(
                        &ast::DatatypeOrDatatypes::from(datatypes_stmt.clone()),
                        Datatypes { datatypes },
                    )
                    .into(),
                )
            }
            ast::Stmt::StructureStmt(s) => {
                let def = self.lower_structure_def(&s.structure_def()?)?;
                Some(
                    self.alloc(
                        &ast::StructureOrStructures::from(s.clone()),
                        Structures {
                            structures: vec![Structure::from(def)],
                        },
                    )
                    .into(),
                )
            }
            ast::Stmt::StructuresStmt(s) => {
                let structures = s
                    .structure_defs()
                    .filter_map(|def| self.lower_structure_def(&def))
                    .map(Structure::from)
                    .collect();
                Some(
                    self.alloc(
                        &ast::StructureOrStructures::from(s.clone()),
                        Structures { structures },
                    )
                    .into(),
                )
            }
        }
    }

    fn lower_structure_def(&mut self, def: &ast::StructureDef) -> Option<StructureDef> {
        let name_def = def.structure_name_def()?;

        let SortDeclaration { name, sort_args } = self.lower_sort_decl(&name_def.sort_decl()?)?;
        let constructors = def
            .structure_constructors()
            .filter_map(|c| self.lower_constructor(&c))
            .collect();
        Some(StructureDef {
            name,
            sort_args,
            constructors,
        })
    }

    fn lower_constructor(
        &mut self,
        constructor: &ast::StructureConstructor,
    ) -> Option<Constructor> {
        let name = lower_name(&constructor.name()?)?;

        let params = match constructor {
            ast::StructureConstructor::ConstantConstructor(_) => {
                vec![]
            }
            ast::StructureConstructor::CompoundConstructor(c) => c
                .maybe_tagged_field_sorts()
                .filter_map(|s| self.lower_constructor_param(&s))
                .collect(),
        };

        Some(Constructor { name, params })
    }

    fn lower_constructor_param(
        &mut self,
        param: &ast::MaybeTaggedFieldSort,
    ) -> Option<ConstructorParam> {
        let tag = param.tag().map(|n| lower_name(&n)).flatten();
        let sort = self.lower_limited_sort(&param.limited_sort()?)?;

        Some(ConstructorParam { tag, sort })
    }

    fn lower_limited_sort(&mut self, sort_ref: &ast::LimitedSort) -> Option<SortRefId> {
        match sort_ref {
            ast::LimitedSort::IdentSort(ident_sort) => Some(self.alloc(
                &sort_like(sort_ref.clone()),
                SortRef::Ident(lower_name_ref(ident_sort)?),
            )),
            ast::LimitedSort::LimitedCompoundSort(compound) => {
                let name = compound.ident_sort()?;
                let ident_sort = ast::Sort::IdentSort(name);
                let ident_sort = self.lower_sort_ref(&ident_sort)?;
                let rest = compound
                    .limited_sorts()
                    .filter_map(|s| self.lower_limited_sort(&s))
                    .collect();
                Some(self.alloc(
                    &sort_like(sort_ref.clone()),
                    SortRef::Apply(ident_sort, rest),
                ))
            }
        }
    }

    fn lower_sort_decl(&mut self, sort_decl: &ast::SortDecl) -> Option<SortDeclaration> {
        match sort_decl {
            ast::SortDecl::IdentSortDecl(ident) => Some(SortDeclaration {
                name: ident.name()?.as_name(),
                sort_args: vec![],
            }),
            ast::SortDecl::CompoundSortDecl(compound) => {
                let mut parts = compound.ident_sort_decls();

                let name = parts.next()?.name()?.as_name();
                let sort_args = parts.filter_map(|part| lower_name(&part)).collect();

                Some(SortDeclaration { name, sort_args })
            }
        }
    }

    fn lower_load_dir(&mut self, load: &ast::LoadDir) -> Option<FileImportId> {
        let path = load.file_path()?.string_token()?;
        let path = path.text().trim_matches('"').to_owned();
        let Some(resolved) = self.db.resolve_file_path(self.hir.file_id, path.clone()) else {
            tracing::error!("failed to resolve file path: {}", path);
            return None;
        };
        let import = FileImport { file: resolved };
        Some(self.alloc(load, import))
    }

    fn lower_phrase(&mut self, phrase: &ast::Phrase) -> Option<PhraseId> {
        match phrase {
            ast::Phrase::Expr(expr) => self.lower_expr(expr).map(into),
            ast::Phrase::Ded(ded) => self.lower_ded(ded).map(into),
        }
    }

    fn lower_sort_ref(&mut self, sort_ref: &ast::Sort) -> Option<SortRefId> {
        let sort_like = sort_like(sort_ref.clone());
        Some(match sort_ref {
            ast::Sort::VarSort(v) => {
                let name = ast::Ident::cast(v.ident_token()?)?.as_name();
                self.alloc(&sort_like, SortRef::Var(name))
            }
            ast::Sort::IdentSort(name) => {
                self.alloc(&sort_like, SortRef::Ident(name.name_ref()?.as_name()))
            }
            ast::Sort::CompoundSort(compound) => {
                let mut sorts = compound.sorts();
                let first_ast = sorts.next()?;
                let first = self.lower_sort_ref(&first_ast)?;

                let rest = sorts
                    .filter_map(|s| self.lower_sort_ref(&s))
                    .collect::<Vec<_>>();

                self.alloc(&sort_like, SortRef::Apply(first, rest))
            }
        })
    }

    fn lower_param(&mut self, param: &ast::MaybeWildcardTypedParam) -> Option<Param> {
        let (name, sort, op_arity) = match param {
            ast::MaybeWildcardTypedParam::MaybeTypedParam(maybe_typed) => match maybe_typed {
                ast::MaybeTypedParam::Name(name) => {
                    (NameOrWildcard::Name(lower_name(name)?), None, None)
                }
                ast::MaybeTypedParam::TypedParam(typed) => {
                    let name = lower_name(&typed.name()?)?;
                    let sort = self.lower_sort_ref(&typed.sort()?)?;
                    (NameOrWildcard::Name(name), Some(sort), None)
                }
                ast::MaybeTypedParam::OpAnnotatedParam(op) => (
                    NameOrWildcard::Name(lower_name(&op.name()?)?),
                    None,
                    lower_name_ref(&op.name_ref()?),
                ),
            },
            ast::MaybeWildcardTypedParam::Wildcard(_) => (NameOrWildcard::Wildcard, None, None),
        };
        Some(Param {
            name,
            sort,
            op_arity,
        })
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
                let pat: ast::Pat = pat.into();
                let pat = self.lower_pat(&pat)?;
                DefKind::PatternValue(pat)
            }
        };
        let body = define.define_body()?;
        let body = self.lower_phrase(&body)?;

        Some(self.alloc(define, Definition { kind, body }))
    }

    fn lower_pat(&mut self, pat: &ast::Pat) -> Option<PatId> {
        let kind = match pat {
            ast::Pat::IdentPat(id) => {
                let p = id.maybe_wildcard_typed_param()?;
                match p {
                    ast::MaybeWildcardTypedParam::MaybeTypedParam(maybe_typed) => match maybe_typed
                    {
                        ast::MaybeTypedParam::Name(name) => {
                            let name = lower_name(&name)?;
                            PatKind::Ident {
                                name,
                                sort: None,
                                op_arity: None,
                            }
                        }
                        ast::MaybeTypedParam::TypedParam(typed) => {
                            let name = lower_name(&typed)?;
                            let sort = self.lower_sort_ref(&typed.sort()?)?;
                            PatKind::Ident {
                                name,
                                sort: Some(sort),
                                op_arity: None,
                            }
                        }
                        ast::MaybeTypedParam::OpAnnotatedParam(op) => {
                            let name = lower_name(&op.name()?)?;
                            PatKind::Ident {
                                name,
                                sort: None,
                                op_arity: lower_name_ref(&op.name_ref()?),
                            }
                        }
                    },
                    ast::MaybeWildcardTypedParam::Wildcard(_) => PatKind::Wildcard,
                }
            }
            ast::Pat::VarPat(var) => {
                let name = hir::Name::new_trustme(var.ident_token()?.text());
                let sort = var.sort().and_then(|s| self.lower_sort_ref(&s));
                PatKind::Var { name, sort }
            }
            ast::Pat::MetaIdentPat(meta) => {
                let name = hir::Name::new_trustme(meta.meta_ident()?.ident_token()?.text());
                PatKind::Symbol(name)
            }
            ast::Pat::LiteralPat(lit) => {
                let literal = lit.literal()?;
                let literal = self.lower_literal(&literal)?;
                PatKind::Literal(literal)
            }
            ast::Pat::UnitPat(_) => PatKind::Unit,
            ast::Pat::NamedPat(named) => {
                let name = lower_name(named)?;
                let pat = self.lower_pat(&named.pat()?)?;
                PatKind::Named { name, pat }
            }
            ast::Pat::ValOfPat(v) => PatKind::ValOf(lower_name_ref(v)?),
            ast::Pat::ListOfPat(l) => {
                let first = self.lower_pat(&l.first()?)?;
                let second = self.lower_pat(&l.second()?)?;
                PatKind::ListOf(first, second)
            }
            ast::Pat::SplitPat(split) => {
                let first = self.lower_pat(&split.first()?)?;
                let second = self.lower_pat(&split.second()?)?;
                let rest = split.rest().filter_map(|r| self.lower_pat(&r));
                PatKind::Split([first, second].into_iter().chain(rest).collect())
            }
            ast::Pat::ListPat(list) => {
                let pats = list.pats().filter_map(|p| self.lower_pat(&p)).collect();
                PatKind::List(pats)
            }
            ast::Pat::CompoundPat(compound) => {
                let pats = compound.pats().filter_map(|p| self.lower_pat(&p)).collect();
                PatKind::Compound(pats)
            }
            ast::Pat::WherePat(where_) => {
                let pat = self.lower_pat(&where_.pat()?)?;
                let cond = self.lower_expr(&where_.expr()?)?;
                PatKind::Where(pat, cond)
            }
            ast::Pat::SomeThingPat(some) => {
                let thing = lower_some_thing(&some.some_thing()?)?;
                let ident = some.ident_pat()?;
                let name = match ident.maybe_wildcard_typed_param()? {
                    ast::MaybeWildcardTypedParam::MaybeTypedParam(mtp) => match mtp {
                        ast::MaybeTypedParam::Name(n) => lower_name(&n)?.into(),
                        ast::MaybeTypedParam::TypedParam(tp) => lower_name(&tp)?.into(),
                        ast::MaybeTypedParam::OpAnnotatedParam(op) => lower_name(&op)?.into(),
                    },
                    ast::MaybeWildcardTypedParam::Wildcard(_) => NameOrWildcard::Wildcard,
                };
                PatKind::Some(thing, name)
            }
        };

        Some(self.alloc(pat, Pat { kind }))
    }

    fn lower_literal(&mut self, lit: &ast::Literal) -> Option<Literal> {
        if let Some(string) = lit.string_token() {
            Some(Literal::String(String::from(
                // remove the quotes
                &string.text()[1..string.text().len() - 1],
            )))
        } else if let Some(char) = lit.char_token() {
            let mut chars = char.text().chars();
            assert_eq!('`', chars.next()?);
            Some(Literal::Char(chars.next()?))
        } else {
            None
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Option<ExprId> {
        match expr {
            ast::Expr::IdentExpr(ident) => {
                Some(self.alloc(expr, Expr::Ident(lower_name_ref(ident)?)))
            }
            ast::Expr::UnitExpr(_) => Some(self.alloc(expr, Expr::Unit)),
            ast::Expr::LiteralExpr(lit) => {
                let lit = self.lower_literal(&lit.literal()?)?;
                Some(self.alloc(expr, Expr::Literal(lit)))
            }
            ast::Expr::TermVarExpr(var) => {
                let sort = var.sort().map(|s| self.lower_sort_ref(&s)).flatten();
                Some(self.alloc(
                    expr,
                    Expr::TermVar(hir::Name::new_trustme(var.ident_token()?.text()), sort),
                ))
            }
            ast::Expr::MetaIdent(meta) => Some(self.alloc(
                expr,
                Expr::Literal(Literal::Symbol(hir::Name::new_trustme(
                    meta.ident_token()?.text(),
                ))),
            )),
            ast::Expr::CheckExpr(_) => todo!(),
            ast::Expr::LambdaExpr(_) => todo!(),
            ast::Expr::ApplicationExpr(_) => todo!(),
            ast::Expr::ListExpr(_) => todo!(),
            ast::Expr::MethodExpr(_) => todo!(),
            ast::Expr::LetExpr(_) => todo!(),
            ast::Expr::LetRecExpr(_) => todo!(),
            ast::Expr::MatchExpr(_) => todo!(),
            ast::Expr::TryExpr(_) => todo!(),
            ast::Expr::CellExpr(_) => todo!(),
            ast::Expr::SetExpr(_) => todo!(),
            ast::Expr::RefExpr(_) => todo!(),
            ast::Expr::WhileExpr(_) => todo!(),
            ast::Expr::MakeVectorExpr(_) => todo!(),
            ast::Expr::VectorSubExpr(_) => todo!(),
            ast::Expr::VectorSetExpr(_) => todo!(),
            ast::Expr::SeqExpr(_) => todo!(),
            ast::Expr::AndExpr(_) => todo!(),
            ast::Expr::OrExpr(_) => todo!(),
            ast::Expr::MapExpr(_) => todo!(),
            ast::Expr::WildcardExpr(_) => todo!(),
            ast::Expr::PrefixCheckExpr(_) => todo!(),
        }
    }

    fn lower_ded(&mut self, _ded: &ast::Ded) -> Option<DedId> {
        match _ded {
            ast::Ded::MethodCallDed(_) => todo!(),
            ast::Ded::BangMethodCallDed(_) => todo!(),
            ast::Ded::AssumeDed(_) => todo!(),
            ast::Ded::NamedAssumeDed(_) => todo!(),
            ast::Ded::ProofByContraDed(_) => todo!(),
            ast::Ded::GeneralizeOverDed(_) => todo!(),
            ast::Ded::PickAnyDed(_) => todo!(),
            ast::Ded::WithWitnessDed(_) => todo!(),
            ast::Ded::PickWitnessDed(_) => todo!(),
            ast::Ded::PickWitnessesDed(_) => todo!(),
            ast::Ded::InductDed(_) => todo!(),
            ast::Ded::CasesDed(_) => todo!(),
            ast::Ded::CheckDed(_) => todo!(),
            ast::Ded::MatchDed(_) => todo!(),
            ast::Ded::LetDed(_) => todo!(),
            ast::Ded::LetRecDed(_) => todo!(),
            ast::Ded::TryDed(_) => todo!(),
            ast::Ded::ConcludeDed(_) => todo!(),
            ast::Ded::InferBlockDed(_) => todo!(),
            ast::Ded::PrefixAssumeDed(_) => todo!(),
            ast::Ded::SeqDed(_) => todo!(),
        }
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
                    let sort_vars = declare
                        .sort_vars_decl()
                        .map(|decl| {
                            decl.ident_sort_decls()
                                .filter_map(|i| i.name().map(|n| n.as_name()))
                                .collect()
                        })
                        .unwrap_or_default();
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
                            sort_vars,
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
                            sort_vars: Vec::new(),
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
