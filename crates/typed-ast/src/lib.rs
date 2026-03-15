mod block;
mod expr;
mod stmt;
mod ty;

use std::{
    fs, mem,
    ops::Index,
    path::{Path, PathBuf},
};

use derive_where::derive_where;
use indexmap::IndexMap;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec};
use mollie_shared::Span;
use mollie_typing::{
    Adt, AdtKind, AdtRef, AdtVariantRef, Arg, FieldRef, FuncRef, ModuleId, PrimitiveType, SpecialAdtKind, Type, TypeContext, TypeError, TypeErrorValue,
    TypeFrameRef, TypeInfo, TypeInfoRef, TypeRef, TypeSolver, TypeStorage, VFuncRef, VTableRef,
};

pub use crate::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef, IsPattern, LitExpr},
    stmt::{Stmt, StmtRef},
};

pub enum FunctionBody<E> {
    Local { ast: TypedAST, entry: BlockRef },
    Import(&'static str),
    External(E),
}

pub trait ModuleLoader<E>: Sized {
    fn load_module(ast: TypedASTContextRef<'_, E, Self>, module: ModuleId);
}

impl<E> ModuleLoader<E> for () {
    fn load_module(_: TypedASTContextRef<'_, E, Self>, _: ModuleId) {
        panic!("load_module: noop")
    }
}

pub struct FileModuleLoader {
    pub current_dir: PathBuf,
}

impl<E> ModuleLoader<E> for FileModuleLoader {
    fn load_module(mut ast: TypedASTContextRef<'_, E, Self>, module: ModuleId) {
        fn module_path(base: &Path, type_context: &TypeContext, module: ModuleId) -> PathBuf {
            if module == ModuleId::ZERO {
                base.to_path_buf()
            } else {
                let module = &type_context.modules[module];
                let base = module
                    .parent
                    .map_or_else(|| base.to_path_buf(), |parent| module_path(base, type_context, parent));

                base.join(&module.name)
            }
        }

        let path = module_path(&ast.state.current_dir, ast.solver.context, module).with_extension("mol");

        println!("load_module: {}", path.display());

        let source = fs::read_to_string(path).unwrap();
        let void = ast.solver.context.core_types.void;

        ast.process(module, source, void);
    }
}

pub struct TypedASTContext<E, M: ModuleLoader<E>> {
    pub vtables: IndexVec<VTableRef, IndexVec<VFuncRef, FunctionBody<E>>>,
    pub functions: IndexVec<FuncRef, FunctionBody<E>>,
    pub type_context: TypeContext,
    pub state: M,
}

impl<E, M: ModuleLoader<E>> TypedASTContext<E, M> {
    pub const fn new(type_context: TypeContext, state: M) -> Self {
        Self {
            vtables: IndexVec::new(),
            functions: IndexVec::new(),
            type_context,
            state,
        }
    }

    pub fn take_ref(&mut self) -> TypedASTContextRef<'_, E, M> {
        TypedASTContextRef::new(&mut self.vtables, &mut self.functions, self.type_context.solver(), &mut self.state)
    }
}

pub struct TypedASTContextRef<'a, E, M: ModuleLoader<E>> {
    pub vtables: &'a mut IndexVec<VTableRef, IndexVec<VFuncRef, FunctionBody<E>>>,
    pub functions: &'a mut IndexVec<FuncRef, FunctionBody<E>>,
    pub solver: TypeSolver<'a>,
    pub state: &'a mut M,
    pub captures: Vec<(String, TypeInfoRef)>,
    pub current_frame: Option<TypeFrameRef>,
}

impl<'a, E, M: ModuleLoader<E>> TypedASTContextRef<'a, E, M> {
    pub const fn new(
        vtables: &'a mut IndexVec<VTableRef, IndexVec<VFuncRef, FunctionBody<E>>>,
        functions: &'a mut IndexVec<FuncRef, FunctionBody<E>>,
        solver: TypeSolver<'a>,
        state: &'a mut M,
    ) -> Self {
        Self {
            vtables,
            functions,
            solver,
            state,
            captures: Vec::new(),
            current_frame: None,
        }
    }

    pub fn process<T: AsRef<str>>(&mut self, module: ModuleId, source: T, returns: TypeRef) -> (TypedAST, BlockRef) {
        let (stmts, final_stmt) =
            mollie_parser::parse_statements_until(&mut mollie_parser::Parser::new(mollie_lexer::Lexer::lex(source)), &mollie_lexer::Token::EOF).unwrap();

        let mut ast = TypedAST { module, ..TypedAST::default() };
        let mut block_stmts = Vec::new();

        for stmt in stmts {
            if let Some(stmt) = Stmt::from_parsed(stmt.value, &mut ast, self, stmt.span) {
                block_stmts.push(stmt);
            }
        }

        let (expr, ty) = match final_stmt {
            Some(mollie_shared::Positioned {
                value: mollie_parser::Stmt::Expression(expr),
                span,
            }) => {
                let expr = Expr::from_parsed(expr, &mut ast, self, span);

                (Some(expr), ast[expr].ty)
            }
            _ => (None, self.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None)),
        };

        let result = ast.add_block(
            Block {
                stmts: block_stmts.into_boxed_slice(),
                expr,
            },
            ty,
            Span::default(),
        );

        ast.solve(result, self, returns)
    }

    pub fn fork(&mut self) -> TypedASTContextRef<'_, E, M> {
        TypedASTContextRef {
            vtables: self.vtables,
            functions: self.functions,
            solver: self.solver.fork(),
            state: self.state,
            captures: Vec::new(),
            current_frame: None,
        }
    }
}

pub trait FromParsed<E, M: ModuleLoader<E>, T, O = Self> {
    fn from_parsed(value: T, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, span: Span) -> O;
}

pub trait Descriptor {
    type Type;
    type IndexResult;
}

pub struct FirstPass;

impl Descriptor for FirstPass {
    type IndexResult = String;
    type Type = TypeInfoRef;
}

pub struct SolvedPass;

impl Descriptor for SolvedPass {
    type IndexResult = FieldRef;
    type Type = TypeRef;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UsedItem {
    VTable(TypeRef, VTableRef, Box<[TypeRef]>),
    Adt(AdtRef, Box<[TypeRef]>),
    Func(FuncRef),
}

#[derive_where(Debug, Clone; T, D::Type)]
pub struct Typed<T, D: Descriptor> {
    pub value: T,
    pub span: Span,
    pub ty: D::Type,
}

#[derive_where(Debug, Clone; D::Type, D::IndexResult)]
#[derive_where(Default)]
pub struct TypedAST<D: Descriptor = SolvedPass> {
    pub module: ModuleId,
    pub blocks: IndexVec<BlockRef, Typed<Block, D>>,
    pub statements: IndexVec<StmtRef, Stmt>,
    pub exprs: IndexVec<ExprRef, Typed<Expr<D>, D>>,
    pub used_items: Vec<UsedItem>,
}

impl<D: Descriptor> TypedAST<D> {
    pub fn add_block(&mut self, block: Block, ty: D::Type, span: Span) -> BlockRef {
        let result = BlockRef::new(self.blocks.len());

        self.blocks.push(Typed { value: block, span, ty });

        result
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtRef {
        let result = StmtRef::new(self.statements.len());

        self.statements.push(stmt);

        result
    }

    pub fn add_expr(&mut self, expr: Expr<D>, ty: D::Type, span: Span) -> ExprRef {
        let result = ExprRef::new(self.exprs.len());

        self.exprs.push(Typed { value: expr, span, ty });

        result
    }

    fn use_item<E>(
        &mut self,
        adt_types: &IndexVec<AdtRef, Adt>,
        vtables: &IndexVec<VTableRef, IndexVec<VFuncRef, FunctionBody<E>>>,
        functions: &IndexVec<FuncRef, FunctionBody<E>>,
        types: &mut TypeStorage,
        item: UsedItem,
        current_vtable: Option<VTableRef>,
    ) {
        if !self.used_items.contains(&item) {
            match &item {
                UsedItem::VTable(_, vtable, vtable_type_args) => {
                    for func in vtables[*vtable].values() {
                        if let FunctionBody::Local { ast, .. } = func {
                            for item in &ast.used_items {
                                let item = match item {
                                    UsedItem::VTable(ty, vtable, type_args) => {
                                        if current_vtable == Some(*vtable) {
                                            continue;
                                        }

                                        UsedItem::VTable(*ty, *vtable, type_args.iter().map(|ty| types.apply_type_args(*ty, vtable_type_args)).collect())
                                    }
                                    UsedItem::Adt(adt_ref, type_args) => {
                                        UsedItem::Adt(*adt_ref, type_args.iter().map(|ty| types.apply_type_args(*ty, vtable_type_args)).collect())
                                    }
                                    &UsedItem::Func(func) => UsedItem::Func(func),
                                };

                                self.use_item(adt_types, vtables, functions, types, item, current_vtable);
                            }
                        }
                    }
                }
                &UsedItem::Func(func) => {
                    if let FunctionBody::Local { ast, .. } = &functions[func] {
                        for item in &ast.used_items {
                            let item = match item {
                                UsedItem::VTable(ty, vtable, type_args) => UsedItem::VTable(*ty, *vtable, type_args.clone()),
                                UsedItem::Adt(adt_ref, type_args) => UsedItem::Adt(*adt_ref, type_args.clone()),
                                &UsedItem::Func(func) => UsedItem::Func(func),
                            };

                            self.use_item(adt_types, vtables, functions, types, item, current_vtable);
                        }
                    }
                }
                UsedItem::Adt(adt, type_args) => {
                    for variant in adt_types[*adt].variants.values() {
                        for field in variant.fields.values() {
                            #[allow(clippy::too_many_arguments)]
                            fn recurse_on_field<D: Descriptor, E>(
                                ty: TypeRef,
                                ast: &mut TypedAST<D>,
                                adt_types: &IndexVec<AdtRef, Adt>,
                                vtables: &IndexVec<VTableRef, IndexVec<VFuncRef, FunctionBody<E>>>,
                                functions: &IndexVec<FuncRef, FunctionBody<E>>,
                                types: &mut TypeStorage,
                                current_vtable: Option<VTableRef>,
                                type_args: &[TypeRef],
                            ) {
                                match &types[ty] {
                                    Type::Adt(adt, field_type_args) => {
                                        let adt = *adt;
                                        let field_type_args = field_type_args.clone();
                                        let type_args: Box<[_]> = field_type_args.into_iter().map(|ty| types.apply_type_args(ty, type_args)).collect();

                                        for &arg in &type_args {
                                            recurse_on_field(arg, ast, adt_types, vtables, functions, types, current_vtable, type_args.as_ref());
                                        }

                                        ast.use_item(adt_types, vtables, functions, types, UsedItem::Adt(adt, type_args), current_vtable);
                                    }
                                    &Type::Array(element, _) => {
                                        recurse_on_field(element, ast, adt_types, vtables, functions, types, current_vtable, type_args);
                                    }
                                    Type::Func(args, returns) => {
                                        let args = args.clone();
                                        let returns = *returns;

                                        for arg in args {
                                            recurse_on_field(arg, ast, adt_types, vtables, functions, types, current_vtable, type_args);
                                        }

                                        recurse_on_field(returns, ast, adt_types, vtables, functions, types, current_vtable, type_args);
                                    }
                                    _ => (),
                                }
                            }

                            recurse_on_field(field.ty, self, adt_types, vtables, functions, types, current_vtable, type_args);
                        }
                    }
                }
            }

            self.used_items.push(item);
        }
    }
}

impl<E, M: ModuleLoader<E>> TypedASTContextRef<'_, E, M> {
    fn use_vtable_impl(&mut self, target: TypeRef, vtable: VTableRef) -> Box<[TypeInfoRef]> {
        let type_args: Box<[_]> = self.solver.context.vtables[vtable]
            .generics
            .iter()
            .copied()
            .map(|ty| TypeSolver::type_to_info(&mut self.solver.type_infos, self.solver.context, ty, &[]))
            .collect();

        let solved_origin_ty = TypeSolver::type_to_info(&mut self.solver.type_infos, self.solver.context, target, &[]);
        let origin_ty = TypeSolver::type_to_info(
            &mut self.solver.type_infos,
            self.solver.context,
            self.solver.context.vtables[vtable].ty,
            &type_args,
        );

        if self.solver.context.vtables[vtable].origin_trait.is_some() {
            self.solver.unify(type_args[0], origin_ty);
        }

        self.solver.unify(origin_ty, solved_origin_ty);

        type_args
    }
}

impl TypedAST<FirstPass> {
    fn solve_expr<E, M: ModuleLoader<E>>(&self, ast: &mut TypedAST, expr: ExprRef, context: &mut TypedASTContextRef<'_, E, M>) -> ExprRef {
        let value = match self[expr].value.clone() {
            Expr::Lit(lit_expr) => Expr::Lit(lit_expr),
            Expr::Var(var) => Expr::Var(var),
            Expr::Binary { operator, lhs, rhs } => {
                let lhs = self.solve_expr(ast, lhs, context);
                let rhs = self.solve_expr(ast, rhs, context);

                if !context.solver.context.is_same(ast[lhs].ty, ast[rhs].ty) {
                    context.solver.context.error(
                        TypeError::Unexpected {
                            expected: TypeErrorValue::ExplicitType(ast[lhs].ty),
                            found: TypeErrorValue::ExplicitType(ast[rhs].ty),
                        },
                        ast[rhs].span,
                    );
                }

                Expr::Binary { operator, lhs, rhs }
            }
            Expr::Construct { adt, variant, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|(field_ref, field_type, field_value)| {
                        let expr = if field_value == ExprRef::INVALID {
                            field_value
                        } else {
                            self.solve_expr(ast, field_value, context)
                        };

                        let field_type = context.solver.get_info(field_type);
                        let field_type = context.solver.solve(field_type);
                        let expected_field_ty = context.solver.context.adt_types[adt].variants[variant].fields[field_ref].ty;

                        if expr != ExprRef::INVALID && !context.solver.context.is_same(ast[expr].ty, field_type) {
                            context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::ExplicitType(field_type),
                                    found: TypeErrorValue::ExplicitType(ast[expr].ty),
                                },
                                ast[expr].span,
                            );
                        }

                        match context.solver.context.types[expected_field_ty] {
                            Type::Trait(trait_ref, _) => {
                                if let Some(vtable) = context.solver.context.find_vtable(ast[expr].ty, Some(trait_ref)) {
                                    let type_args = context.use_vtable_impl(ast[expr].ty, vtable);
                                    let item = UsedItem::VTable(ast[expr].ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                                    ast.use_item(
                                        &context.solver.context.adt_types,
                                        context.vtables,
                                        context.functions,
                                        &mut context.solver.context.types,
                                        item,
                                        Some(vtable),
                                    );
                                }
                            }
                            Type::Array(element, _) => {
                                if let Type::Trait(trait_ref, _) = context.solver.context.types[element]
                                    && let Expr::Array { elements, .. } = &ast[expr].value
                                {
                                    let elements = elements.clone();

                                    for element in elements {
                                        if let Some(vtable) = context.solver.context.find_vtable(ast[element].ty, Some(trait_ref)) {
                                            let type_args = context.use_vtable_impl(ast[element].ty, vtable);
                                            let item =
                                                UsedItem::VTable(ast[element].ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                                            ast.use_item(
                                                &context.solver.context.adt_types,
                                                context.vtables,
                                                context.functions,
                                                &mut context.solver.context.types,
                                                item,
                                                Some(vtable),
                                            );
                                        }
                                    }
                                }
                            }
                            _ => (),
                        }

                        (field_ref, field_type, expr)
                    })
                    .collect();

                Expr::Construct { adt, variant, fields }
            }
            Expr::AdtIndex { target, field } => {
                let target = self.solve_expr(ast, target, context);

                if let Some((vtable, func)) = context.solver.context.find_vtable_by_func(ast[target].ty, &field) {
                    let ty = context.solver.context.vtables[vtable].functions[func].ty;
                    let type_args: Box<[_]> = context.solver.context.vtables[vtable]
                        .generics
                        .iter()
                        .copied()
                        .map(|ty| TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]))
                        .collect();

                    let solved_origin_ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ast[target].ty, &[]);
                    let origin_ty = TypeSolver::type_to_info(
                        &mut context.solver.type_infos,
                        context.solver.context,
                        context.solver.context.vtables[vtable].ty,
                        &type_args,
                    );

                    if context.solver.context.vtables[vtable].origin_trait.is_some() {
                        context.solver.unify(type_args[0], origin_ty);
                    }

                    context.solver.unify(origin_ty, solved_origin_ty);

                    let info = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &type_args);

                    context.solver.unify(self[expr].ty, info);

                    let type_args: Box<_> = type_args.into_iter().map(|ty| context.solver.solve(ty)).collect();

                    let ty = context.solver.context.types.apply_type_args(ty, &type_args);

                    ast.use_item(
                        &context.solver.context.adt_types,
                        context.vtables,
                        context.functions,
                        &mut context.solver.context.types,
                        UsedItem::VTable(ast[target].ty, vtable, type_args),
                        Some(vtable),
                    );

                    return ast.add_expr(
                        Expr::VTableIndex {
                            target: Some(target),
                            target_ty: ast[target].ty,
                            vtable,
                            func,
                        },
                        ty,
                        self[expr].span,
                    );
                } else if let Type::Trait(trait_ref, _) = context.solver.context.types[ast[target].ty]
                    && let Some((func, func_info)) = context.solver.context.traits[trait_ref].functions.iter().find(|(_, func)| func.name == field)
                {
                    let args = func_info
                        .args
                        .iter()
                        .map(|arg| context.solver.context.types.apply_type_args(arg.ty, &[ast[target].ty]))
                        .collect();

                    let returns = context.solver.context.types.apply_type_args(func_info.returns, &[ast[target].ty]);
                    let ty = context.solver.context.types.get_or_add(Type::Func(args, returns));

                    return ast.add_expr(Expr::TraitFunc { target, trait_ref, func }, ty, self[expr].span);
                }

                if let Type::Adt(adt_ref, args) = &context.solver.context.types[ast[target].ty] {
                    let adt = &context.solver.context.adt_types[*adt_ref];

                    if matches!(adt.kind, AdtKind::Enum) {
                        let ty = context.solver.context.types.get_or_add(Type::Error);

                        return ast.add_expr(
                            Expr::Error(context.solver.context.error(
                                TypeError::NonIndexable {
                                    ty: ast[target].ty,
                                    name: field,
                                },
                                self[expr].span,
                            )),
                            ty,
                            self[expr].span,
                        );
                    }

                    let Some(field) = adt.variants[AdtVariantRef::ZERO]
                        .fields
                        .iter()
                        .find_map(|(field_ref, variant_field)| if variant_field.name == field { Some(field_ref) } else { None })
                    else {
                        let adt = *adt_ref;
                        let ty = context.solver.context.types.get_or_add(Type::Error);

                        return ast.add_expr(
                            Expr::Error(context.solver.context.error(
                                TypeError::NoField {
                                    adt,
                                    variant: AdtVariantRef::ZERO,
                                    name: field,
                                },
                                self[expr].span,
                            )),
                            ty,
                            self[expr].span,
                        );
                    };

                    let ty = adt[field].ty;
                    let args = args.clone();
                    let ty = context.solver.context.types.apply_type_args(ty, &args);

                    let info = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]);

                    context.solver.unify(self[expr].ty, info);

                    return ast.add_expr(Expr::AdtIndex { target, field }, ty, self[expr].span);
                }

                let ty = context.solver.context.types.get_or_add(Type::Error);

                return ast.add_expr(
                    Expr::Error(context.solver.context.error(
                        TypeError::Unexpected {
                            expected: TypeErrorValue::Adt(SpecialAdtKind::WithExpectation(AdtKind::Enum)),
                            found: TypeErrorValue::ExplicitType(ast[target].ty),
                        },
                        ast[target].span,
                    )),
                    ty,
                    self[expr].span,
                );
            }
            Expr::TraitFunc { .. } => unreachable!(),
            Expr::ArrayIndex { target, element } => {
                let target = self.solve_expr(ast, target, context);
                let Type::Array(element_ty, _) = context.solver.context.types[ast[target].ty] else {
                    let ty = context.solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Array(None),
                                found: TypeErrorValue::ExplicitType(ast[target].ty),
                            },
                            ast[target].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                };

                let element = self.solve_expr(ast, element, context);
                let info = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, element_ty, &[]);

                context.solver.unify(self[expr].ty, info);

                return ast.add_expr(Expr::ArrayIndex { target, element }, element_ty, self[expr].span);
            }
            Expr::Closure { args, captures, body } => {
                let captures = captures.into_iter().map(|(name, ty)| (name, context.solver.solve(ty))).collect();
                let args = args
                    .into_iter()
                    .map(|arg| Arg {
                        name: arg.name,
                        kind: arg.kind,
                        ty: context.solver.solve(arg.ty),
                    })
                    .collect();

                let body = self.solve_block(ast, body, context);

                Expr::Closure { args, captures, body }
            }
            Expr::Call { func, args } => {
                let func = self.solve_expr(ast, func, context);
                let args: Box<[_]> = args.into_iter().map(|arg| self.solve_expr(ast, arg, context)).collect();

                if let Type::Func(arg_types, returns) = &context.solver.context.types[ast[func].ty] {
                    let arg_types = arg_types.clone();
                    let returns = *returns;
                    let skip = usize::from(matches!(ast[func].value, Expr::VTableIndex { target: Some(_), .. } | Expr::TraitFunc { .. }));
                    let expected = arg_types.len() - skip;
                    let found = args.len();

                    if found != expected {
                        context
                            .solver
                            .context
                            .error(TypeError::ArgumentCountMismatch { expected, found }, self[expr].span);
                    }

                    for (arg, arg_type) in args.iter().copied().zip(arg_types.into_iter().skip(skip)) {
                        if !context.solver.context.types.is_same(ast[arg].ty, arg_type) {
                            context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::ExplicitType(arg_type),
                                    found: TypeErrorValue::ExplicitType(ast[arg].ty),
                                },
                                ast[arg].span,
                            );
                        }
                    }

                    let ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, returns, &[]);

                    context.solver.unify(self[expr].ty, ty);

                    return ast.add_expr(Expr::Call { func, args }, returns, self[expr].span);
                }

                let ty = context.solver.context.types.get_or_add(Type::Error);

                return ast.add_expr(
                    Expr::Error(context.solver.context.error(
                        TypeError::Unexpected {
                            expected: TypeErrorValue::Function,
                            found: TypeErrorValue::ExplicitType(ast[func].ty),
                        },
                        ast[func].span,
                    )),
                    ty,
                    self[expr].span,
                );
            }
            Expr::Array { element, elements } => {
                let element = context.solver.solve(element);
                let elements = elements.into_iter().map(|element| self.solve_expr(ast, element, context)).collect();

                Expr::Array { element, elements }
            }
            Expr::IfElse { condition, block, otherwise } => {
                let condition = self.solve_expr(ast, condition, context);

                if context.solver.context.types[ast[condition].ty] != Type::Primitive(PrimitiveType::Bool) {
                    let ty = context.solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::PrimitiveType(PrimitiveType::Bool),
                                found: TypeErrorValue::ExplicitType(ast[condition].ty),
                            },
                            ast[condition].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                }

                let block = self.solve_block(ast, block, context);
                let otherwise = otherwise.map(|otherwise| self.solve_expr(ast, otherwise, context));

                Expr::IfElse { condition, block, otherwise }
            }
            Expr::While { condition, block } => {
                let condition = self.solve_expr(ast, condition, context);

                if context.solver.context.types[ast[condition].ty] != Type::Primitive(PrimitiveType::Bool) {
                    let ty = context.solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::PrimitiveType(PrimitiveType::Bool),
                                found: TypeErrorValue::ExplicitType(ast[condition].ty),
                            },
                            ast[condition].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                }

                let block = self.solve_block(ast, block, context);

                Expr::While { condition, block }
            }
            Expr::Block(block) => {
                let block = self.solve_block(ast, block, context);

                Expr::Block(block)
            }
            Expr::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => {
                let target = target.map(|target| self.solve_expr(ast, target, context));
                let target_ty = context.solver.solve(target_ty);

                let type_args = context.use_vtable_impl(target_ty, vtable);
                let item = UsedItem::VTable(target_ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                ast.use_item(
                    &context.solver.context.adt_types,
                    context.vtables,
                    context.functions,
                    &mut context.solver.context.types,
                    item,
                    Some(vtable),
                );

                Expr::VTableIndex {
                    target,
                    target_ty,
                    vtable,
                    func,
                }
            }
            Expr::TypeCast(expr, ty) => {
                let expr = self.solve_expr(ast, expr, context);

                Expr::TypeCast(expr, ty)
            }
            Expr::IsPattern { target, pattern } => {
                fn solve_pattern<E, M: ModuleLoader<E>>(
                    first_pass: &TypedAST<FirstPass>,
                    ast: &mut TypedAST,
                    pattern: IsPattern<FirstPass>,
                    context: &mut TypedASTContextRef<'_, E, M>,
                ) -> IsPattern<SolvedPass> {
                    match pattern {
                        IsPattern::Literal(expr) => {
                            let expr = first_pass.solve_expr(ast, expr, context);

                            IsPattern::Literal(expr)
                        }
                        IsPattern::EnumVariant {
                            adt,
                            adt_variant,
                            adt_type_args,
                            values,
                        } => {
                            let adt_type_args = adt_type_args.into_iter().map(|type_arg| context.solver.solve(type_arg)).collect();
                            let values = values
                                .into_iter()
                                .map(|(field, name, pattern)| (field, name, pattern.map(|pattern| solve_pattern(first_pass, ast, pattern, context))))
                                .collect();

                            IsPattern::EnumVariant {
                                adt,
                                adt_variant,
                                adt_type_args,
                                values,
                            }
                        }
                        IsPattern::TypeName { ty, name } => {
                            let ty = context.solver.solve(ty);

                            IsPattern::TypeName { ty, name }
                        }
                    }
                }

                let target = self.solve_expr(ast, target, context);
                let pattern = solve_pattern(self, ast, pattern, context);

                Expr::IsPattern { target, pattern }
            }
            Expr::Func(func) => {
                ast.use_item(
                    &context.solver.context.adt_types,
                    context.vtables,
                    context.functions,
                    &mut context.solver.context.types,
                    UsedItem::Func(func),
                    None,
                );

                return ast.add_expr(Expr::Func(func), context.solver.context.functions[func].ty, self[expr].span);
            }
            Expr::Error(error) => Expr::Error(error),
        };

        let ty = context.solver.solve(self[expr].ty);

        if let (Expr::Construct { .. }, Type::Adt(adt, type_args)) = (&value, &context.solver.context.types[ty]) {
            let item = UsedItem::Adt(*adt, type_args.clone());

            ast.use_item(
                &context.solver.context.adt_types,
                context.vtables,
                context.functions,
                &mut context.solver.context.types,
                item,
                None,
            );
        }

        ast.add_expr(value, ty, self[expr].span)
    }

    fn solve_block<E, M: ModuleLoader<E>>(&self, ast: &mut TypedAST, block: BlockRef, context: &mut TypedASTContextRef<'_, E, M>) -> BlockRef {
        let input_block = &self.blocks[block];
        let output_block = Block {
            stmts: input_block
                .value
                .stmts
                .iter()
                .map(|&stmt| {
                    let stmt = match &self[stmt] {
                        &Stmt::Expr(expr) => Stmt::Expr(self.solve_expr(ast, expr, context)),
                        Stmt::NewVar { mutable, name, value } => Stmt::NewVar {
                            mutable: *mutable,
                            name: name.clone(),
                            value: self.solve_expr(ast, *value, context),
                        },
                    };

                    ast.add_stmt(stmt)
                })
                .collect(),
            expr: input_block.value.expr.map(|expr| self.solve_expr(ast, expr, context)),
        };

        ast.add_block(output_block, context.solver.solve(input_block.ty), input_block.span)
    }

    fn solve_expr_final<E, M: ModuleLoader<E>>(self, root: ExprRef, context: &mut TypedASTContextRef<'_, E, M>) -> (TypedAST, ExprRef) {
        // context.solver.finalize();

        let mut ast = TypedAST {
            module: self.module,
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
            used_items: Vec::new(),
        };

        let expr = self.solve_expr(&mut ast, root, context);

        (ast, expr)
    }

    pub fn solve<E, M: ModuleLoader<E>>(mut self, root: BlockRef, context: &mut TypedASTContextRef<'_, E, M>, returns: TypeRef) -> (TypedAST, BlockRef) {
        // context.solver.finalize();

        let mut ast = TypedAST {
            module: self.module,
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
            used_items: mem::take(&mut self.used_items),
        };

        let returns_info = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, returns, &[]);

        context.solver.unify(self[root].ty, returns_info);

        let output_block = self.solve_block(&mut ast, root, context);

        match context.solver.context.types[returns] {
            Type::Trait(trait_ref, _) => {
                if let Some(vtable) = context.solver.context.find_vtable(ast[output_block].ty, Some(trait_ref)) {
                    let type_args = context.use_vtable_impl(ast[output_block].ty, vtable);
                    let item = UsedItem::VTable(ast[output_block].ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                    ast.use_item(
                        &context.solver.context.adt_types,
                        context.vtables,
                        context.functions,
                        &mut context.solver.context.types,
                        item,
                        Some(vtable),
                    );
                }
            }
            Type::Array(element, _) => {
                if let Type::Trait(trait_ref, _) = context.solver.context.types[element]
                    && let Some(Expr::Array { elements, .. }) = ast[output_block].value.expr.map(|expr| &ast[expr].value)
                {
                    let elements = elements.clone();

                    for element in elements {
                        if let Some(vtable) = context.solver.context.find_vtable(ast[element].ty, Some(trait_ref)) {
                            let type_args = context.use_vtable_impl(ast[element].ty, vtable);
                            let item = UsedItem::VTable(ast[element].ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                            ast.use_item(
                                &context.solver.context.adt_types,
                                context.vtables,
                                context.functions,
                                &mut context.solver.context.types,
                                item,
                                Some(vtable),
                            );
                        }
                    }
                }
            }
            _ => (),
        }

        (ast, output_block)
    }
}

impl<D: Descriptor> Index<ExprRef> for TypedAST<D> {
    type Output = Typed<Expr<D>, D>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.exprs[index]
    }
}

impl<D: Descriptor> Index<BlockRef> for TypedAST<D> {
    type Output = Typed<Block, D>;

    fn index(&self, index: BlockRef) -> &Self::Output {
        &self.blocks[index]
    }
}

impl<D: Descriptor> Index<StmtRef> for TypedAST<D> {
    type Output = Stmt;

    fn index(&self, index: StmtRef) -> &Self::Output {
        &self.statements[index]
    }
}

#[derive(Debug, Default)]
pub struct ConstantContext {
    pub frames: Vec<IndexMap<String, ConstantValue>>,
}

impl ConstantContext {
    pub fn push_frame(&mut self) {
        self.frames.push(IndexMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn current_frame(&self) -> &IndexMap<String, ConstantValue> {
        &self.frames[self.frames.len() - 1]
    }

    pub fn current_frame_mut(&mut self) -> &mut IndexMap<String, ConstantValue> {
        let frames = self.frames.len();

        &mut self.frames[frames - 1]
    }

    pub fn set_var<T: Into<String>>(&mut self, name: T, value: ConstantValue) {
        let frame = self.current_frame_mut();

        frame.insert(name.into(), value);
    }

    pub fn get_var(&self, (frame, var): (usize, usize)) -> &ConstantValue {
        &self.frames[frame][var]
    }

    pub fn search_var<T: AsRef<str>>(&mut self, name: T) -> Option<&ConstantValue> {
        self.search_var_idx(name).map(|idx| self.get_var(idx))
    }

    pub fn search_var_idx<T: AsRef<str>>(&mut self, name: T) -> Option<(usize, usize)> {
        let name = name.as_ref();

        for (frame_idx, frame) in self.frames.iter().rev().enumerate() {
            if let Some(var) = frame.get_index_of(name) {
                return Some((frame_idx, var));
            }
        }

        None
    }
}

pub trait IntoConstVal {
    /// # Errors
    ///
    /// TODO
    #[allow(clippy::result_unit_err)]
    fn into_const_val(self, ast: &TypedAST, type_context: &TypeContext, const_context: &mut ConstantContext) -> Result<ConstantValue, ()>;
}

#[cfg(test)]
mod tests {
    use std::{fmt, mem, path::PathBuf};

    use itertools::Itertools;
    use mollie_index::{Idx, IndexBoxedSlice, IndexVec};
    use mollie_typing::{
        Adt, AdtKind, AdtVariant, AdtVariantField, Arg, ArgType, Func, IntType, ModuleId, PrimitiveType, Trait, TraitFunc, Type, TypeContext, TypeInfo,
        UIntType,
    };

    use super::Stmt;
    use crate::{
        FileModuleLoader, FunctionBody, SolvedPass, TypedAST, TypedASTContext, UsedItem,
        block::BlockRef,
        expr::{Expr, ExprRef, IsPattern, LitExpr},
    };

    struct TypeBlockFmt<'a> {
        ast: &'a TypedAST,
        storage: &'a TypeContext,
        block: BlockRef,
    }

    impl fmt::Display for TypeBlockFmt<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let block = &self.ast[self.block];
            let mut prev_stmt = None;

            for stmt in block.value.stmts.iter().copied() {
                match (prev_stmt.map(|stmt| &self.ast[stmt]), &self.ast[stmt]) {
                    (None, _) | (Some(Stmt::NewVar { .. }), Stmt::NewVar { .. }) => (),
                    (Some(&Stmt::Expr(prev_expr)), &Stmt::Expr(expr)) => match (&self.ast[prev_expr].value, &self.ast[expr].value) {
                        (Expr::Lit(_), Expr::Lit(_))
                        | (Expr::Var(_), Expr::Var(_))
                        | (Expr::Array { .. }, Expr::Array { .. })
                        | (Expr::Binary { .. }, Expr::Binary { .. })
                        | (Expr::Closure { .. }, Expr::Closure { .. })
                        | (Expr::Call { .. }, Expr::Call { .. })
                        | (Expr::Construct { .. }, Expr::Construct { .. })
                        | (Expr::AdtIndex { .. }, Expr::AdtIndex { .. })
                        | (Expr::VTableIndex { .. }, Expr::VTableIndex { .. })
                        | (Expr::ArrayIndex { .. }, Expr::ArrayIndex { .. })
                        | (Expr::Error(_), Expr::Error(_)) => (),
                        _ => writeln!(f)?,
                    },
                    _ => writeln!(f)?,
                }

                match &self.ast[stmt] {
                    &Stmt::Expr(expr) => {
                        writeln!(f, "{};", TypeExprFmt {
                            ast: self.ast,
                            storage: self.storage,
                            expr,
                        })?;
                    }
                    Stmt::NewVar { mutable, name, value } => {
                        writeln!(
                            f,
                            "{} {name}: {} = {};",
                            if *mutable { "let" } else { "const" },
                            self.storage.display_of(self.ast[*value].ty),
                            TypeExprFmt {
                                ast: self.ast,
                                storage: self.storage,
                                expr: *value
                            }
                        )?;
                    }
                }

                prev_stmt = Some(stmt);
            }

            if let Some(expr) = block.value.expr {
                TypeExprFmt {
                    ast: self.ast,
                    storage: self.storage,
                    expr,
                }
                .fmt(f)?;
            }

            Ok(())
        }
    }

    struct TypePatternFmt<'a> {
        ast: &'a TypedAST,
        storage: &'a TypeContext,
        expr: &'a IsPattern<SolvedPass>,
    }

    impl fmt::Display for TypePatternFmt<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.expr {
                &IsPattern::Literal(expr) => TypeExprFmt {
                    ast: self.ast,
                    storage: self.storage,
                    expr,
                }
                .fmt(f),
                IsPattern::EnumVariant {
                    adt,
                    adt_variant,
                    adt_type_args,
                    values,
                } => write!(
                    f,
                    "{}{}::{}{}",
                    self.storage.adt_types[*adt].name.as_deref().unwrap_or_default(),
                    if adt_type_args.is_empty() {
                        String::new()
                    } else {
                        format!("::<{}>", adt_type_args.iter().copied().map(|ty| self.storage.display_of(ty)).join(", "))
                    },
                    self.storage.adt_types[*adt].variants[*adt_variant].name.as_deref().unwrap_or_default(),
                    if values.is_empty() {
                        String::new()
                    } else {
                        format!(
                            " {{ {} }}",
                            values
                                .iter()
                                .map(|(_, value, pattern)| format!(
                                    "{value}{}",
                                    pattern
                                        .as_ref()
                                        .map(|pattern| format!(": {}", TypePatternFmt {
                                            ast: self.ast,
                                            storage: self.storage,
                                            expr: pattern
                                        }))
                                        .unwrap_or_default()
                                ))
                                .join(", ")
                        )
                    }
                ),
                IsPattern::TypeName { ty, name } => write!(f, "{} {name}", self.storage.display_of(*ty)),
            }
        }
    }

    struct TypeExprFmt<'a> {
        ast: &'a TypedAST,
        storage: &'a TypeContext,
        expr: ExprRef,
    }

    impl TypeExprFmt<'_> {
        fn fork(&self, expr: ExprRef) -> Self {
            Self {
                ast: self.ast,
                storage: self.storage,
                expr,
            }
        }
    }

    impl fmt::Display for TypeExprFmt<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if self.expr == ExprRef::INVALID {
                return Ok(());
            }

            match &self.ast[self.expr].value {
                Expr::Var(name) => f.write_str(name),
                Expr::Lit(value) => match value {
                    LitExpr::Bool(value) => write!(f, "{value}"),
                    LitExpr::F32(value) => write!(f, "{value}"),
                    LitExpr::Int(value) => write!(f, "{value}{}", self.storage.display_of(self.ast[self.expr].ty)),
                    LitExpr::String(value) => write!(f, "{value:?}"),
                },
                Expr::Array { elements, .. } => {
                    write!(f, "[{}]", elements.iter().map(|&element| self.fork(element)).join(", "))
                }
                &Expr::Binary { operator, lhs, rhs } => {
                    write!(f, "{} {} {}", self.fork(lhs), operator.value, self.fork(rhs))
                }
                Expr::Closure { args, captures, body } => {
                    write!(
                        f,
                        "|{}|{} {{ {} }}",
                        args.iter().map(|arg| format!("{}: {}", arg.name, self.storage.display_of(arg.ty))).join(", "),
                        if captures.is_empty() {
                            String::new()
                        } else {
                            format!(
                                "({})",
                                captures.iter().map(|(name, ty)| format!("{name}: {}", self.storage.display_of(*ty))).join(", ")
                            )
                        },
                        self.fork(self.ast[*body].value.expr.unwrap())
                    )
                }
                Expr::Construct { adt, variant, fields } => {
                    write!(
                        f,
                        "{}{} {{ {} }}",
                        self.storage.adt_types[*adt].name.as_deref().unwrap_or_default(),
                        self.storage.adt_types[*adt].variants[*variant]
                            .name
                            .as_ref()
                            .map_or_else(String::new, |name| format!("::{name}")),
                        fields
                            .iter()
                            .skip(usize::from(matches!(self.storage.adt_types[*adt].kind, AdtKind::Enum)))
                            .filter(|(.., value)| *value != ExprRef::INVALID)
                            .map(|&(field_ref, _, field_value)| format!(
                                "{}: {}",
                                self.storage.adt_types[*adt][(*variant, field_ref)].name,
                                self.fork(field_value)
                            ))
                            .join(", "),
                    )
                }
                &Expr::AdtIndex { target, field } => {
                    write!(
                        f,
                        "{}.{}",
                        self.fork(target),
                        if let Type::Adt(adt, _) = self.storage.types[self.ast[target].ty] {
                            self.storage.adt_types[adt][field].name.as_str()
                        } else {
                            "<unknown>"
                        }
                    )
                }
                &Expr::VTableIndex {
                    target,
                    target_ty,
                    vtable,
                    func,
                } => match (target, self.storage.vtables[vtable].origin_trait) {
                    (Some(target), Some(origin_trait)) => write!(
                        f,
                        "({} as {})::{}",
                        self.fork(target),
                        self.storage.traits[origin_trait].name,
                        self.storage.vtables[vtable].functions[func].name
                    ),
                    (Some(target), None) => write!(f, "{}.{}", self.fork(target), self.storage.vtables[vtable].functions[func].name),
                    (None, Some(origin_trait)) => write!(
                        f,
                        "({} as {}).{}",
                        self.storage.display_of(target_ty),
                        self.storage.traits[origin_trait].name,
                        self.storage.vtables[vtable].functions[func].name
                    ),
                    (None, None) => write!(
                        f,
                        "{}::{}",
                        self.storage.display_of(target_ty),
                        self.storage.vtables[vtable].functions[func].name
                    ),
                },
                &Expr::ArrayIndex { target, element } => {
                    write!(f, "{}[{}]", self.fork(target), self.fork(element))
                }
                &Expr::While { condition, block } => {
                    write!(f, "while {} {{\n{}\n}}", self.fork(condition), TypeBlockFmt {
                        ast: self.ast,
                        storage: self.storage,
                        block
                    })
                }
                &Expr::Block(block) => {
                    write!(f, "{{\n{}\n}}", TypeBlockFmt {
                        ast: self.ast,
                        storage: self.storage,
                        block
                    })
                }
                &Expr::IfElse { condition, block, otherwise } => {
                    write!(
                        f,
                        "if {} {{\n{}\n}}{}",
                        self.fork(condition),
                        TypeBlockFmt {
                            ast: self.ast,
                            storage: self.storage,
                            block
                        },
                        otherwise.map_or_else(String::new, |otherwise| format!(" else {}", self.fork(otherwise)))
                    )
                }
                Expr::Call { func, args } => {
                    write!(f, "{}({})", self.fork(*func), args.iter().map(|&arg| self.fork(arg)).join(", "))
                }
                Expr::IsPattern { target, pattern } => {
                    write!(f, "{} is {}", self.fork(*target), TypePatternFmt {
                        ast: self.ast,
                        storage: self.storage,
                        expr: pattern
                    })
                }
                &Expr::TypeCast(target, ty) => {
                    write!(f, "{} as {ty}", self.fork(target))
                }
                &Expr::TraitFunc { trait_ref, func, .. } => write!(
                    f,
                    "{}::{}",
                    self.storage.traits[trait_ref].name, self.storage.traits[trait_ref].functions[func].name
                ),
                &Expr::Func(func_ref) => self.storage.functions[func_ref].name.fmt(f),
                Expr::Error(_) => f.write_str("<error>"),
            }
        }
    }

    #[test]
    fn test_literal() {
        #[allow(unused_variables)]
        let source = "{
            let hello = 12;
            let hello2 = [1, 2, 4i16];
            let volua = |a, b| { a + b };

            enum Option<T> {
                Some { value: T },
                None
            }

            struct A<T> {
                value: T
            }

            struct B<T> {
                value: A<T>
            }

            trait Hello<T> {
                func hello(self) -> A<T>;
            }

            struct C<T> {
                hi: Hello<T>
            }

            impl<T> Hello<T> for A<T> {
                func hello(self) -> A<T> {
                    let hew = A { value: true };

                    A { value: self.value }
                }
            }

            func println(input: i64) -> i64 {
                let dengi = Option::Some { value: A { value: 5i16 } };

                input + 32
            }

            let test_println = println(58);

            let heil = Option::Some { value: 53, double_double };
            let heil2 = Option { value: 53 };
            let world = test::test::Test { };
            let world2 = test::test::Test;

            double_double = 40;

            let a = if 1 == 4 {
                \"peak\"
            } else {
                \"kaep\"
            };

            heil.heil;
            hello.hello;

            if heil is Option::Some { value } {
                println(value);
            }

            hello = 54usize;

            calc_smth(|b| { b * 2 });

            hello = hello + 4 + volua(1, 2);

            let damn = B { value: A { value: 50 } };
            let volua2 = |a| { a.value.value - hello };

            damn = B { value: A { value: 50usize } };
            damn.val;
            damn();
            volua2(damn);
            volua2();
            volua2(hello);

            let mm = C { hi: damn.value.hello() };

            mm = mm;
            mm
        }";
        let source = include_str!("../../../examples/ui.mol");
        let mut context = TypedASTContext::<(), _>::new(TypeContext::new(), FileModuleLoader {
            current_dir: PathBuf::from("/home/aiving/Documents/dev-v2/dev/meralus-project/mollie/examples"),
        });

        let usize = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize)));
        let string = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::String));
        let bool = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::Bool));
        let any = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::Any));
        let f32 = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::F32));
        let void = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::Void));

        for (name, args, returns) in [
            ("println", Box::new([usize]) as Box<[_]>, void),
            ("println_frame_addr", Box::new([]), void),
            ("println_fat", Box::new([string]), void),
            ("println_str", Box::new([string]), void),
            ("println_bool", Box::new([bool]), void),
            ("println_f32", Box::new([f32]), void),
            ("println_addr", Box::new([any]), void),
            ("get_type_idx", Box::new([any]), usize),
            ("get_size", Box::new([any]), usize),
        ] {
            let ty = context.type_context.types.get_or_add(Type::Func(args, returns));

            context.functions.push(FunctionBody::Import(name));
            context.type_context.register_func_in_module(ModuleId::ZERO, Func {
                postfix: false,
                name: name.to_string(),
                arg_names: Vec::new(),
                ty,
            });
        }

        let func = context.type_context.types.get_or_add(Type::Func(Box::new([]), usize));
        let module = context.type_context.register_module("std");

        context.functions.push(FunctionBody::Import("__ext__get_timestamp"));
        context.type_context.register_func_in_module(module, Func {
            postfix: false,
            name: "timestamp".to_string(),
            arg_names: Vec::new(),
            ty: func,
        });

        // let func_arg = func_compiler.checker.solver.add_info(TypeInfo::Generic(0,
        // None), None);
        // let ty = TypeInfo::Func(Box::new([FuncArg::Regular(func_arg)]),
        // func_compiler.checker.core_types.uint_size);

        // context
        //     .type_context
        //     .register_intrinsic_in_module(module, "size_of_val",
        // IntrinsicKind::SizeOfValue, ty);
        let generic = context.type_context.types.get_or_add(Type::Generic(0));

        context.type_context.register_adt_in_module(module, Adt {
            name: Some("Option".into()),
            collectable: true,
            kind: AdtKind::Enum,
            generics: 1,
            variants: IndexBoxedSlice::from_iter([
                AdtVariant {
                    name: Some("Some".into()),
                    discriminant: 0,
                    fields: IndexBoxedSlice::from_iter([AdtVariantField {
                        name: "value".into(),
                        ty: generic,
                        default_value: None,
                    }]),
                },
                AdtVariant {
                    name: Some("None".into()),
                    discriminant: 1,
                    fields: IndexBoxedSlice::default(),
                },
            ]),
        });

        let module = context.type_context.register_module("graphics");

        context.type_context.register_adt_in_module(module, Adt {
            name: Some("Image".into()),
            collectable: true,
            kind: AdtKind::Enum,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([
                AdtVariant {
                    name: Some("Path".into()),
                    discriminant: 0,
                    fields: IndexBoxedSlice::from_iter([AdtVariantField {
                        name: "value".into(),
                        ty: string,
                        default_value: None,
                    }]),
                },
                AdtVariant {
                    name: Some("Url".into()),
                    discriminant: 1,
                    fields: IndexBoxedSlice::from_iter([AdtVariantField {
                        name: "value".into(),
                        ty: string,
                        default_value: None,
                    }]),
                },
            ]),
        });

        let u8 = context.type_context.types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::U8)));

        context.type_context.register_adt_in_module(module, Adt {
            name: Some("Color".into()),
            collectable: true,
            kind: AdtKind::Struct,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([AdtVariant {
                name: None,
                discriminant: 0,
                fields: IndexBoxedSlice::from_iter([
                    AdtVariantField {
                        name: "red".into(),
                        ty: u8,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "blue".into(),
                        ty: u8,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "green".into(),
                        ty: u8,
                        default_value: None,
                    },
                ]),
            }]),
        });

        context.type_context.register_adt_in_module(module, Adt {
            name: Some("CornerRadius".into()),
            collectable: true,
            kind: AdtKind::Struct,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([AdtVariant {
                name: None,
                discriminant: 0,
                fields: IndexBoxedSlice::from_iter([
                    AdtVariantField {
                        name: "top_left".into(),
                        ty: f32,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "top_right".into(),
                        ty: f32,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "bottom_left".into(),
                        ty: f32,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "bottom_right".into(),
                        ty: f32,
                        default_value: None,
                    },
                ]),
            }]),
        });

        let size_ty = context.type_context.register_adt_in_module(module, Adt {
            name: Some("Size".into()),
            collectable: true,
            kind: AdtKind::Struct,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([AdtVariant {
                name: None,
                discriminant: 0,
                fields: IndexBoxedSlice::from_iter([
                    AdtVariantField {
                        name: "width".into(),
                        ty: f32,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "height".into(),
                        ty: f32,
                        default_value: None,
                    },
                ]),
            }]),
        });

        let point_ty = context.type_context.register_adt_in_module(module, Adt {
            name: Some("Point".into()),
            collectable: true,
            kind: AdtKind::Struct,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([AdtVariant {
                name: None,
                discriminant: 0,
                fields: IndexBoxedSlice::from_iter([
                    AdtVariantField {
                        name: "x".into(),
                        ty: f32,
                        default_value: None,
                    },
                    AdtVariantField {
                        name: "y".into(),
                        ty: f32,
                        default_value: None,
                    },
                ]),
            }]),
        });

        let draw_ctx_ty = context.type_context.register_adt(Adt {
            name: Some("DrawContext".into()),
            collectable: false,
            kind: AdtKind::Struct,
            generics: 0,
            variants: IndexBoxedSlice::from_iter([AdtVariant {
                name: None,
                discriminant: 0,
                fields: IndexBoxedSlice::from_iter([]),
            }]),
        });

        let point_type = context.type_context.types.get_or_add(Type::Adt(point_ty, Box::new([])));
        let size_type = context.type_context.types.get_or_add(Type::Adt(size_ty, Box::new([])));
        let draw_ctx_type = context.type_context.types.get_or_add(Type::Adt(draw_ctx_ty, Box::new([])));

        context.type_context.register_trait(Trait {
            name: "Drawable".into(),
            generics: 1,
            functions: IndexVec::from_iter([
                TraitFunc {
                    name: "measure".into(),
                    args: Box::new([
                        Arg {
                            name: "self".into(),
                            kind: ArgType::This,
                            ty: generic,
                        },
                        Arg {
                            name: "size".into(),
                            kind: ArgType::Regular,
                            ty: size_type,
                        },
                        Arg {
                            name: "ctx".into(),
                            kind: ArgType::Regular,
                            ty: draw_ctx_type,
                        },
                    ]),
                    returns: size_type,
                },
                TraitFunc {
                    name: "render".into(),
                    args: Box::new([
                        Arg {
                            name: "self".into(),
                            kind: ArgType::This,
                            ty: generic,
                        },
                        Arg {
                            name: "origin".into(),
                            kind: ArgType::Regular,
                            ty: point_type,
                        },
                        Arg {
                            name: "size".into(),
                            kind: ArgType::Regular,
                            ty: size_type,
                        },
                        Arg {
                            name: "ctx".into(),
                            kind: ArgType::Regular,
                            ty: draw_ctx_type,
                        },
                    ]),
                    returns: void,
                },
            ]),
        });

        let mut context_ref = context.take_ref();

        let isize = context_ref.solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::ISize)), None);
        let input_func = context_ref.solver.add_info(TypeInfo::Func(Box::new([isize]), isize), None);
        let func = context_ref.solver.add_info(TypeInfo::Func(Box::new([input_func]), isize), None);

        let draw_ctx_info = context_ref.solver.add_info(TypeInfo::Adt(draw_ctx_ty, Box::new([])), None);

        context_ref.solver.set_var("context", draw_ctx_info);
        context_ref.solver.set_var("calc_smth", func);

        let (solved, block) = context_ref.process(ModuleId::ZERO, source, void);
        let mut type_context = context.type_context;

        println!("Solved Typed AST dump (fmt):\n{}", TypeBlockFmt {
            ast: &solved,
            storage: &type_context,
            block
        });

        for item in solved.used_items {
            match item {
                UsedItem::VTable(ty, vtable, type_args) => println!(
                    "used vtable item: ({})<{}> for {}",
                    type_context.vtables[vtable].origin_trait.map_or_else(
                        || type_context.display_of(ty).to_string(),
                        |origin_trait| type_context.traits[origin_trait].name.clone()
                    ),
                    type_args.into_iter().map(|ty| type_context.display_of(ty)).join(", "),
                    type_context.display_of(ty)
                ),
                UsedItem::Adt(adt_ref, type_args) => println!(
                    "used adt item: {}<{}>",
                    type_context.adt_types[adt_ref].name.as_deref().unwrap_or_default(),
                    type_args.into_iter().map(|ty| type_context.display_of(ty)).join(", ")
                ),
                UsedItem::Func(func) => println!("used func item: {}", type_context.functions[func].name),
            }
        }

        for error in mem::take(&mut type_context.errors).into_values() {
            let mut report = ariadne::Report::build(ariadne::ReportKind::Error, ("ui.mol", error.span.start..error.span.end))
                .with_config(ariadne::Config::new().with_compact(true));

            error
                .value
                .add_to_report(("ui.mol", error.span.start..error.span.end), &mut report, &type_context);

            report.finish().print(("ui.mol", ariadne::Source::from(source))).unwrap();
        }
    }
}
