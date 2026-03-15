use std::mem;

use derive_where::derive_where;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexBoxedSlice};
use mollie_shared::{LangItem, Operator, Positioned, Span};
use mollie_typing::{
    AdtKind, AdtRef, AdtVariantRef, Arg, ArgType, FieldRef, FuncRef, IntType, LookupType, ModuleItem, PrimitiveType, SpecialAdtKind, TraitFuncRef, TraitRef,
    Type, TypeContext, TypeError, TypeErrorRef, TypeErrorValue, TypeInfo, TypeSolver, UIntType, VFuncRef, VTableRef,
};

use crate::{
    ConstantContext, Descriptor, FirstPass, FromParsed, IntoConstVal, ModuleLoader, SolvedPass, TypedAST, TypedASTContextRef, UsedItem,
    block::{Block, BlockRef},
    stmt::Stmt,
    ty::TypePathResult,
};

mollie_index::new_idx_type!(ExprRef);

#[derive(Debug, Clone)]
pub enum LitExpr {
    Bool(bool),
    F32(f32),
    Int(i64),
    String(String),
}

#[derive_where(Debug, Clone; D::Type)]
pub enum IsPattern<D: Descriptor> {
    Literal(ExprRef),
    EnumVariant {
        adt: AdtRef,
        adt_variant: AdtVariantRef,
        adt_type_args: Box<[D::Type]>,
        values: Box<[(FieldRef, String, Option<Self>)]>,
    },
    TypeName {
        ty: D::Type,
        name: String,
    },
}

impl FromParsed<mollie_parser::IsPattern> for IsPattern<FirstPass> {
    fn from_parsed(
        pattern: mollie_parser::IsPattern,
        ast: &mut TypedAST<FirstPass>,
        context: &mut TypedASTContextRef<'_>,
        loader: &mut dyn ModuleLoader,
        span: Span,
    ) -> Self {
        match pattern {
            mollie_parser::IsPattern::Literal(literal_expr) => Self::Literal(Expr::from_parsed(literal_expr, ast, context, loader, span)),
            mollie_parser::IsPattern::Type { ty, pattern } => {
                let path = TypePathResult::from_parsed(ty.value, ast, context, loader, ty.span);

                match path {
                    TypePathResult::Adt(adt, adt_type_args, adt_variant) => match (adt_variant, pattern.value) {
                        (Some(adt_variant), mollie_parser::TypePattern::Values(values)) => {
                            let fields = context.solver.instantiate_adt(adt, adt_variant, adt_type_args.as_ref()).collect::<Box<[_]>>();

                            let mut new_values = Vec::new();

                            for value in values {
                                if let Some(prop) = fields.iter().find_map(|prop| {
                                    let name = &context.solver.context.adt_types[adt].variants[adt_variant].fields[prop.0].name;

                                    if name == &value.value.name.value.0 {
                                        Some((name.clone(), prop.0, prop.1))
                                    } else {
                                        None
                                    }
                                }) {
                                    let pattern = if let Some(value) = value.value.value {
                                        Some(Self::from_parsed(value.value, ast, context, loader, value.span))
                                    } else {
                                        None
                                    };

                                    if pattern.is_none() {
                                        context.solver.set_var(&prop.0, prop.2);
                                    }

                                    new_values.push((prop.1, prop.0, pattern));
                                }
                            }

                            Self::EnumVariant {
                                adt,
                                adt_type_args,
                                adt_variant,
                                values: new_values.into_boxed_slice(),
                            }
                        }
                        (None, mollie_parser::TypePattern::Name(name)) => {
                            let ty = context.solver.add_info(TypeInfo::Adt(adt, adt_type_args), Some(span));

                            context.solver.set_var(&name.0, ty);

                            Self::TypeName { ty, name: name.0 }
                        }
                        _ => unimplemented!(),
                    },
                    TypePathResult::Trait(..) => todo!(),
                    TypePathResult::Error(..)
                    | TypePathResult::Intrinsic(..)
                    | TypePathResult::VFunc(..)
                    | TypePathResult::Module(_)
                    | TypePathResult::Generic(..)
                    | TypePathResult::Func(_) => unimplemented!(),
                }
            }
        }
    }
}

#[derive_where(Debug, Clone; D::Type, D::IndexResult)]
pub enum Expr<D: Descriptor = SolvedPass> {
    Lit(LitExpr),
    Var(String),
    Array {
        element: D::Type,
        elements: Box<[ExprRef]>,
    },
    IfElse {
        condition: ExprRef,
        block: BlockRef,
        otherwise: Option<ExprRef>,
    },
    While {
        condition: ExprRef,
        block: BlockRef,
    },
    Block(BlockRef),
    Binary {
        operator: Positioned<Operator>,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    Closure {
        args: Box<[Arg<D::Type>]>,
        captures: Box<[(String, D::Type)]>,
        body: BlockRef,
    },
    Call {
        func: ExprRef,
        args: Box<[ExprRef]>,
    },
    Construct {
        adt: AdtRef,
        variant: AdtVariantRef,
        fields: Box<[(FieldRef, D::Type, ExprRef)]>,
    },
    AdtIndex {
        target: ExprRef,
        field: D::IndexResult,
    },
    VTableIndex {
        target: Option<ExprRef>,
        target_ty: D::Type,
        vtable: VTableRef,
        func: VFuncRef,
    },
    ArrayIndex {
        target: ExprRef,
        element: ExprRef,
    },
    TraitFunc {
        target: ExprRef,
        trait_ref: TraitRef,
        func: TraitFuncRef,
    },
    Func(FuncRef),
    TypeCast(ExprRef, PrimitiveType),
    IsPattern {
        target: ExprRef,
        pattern: IsPattern<D>,
    },
    Error(TypeErrorRef),
}

impl FromParsed<mollie_parser::LiteralExpr, ExprRef> for Expr<FirstPass> {
    fn from_parsed(
        lit: mollie_parser::LiteralExpr,
        ast: &mut TypedAST<FirstPass>,
        context: &mut TypedASTContextRef<'_>,
        _loader: &mut dyn ModuleLoader,
        span: Span,
    ) -> ExprRef {
        match lit {
            mollie_parser::LiteralExpr::Number(number, postfix) => match (number.value, postfix) {
                (mollie_parser::Number::I64(value), Some(postfix)) => {
                    let info = match postfix.value.as_str() {
                        "usize" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize)),
                        "u64" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U64)),
                        "u32" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U32)),
                        "u16" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U16)),
                        "u8" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U8)),
                        "isize" => TypeInfo::Primitive(PrimitiveType::Int(IntType::ISize)),
                        "i64" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I64)),
                        "i32" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I32)),
                        "i16" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I16)),
                        "i8" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I8)),
                        _ => TypeInfo::Integer,
                    };

                    ast.add_expr(Self::Lit(LitExpr::Int(value)), context.solver.add_info(info, Some(number.span)), number.span)
                }
                (mollie_parser::Number::I64(value), None) => ast.add_expr(
                    Self::Lit(LitExpr::Int(value)),
                    context.solver.add_info(TypeInfo::Integer, Some(number.span)),
                    number.span,
                ),
                (mollie_parser::Number::F32(value), Some(postfix)) => {
                    let postfix_span = postfix.span;

                    if let Some(func_ref) = context.solver.context.modules[ast.module]
                        .items
                        .get(postfix.value.as_str())
                        .and_then(|item| if let &ModuleItem::Func(func) = item { Some(func) } else { None })
                    {
                        let func = &context.solver.context.functions[func_ref];

                        if func.postfix {
                            let Type::Func(args, returns) = &context.solver.context.types[func.ty] else {
                                return ast.add_expr(
                                    Self::Error(context.solver.context.error(
                                        TypeError::Unexpected {
                                            expected: TypeErrorValue::Function,
                                            found: TypeErrorValue::ExplicitType(func.ty),
                                        },
                                        postfix_span,
                                    )),
                                    context.solver.add_info(TypeInfo::Error, Some(span)),
                                    postfix_span,
                                );
                            };

                            let func_ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, func.ty, &[]);
                            let arg = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, args[0], &[]);
                            let returns = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, *returns, &[]);

                            let expr = ast.add_expr(Self::Lit(LitExpr::F32(value)), arg, number.span);

                            // if let TypeInfo::Func(args, returns) =
                            // checker.solver.get_info(checker.local_functions[func_ref].ty) {
                            //     for arg in args {
                            //         if let TypeInfo::Adt(adt_ref, _, args) =
                            // checker.solver.get_info(arg.inner()) {
                            //             ast.use_item(UsedItem::Adt(*adt_ref, args.clone()));
                            //         }
                            //     }

                            //     if let TypeInfo::Adt(adt_ref, _, args) =
                            // checker.solver.get_info(*returns) {         ast.
                            // use_item(UsedItem::Adt(*adt_ref, args.clone()));
                            //     }
                            // }

                            ast.use_item(
                                &context.solver.context.adt_types,
                                context.vtables,
                                context.functions,
                                &mut context.solver.context.types,
                                UsedItem::Func(func_ref),
                                None,
                            );

                            let func = ast.add_expr(Self::Func(func_ref), func_ty, postfix_span);

                            ast.add_expr(Self::Call { func, args: Box::new([expr]) }, returns, span)
                        } else {
                            ast.add_expr(
                                Self::Error(context.solver.context.error(TypeError::NotPostfix { name: postfix.value }, postfix_span)),
                                context.solver.add_info(TypeInfo::Error, Some(span)),
                                postfix_span,
                            )
                        }
                    } else {
                        ast.add_expr(
                            Self::Error(context.solver.context.error(
                                TypeError::NoFunction {
                                    name: postfix.value,
                                    postfix: true,
                                },
                                postfix_span,
                            )),
                            context.solver.add_info(TypeInfo::Error, Some(span)),
                            postfix_span,
                        )
                    }
                }
                (mollie_parser::Number::F32(value), None) => ast.add_expr(
                    Self::Lit(LitExpr::F32(value)),
                    context.solver.add_info(TypeInfo::Primitive(PrimitiveType::F32), Some(number.span)),
                    number.span,
                ),
            },
            mollie_parser::LiteralExpr::Bool(value) => ast.add_expr(
                Self::Lit(LitExpr::Bool(value)),
                context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Bool), Some(span)),
                span,
            ),
            mollie_parser::LiteralExpr::String(value) => ast.add_expr(
                Self::Lit(LitExpr::String(value)),
                context.solver.add_info(TypeInfo::Primitive(PrimitiveType::String), Some(span)),
                span,
            ),
        }
    }
}

impl FromParsed<mollie_parser::Expr, ExprRef> for Expr<FirstPass> {
    fn from_parsed(
        expr: mollie_parser::Expr,
        ast: &mut TypedAST<FirstPass>,
        context: &mut TypedASTContextRef<'_>,
        loader: &mut dyn ModuleLoader,
        span: Span,
    ) -> ExprRef {
        match expr {
            mollie_parser::Expr::Literal(literal_expr) => Self::from_parsed(literal_expr, ast, context, loader, span),
            mollie_parser::Expr::FunctionCall(func_call_expr) => {
                let func_span = func_call_expr.function.span;
                let func = Self::from_parsed(func_call_expr.function.value, ast, context, loader, func_call_expr.function.span);

                let ty = if let TypeInfo::Func(_, returns) = context.solver.type_infos[ast[func].ty].value {
                    returns
                } else {
                    context.solver.add_unknown(None, Some(func_span))
                };

                let args: Box<[_]> = func_call_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| Self::from_parsed(arg.value, ast, context, loader, arg.span))
                    .collect();

                let func_ty = context
                    .solver
                    .add_info(TypeInfo::Func(args.iter().map(|&arg| ast[arg].ty).collect(), ty), Some(span));

                context.solver.unify(ast[func].ty, func_ty);

                ast.add_expr(Self::Call { func, args }, ty, span)
            }
            mollie_parser::Expr::Node(mut node_expr) => {
                let name_span = node_expr.name.span;
                let ty = TypePathResult::from_parsed(node_expr.name.value, ast, context, loader, node_expr.name.span);

                let (adt, type_args, variant) = match ty {
                    TypePathResult::Adt(adt, type_args, variant) => (adt, type_args, variant),
                    result => {
                        let (ty, found) = match result {
                            TypePathResult::VFunc(.., vtable, vfunc) => {
                                let ty = TypeSolver::type_to_info(
                                    &mut context.solver.type_infos,
                                    context.solver.context,
                                    context.solver.context.vtables[vtable].functions[vfunc].ty,
                                    &[],
                                );

                                (ty, TypeErrorValue::Function)
                            }
                            TypePathResult::Trait(trait_ref, type_args) => {
                                let ty = context.solver.add_info(TypeInfo::Trait(trait_ref, type_args), None);

                                (ty, TypeErrorValue::Trait)
                            }
                            TypePathResult::Func(func) => {
                                let ty = TypeSolver::type_to_info(
                                    &mut context.solver.type_infos,
                                    context.solver.context,
                                    context.solver.context.functions[func].ty,
                                    &[],
                                );

                                (ty, TypeErrorValue::Function)
                            }
                            TypePathResult::Intrinsic(_) => todo!(),
                            TypePathResult::Generic(ty) => (ty, TypeErrorValue::Generic),
                            TypePathResult::Module(_) => (context.solver.add_info(TypeInfo::Error, None), TypeErrorValue::Module),
                            TypePathResult::Error(..) => (context.solver.add_info(TypeInfo::Error, None), TypeErrorValue::Nothing),
                            TypePathResult::Adt(..) => unreachable!(),
                        };

                        return ast.add_expr(
                            Self::Error(context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::Adt(SpecialAdtKind::AnyOf),
                                    found,
                                },
                                name_span,
                            )),
                            ty,
                            name_span,
                        );
                    }
                };

                let variant = match (context.solver.context.adt_types[adt].kind, variant) {
                    (AdtKind::Struct | AdtKind::View, None) => AdtVariantRef::ZERO,
                    (AdtKind::Struct | AdtKind::View, Some(_)) => todo!(),
                    (AdtKind::Enum, None) => {
                        let ty = context.solver.add_info(TypeInfo::Error, None);

                        return ast.add_expr(
                            Self::Error(context.solver.context.error(TypeError::VariantRequired(adt), name_span)),
                            ty,
                            name_span,
                        );
                    }
                    (AdtKind::Enum, Some(variant)) => variant,
                };

                let mut fields = context
                    .solver
                    .instantiate_adt(adt, variant, &type_args)
                    .map(|(field_ref, field_type)| (field_ref, field_type, ExprRef::INVALID))
                    .collect::<IndexBoxedSlice<FieldRef, _>>();

                let ty = context.solver.add_info(TypeInfo::Adt(adt, type_args), None);

                for prop in node_expr.properties {
                    let name = prop.value.name.value.0;

                    let field = context.solver.context.adt_types[adt].variants[variant]
                        .fields
                        .iter()
                        .find_map(|(field_ref, field)| if field.name == name { Some(field_ref) } else { None });

                    if let Some(field) = field {
                        let value = match prop.value.value {
                            Some(value) => Self::from_parsed(value.value, ast, context, loader, value.span),
                            None => {
                                if let Some((frame, ty)) = context.solver.get_var(&name) {
                                    if let Some(current_frame) = context.current_frame
                                        && frame < current_frame
                                    {
                                        context.captures.push((name.clone(), ty));
                                    }

                                    ast.add_expr(Self::Var(name), ty, prop.value.name.span)
                                } else {
                                    ast.add_expr(
                                        Self::Error(context.solver.context.error(
                                            TypeError::NotFound {
                                                name,
                                                was_looking_for: LookupType::Variable,
                                            },
                                            prop.value.name.span,
                                        )),
                                        fields[field].1,
                                        prop.value.name.span,
                                    )
                                }
                            }
                        };

                        context.solver.unify(fields[field].1, ast[value].ty);

                        fields[field].2 = value;
                    } else {
                        let ty = context.solver.add_info(TypeInfo::Unknown(None), None);

                        ast.add_expr(
                            Self::Error(context.solver.context.error(TypeError::NoField { adt, variant, name }, prop.span)),
                            ty,
                            prop.span,
                        );
                    }
                }

                if !node_expr.children.value.is_empty() {
                    if matches!(context.solver.context.adt_types[adt].kind, AdtKind::View) {
                        let field = context.solver.context.adt_types[adt].variants[variant]
                            .fields
                            .iter()
                            .find_map(|(field_ref, field)| if field.name == "children" { Some(field_ref) } else { None });

                        if let Some(field) = field {
                            let element_count = node_expr.children.value.len();
                            let value = if element_count == 1 {
                                let node = node_expr.children.value.remove(0);

                                Self::from_parsed(mollie_parser::Expr::Node(node.value), ast, context, loader, node.span)
                            } else {
                                let elements: Box<[ExprRef]> = node_expr
                                    .children
                                    .value
                                    .into_iter()
                                    .map(|child| Self::from_parsed(mollie_parser::Expr::Node(child.value), ast, context, loader, child.span))
                                    .collect();

                                let element = ast.exprs[elements[0]].ty;
                                let ty = context.solver.add_info(TypeInfo::Array(element, Some(element_count)), None);

                                ast.add_expr(Self::Array { element, elements }, ty, node_expr.children.span)
                            };

                            context.solver.unify(fields[field].1, ast[value].ty);

                            fields[field].2 = value;
                        } else {
                            todo!("error: this view type can't have children");
                        }
                    } else {
                        todo!("error: children-syntax is available only for view types");
                    }
                }

                let fields = fields.raw;

                ast.add_expr(Self::Construct { adt, variant, fields }, ty, span)
            }
            mollie_parser::Expr::Index(index_expr) => {
                let target = Self::from_parsed(index_expr.target.value, ast, context, loader, index_expr.target.span);

                match index_expr.index.value {
                    mollie_parser::IndexTarget::Named(ident) => {
                        let ty = context.solver.solve(ast[target].ty);

                        let ty = if let Type::Adt(adt, _) = context.solver.context.types[ty]
                            && context.solver.context.adt_types[adt].kind != AdtKind::Enum
                            && let Some(field) = context.solver.context.adt_types[adt].variants[AdtVariantRef::ZERO]
                                .fields
                                .values()
                                .find(|field| field.name == ident.0)
                        {
                            TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, field.ty, &[])
                        } else {
                            context.solver.add_unknown(None, Some(index_expr.index.span))
                        };

                        ast.add_expr(Self::AdtIndex { target, field: ident.0 }, ty, span)
                    }
                    mollie_parser::IndexTarget::Expression(expr) => {
                        let element = Self::from_parsed(*expr, ast, context, loader, index_expr.index.span);
                        let usize = context
                            .solver
                            .add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize)), Some(index_expr.index.span));

                        context.solver.unify(ast[element].ty, usize);

                        ast.add_expr(
                            Self::ArrayIndex { target, element },
                            context.solver.add_unknown(None, Some(index_expr.index.span)),
                            span,
                        )
                    }
                }
            }
            mollie_parser::Expr::Binary(binary_expr) => {
                let lhs = Self::from_parsed(binary_expr.lhs.value, ast, context, loader, binary_expr.lhs.span);
                let rhs = Self::from_parsed(binary_expr.rhs.value, ast, context, loader, binary_expr.rhs.span);

                context.solver.unify(ast[rhs].ty, ast[lhs].ty);

                let ty = match binary_expr.operator.value {
                    Operator::Equal
                    | Operator::NotEqual
                    | Operator::LessThan
                    | Operator::LessThanEqual
                    | Operator::GreaterThan
                    | Operator::GreaterThanEqual => context
                        .solver
                        .add_info(TypeInfo::Primitive(PrimitiveType::Bool), Some(binary_expr.operator.span)),
                    _ => ast[lhs].ty,
                };

                ast.add_expr(
                    Self::Binary {
                        operator: binary_expr.operator,
                        lhs,
                        rhs,
                    },
                    ty,
                    span,
                )
            }
            mollie_parser::Expr::TypeIndex(type_path_expr) => {
                let result = TypePathResult::from_parsed(type_path_expr, ast, context, loader, span);

                match result {
                    TypePathResult::VFunc(adt_ref, type_info_refs, vtable, func) => {
                        let target_ty = context.solver.add_info(TypeInfo::Adt(adt_ref, type_info_refs), None);
                        let ty = context.solver.context.vtables[vtable].functions[func].ty;
                        let ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]);

                        ast.add_expr(
                            Self::VTableIndex {
                                target: None,
                                target_ty,
                                vtable,
                                func,
                            },
                            ty,
                            span,
                        )
                    }
                    TypePathResult::Adt(adt, type_info_refs, variant) => {
                        let ty = context.solver.add_info(TypeInfo::Adt(adt, type_info_refs), None);
                        let variant = match (context.solver.context.adt_types[adt].kind, variant) {
                            (AdtKind::Struct | AdtKind::View, None) => AdtVariantRef::ZERO,
                            (AdtKind::Struct | AdtKind::View, Some(_)) => todo!(),
                            (AdtKind::Enum, None) => {
                                let ty = context.solver.add_info(TypeInfo::Error, None);

                                return ast.add_expr(Self::Error(context.solver.context.error(TypeError::VariantRequired(adt), span)), ty, span);
                            }
                            (AdtKind::Enum, Some(variant)) => variant,
                        };

                        ast.add_expr(
                            Self::Construct {
                                adt,
                                variant,
                                fields: Box::new([]),
                            },
                            ty,
                            span,
                        )
                    }
                    TypePathResult::Trait(trait_ref, type_args) => {
                        let ty = context.solver.add_info(TypeInfo::Trait(trait_ref, type_args), None);

                        ast.add_expr(
                            Self::Error(context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::Value,
                                    found: TypeErrorValue::Trait,
                                },
                                span,
                            )),
                            ty,
                            span,
                        )
                    }
                    TypePathResult::Func(func_ref) => {
                        let ty = context.solver.context.functions[func_ref].ty;
                        let ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]);

                        ast.add_expr(Self::Func(func_ref), ty, span)
                    }
                    TypePathResult::Intrinsic(intrinsic_kind) => todo!("value: intrinsic({intrinsic_kind:?})"),
                    TypePathResult::Generic(ty) => ast.add_expr(
                        Self::Error(context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Value,
                                found: TypeErrorValue::Generic,
                            },
                            span,
                        )),
                        ty,
                        span,
                    ),
                    TypePathResult::Module(_) => {
                        let ty = context.solver.add_info(TypeInfo::Error, None);

                        ast.add_expr(
                            Self::Error(context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::Value,
                                    found: TypeErrorValue::Module,
                                },
                                span,
                            )),
                            ty,
                            span,
                        )
                    }
                    TypePathResult::Error(error, span) => {
                        let ty = context.solver.add_info(TypeInfo::Error, None);

                        ast.add_expr(Self::Error(error), ty, span)
                    }
                }
            }
            mollie_parser::Expr::Array(array_expr) => {
                let element = context.solver.add_unknown(None, Some(span));
                let mut elements = Vec::with_capacity(array_expr.elements.capacity());

                for arr_element in array_expr.elements {
                    let arr_element = Self::from_parsed(arr_element.value, ast, context, loader, arr_element.span);

                    context.solver.unify(ast[arr_element].ty, element);

                    elements.push(arr_element);
                }

                let elements = elements.into_boxed_slice();
                let size = elements.len();
                let array_ty = context.solver.add_info(TypeInfo::Array(element, Some(size)), Some(span));

                ast.add_expr(Self::Array { element, elements }, array_ty, span)
            }
            mollie_parser::Expr::IfElse(if_else_expr) => {
                let condition = Self::from_parsed(if_else_expr.condition.value, ast, context, loader, if_else_expr.condition.span);
                let expected = context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Bool), None);

                context.solver.unify(ast[condition].ty, expected);

                let block = Block::from_parsed(if_else_expr.block.value, ast, context, loader, if_else_expr.block.span);
                let otherwise = if let Some(otherwise) = if_else_expr.else_block {
                    let expr = Self::from_parsed(otherwise.value, ast, context, loader, otherwise.span);

                    context.solver.unify(ast[block].ty, ast[expr].ty);

                    Some(expr)
                } else {
                    let expected = context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None);

                    context.solver.unify(ast[block].ty, expected);

                    None
                };

                let ty = ast[block].ty;

                ast.add_expr(Self::IfElse { condition, block, otherwise }, ty, span)
            }
            mollie_parser::Expr::While(while_expr) => {
                let condition = Self::from_parsed(while_expr.condition.value, ast, context, loader, while_expr.condition.span);
                let expected = context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Bool), None);

                context.solver.unify(ast[condition].ty, expected);

                let block = Block::from_parsed(while_expr.block.value, ast, context, loader, while_expr.block.span);
                let ty = ast[block].ty;
                let expected = context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None);

                context.solver.unify(ty, expected);

                ast.add_expr(Self::While { condition, block }, ty, span)
            }
            mollie_parser::Expr::Block(block_expr) => {
                let block = Block::from_parsed(block_expr, ast, context, loader, span);
                let ty = ast[block].ty;

                ast.add_expr(Self::Block(block), ty, span)
            }
            mollie_parser::Expr::ForIn(for_in) => {
                let target_span = for_in.target.span;
                let target = Self::from_parsed(for_in.target.value, ast, context, loader, for_in.target.span);
                let while_loop = (|| {
                    let into_iterator = context.solver.context.get_trait_item(LangItem::IntoIterator)?;
                    let ty = context.solver.solve(ast[target].ty);
                    let vtable = context.solver.context.find_vtable(ty, Some(into_iterator))?;

                    let (func_ty, func) = {
                        let type_args = context.use_vtable_impl(ty, vtable);
                        let func_ref = context
                            .solver
                            .context
                            .get_trait_func_item(into_iterator, LangItem::IntoIteratorIntoIter)?
                            .as_vfunc();

                        let func = &context.solver.context.vtables[vtable].functions[func_ref];
                        let func_ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, func.ty, &type_args);
                        let item = UsedItem::VTable(ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                        ast.use_item(
                            &context.solver.context.adt_types,
                            context.vtables,
                            context.functions,
                            &mut context.solver.context.types,
                            item,
                            Some(vtable),
                        );

                        (func_ty, func_ref)
                    };

                    let func = ast.add_expr(
                        Self::VTableIndex {
                            target: Some(target),
                            target_ty: ast[target].ty,
                            vtable,
                            func,
                        },
                        func_ty,
                        target_span,
                    );

                    let TypeInfo::Func(_, returns) = context.solver.type_infos[context.solver.get_info(func_ty)].value else {
                        return None;
                    };

                    let value = ast.add_expr(Self::Call { func, args: Box::new([]) }, returns, span);
                    let mut stmts = Vec::new();

                    stmts.push(ast.add_stmt(Stmt::NewVar {
                        mutable: true,
                        name: "!".into(),
                        value,
                    }));

                    let iterator = context.solver.context.get_trait_item(LangItem::Iterator)?;
                    let ty = context.solver.solve(returns);
                    let vtable = context.solver.context.find_vtable(ty, Some(iterator))?;

                    let (func_ty, func) = {
                        let type_args = context.use_vtable_impl(ty, vtable);
                        let func_ref = context.solver.context.get_trait_func_item(iterator, LangItem::IteratorNext)?.as_vfunc();
                        let func = &context.solver.context.vtables[vtable].functions[func_ref];
                        let func_ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, func.ty, &type_args);
                        let item = UsedItem::VTable(ty, vtable, type_args.into_iter().map(|ty| context.solver.solve(ty)).collect());

                        ast.use_item(
                            &context.solver.context.adt_types,
                            context.vtables,
                            context.functions,
                            &mut context.solver.context.types,
                            item,
                            Some(vtable),
                        );

                        (func_ty, func_ref)
                    };

                    let target = ast.add_expr(Self::Var(String::from("!")), returns, span);
                    let func = ast.add_expr(
                        Self::VTableIndex {
                            target: Some(target),
                            target_ty: ast[target].ty,
                            vtable,
                            func,
                        },
                        func_ty,
                        target_span,
                    );

                    let TypeInfo::Func(_, returns) = context.solver.type_infos[context.solver.get_info(func_ty)].value else {
                        return None;
                    };

                    let item = ast.add_expr(Self::Call { func, args: Box::new([]) }, returns, span);
                    let adt = context.solver.context.get_adt_item(LangItem::Option)?;
                    let adt_variant = context.solver.context.get_adt_variant_item(LangItem::OptionSome)?.1;

                    context.solver.push_frame();

                    if let TypeInfo::Adt(.., type_args) = &context.solver.type_infos[context.solver.get_info(returns)].value {
                        let type_args = type_args.clone();
                        let returns = context.solver.context.adt_types[adt].variants[adt_variant].fields[FieldRef::new(1)].ty;
                        let returns = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, returns, &type_args);

                        context.solver.set_var(&for_in.name.value.0, returns);
                    }

                    let adt_type_args = if let TypeInfo::Adt(.., generic_args) = &context.solver.type_infos[context.solver.get_info(returns)].value {
                        generic_args.clone()
                    } else {
                        Box::default()
                    };

                    let condition = ast.add_expr(
                        Self::IsPattern {
                            target: item,
                            pattern: IsPattern::EnumVariant {
                                adt,
                                adt_variant,
                                adt_type_args,
                                values: Box::new([(FieldRef::new(1), for_in.name.value.0, None)]),
                            },
                        },
                        context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Bool), None),
                        span,
                    );

                    let block = Block::from_parsed(for_in.block.value, ast, context, loader, for_in.block.span);
                    let while_loop = ast.add_expr(Self::While { condition, block }, ast[block].ty, span);

                    stmts.push(ast.add_stmt(Stmt::Expr(while_loop)));
                    context.solver.pop_frame();

                    let block = ast.add_block(
                        Block {
                            stmts: stmts.into_boxed_slice(),
                            expr: None,
                        },
                        context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None),
                        span,
                    );

                    let ty = ast[block].ty;

                    Some(ast.add_expr(Self::Block(block), ty, span))
                })();

                while_loop.unwrap()
            }
            mollie_parser::Expr::Is(is_expr) => {
                let target = Self::from_parsed(is_expr.target.value, ast, context, loader, is_expr.target.span);
                let pattern = IsPattern::from_parsed(is_expr.pattern.value, ast, context, loader, is_expr.pattern.span);

                match &pattern {
                    IsPattern::Literal(_) => (),
                    IsPattern::EnumVariant { adt_type_args, .. } => {
                        if let TypeInfo::Adt(_, target_type_args) = &context.solver.type_infos[ast[target].ty].value {
                            let target_type_args = target_type_args.clone();

                            for (type_arg, target_type_arg) in adt_type_args.iter().copied().zip(target_type_args) {
                                context.solver.unify(type_arg, target_type_arg);
                            }
                        }
                    }
                    &IsPattern::TypeName { ty, .. } => {
                        if let TypeInfo::Adt(_, adt_type_args) = &context.solver.type_infos[ty].value
                            && let TypeInfo::Adt(_, target_type_args) = &context.solver.type_infos[ast[target].ty].value
                        {
                            let adt_type_args = adt_type_args.clone();
                            let target_type_args = target_type_args.clone();

                            for (type_arg, target_type_arg) in adt_type_args.into_iter().zip(target_type_args) {
                                context.solver.unify(type_arg, target_type_arg);
                            }
                        }
                    }
                }

                ast.add_expr(
                    Self::IsPattern { target, pattern },
                    context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Bool), None),
                    span,
                )
            }
            mollie_parser::Expr::Cast(expr, new_type) => {
                let expr = Self::from_parsed(expr.value, ast, context, loader, expr.span);

                ast.add_expr(
                    Self::TypeCast(expr, new_type.value),
                    context.solver.add_info(TypeInfo::Primitive(new_type.value), Some(new_type.span)),
                    span,
                )
            }
            mollie_parser::Expr::Closure(closure_expr) => {
                let current_frame = context.solver.push_frame();
                let prev_captures = mem::take(&mut context.captures);
                let prev_current_frame = context.current_frame.replace(current_frame);

                let args: Box<[_]> = closure_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| {
                        let ty = context.solver.add_unknown(None, Some(arg.span));

                        context.solver.set_var(&arg.value.0, ty);

                        Arg {
                            name: arg.value.0,
                            kind: ArgType::Regular,
                            ty,
                        }
                    })
                    .collect();

                let body = Block::from_parsed(closure_expr.body.value, ast, context, loader, closure_expr.body.span);
                let ty = context
                    .solver
                    .add_info(TypeInfo::Func(args.iter().map(|arg| arg.ty).collect(), ast[body].ty), Some(span));

                context.solver.pop_frame();
                context.current_frame = prev_current_frame;

                let captures = mem::replace(&mut context.captures, prev_captures).into_boxed_slice();

                ast.add_expr(Self::Closure { args, captures, body }, ty, span)
            }
            mollie_parser::Expr::Ident(ident) => {
                if let Some((frame, ty)) = context.solver.get_var(&ident) {
                    if let Some(current_frame) = context.current_frame
                        && frame < current_frame
                    {
                        context.captures.push((ident.0.clone(), ty));
                    }

                    ast.add_expr(Self::Var(ident.0), ty, span)
                } else if let Some(item) = context.solver.context.modules[ast.module].items.get(&ident) {
                    match item {
                        ModuleItem::SubModule(_) => {
                            let ty = context.solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(context.solver.context.error(
                                    TypeError::Unexpected {
                                        expected: TypeErrorValue::Value,
                                        found: TypeErrorValue::Module,
                                    },
                                    span,
                                )),
                                ty,
                                span,
                            )
                        }
                        ModuleItem::Adt(adt) => {
                            let found = TypeErrorValue::Adt(SpecialAdtKind::Specific(context.solver.context.adt_types[*adt].kind));
                            let ty = context.solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(context.solver.context.error(
                                    TypeError::Unexpected {
                                        expected: TypeErrorValue::Value,
                                        found,
                                    },
                                    span,
                                )),
                                ty,
                                span,
                            )
                        }
                        ModuleItem::Trait(_) => {
                            let ty = context.solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(context.solver.context.error(
                                    TypeError::Unexpected {
                                        expected: TypeErrorValue::Value,
                                        found: TypeErrorValue::Trait,
                                    },
                                    span,
                                )),
                                ty,
                                span,
                            )
                        }
                        &ModuleItem::Func(func_ref) => {
                            let ty = context.solver.context.functions[func_ref].ty;
                            let ty = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]);

                            ast.add_expr(Self::Func(func_ref), ty, span)
                        }
                        ModuleItem::Intrinsic(intrinsic_kind, _) => todo!("value: intrinsic({intrinsic_kind:?})"),
                    }
                } else {
                    let ty = context.solver.add_info(TypeInfo::Error, None);

                    ast.add_expr(
                        Self::Error(context.solver.context.error(
                            TypeError::NotFound {
                                name: ident.0,
                                was_looking_for: LookupType::Variable,
                            },
                            span,
                        )),
                        ty,
                        span,
                    )
                }
            }
            mollie_parser::Expr::This => {
                if let Some((frame, ty)) = context.solver.get_var("self") {
                    if let Some(current_frame) = context.current_frame
                        && frame < current_frame
                    {
                        context.captures.push(("self".into(), ty));
                    }

                    ast.add_expr(Self::Var("self".to_string()), ty, span)
                } else {
                    let ty = context.solver.add_info(TypeInfo::Error, None);

                    ast.add_expr(
                        Self::Error(context.solver.context.error(
                            TypeError::NotFound {
                                name: String::from("self"),
                                was_looking_for: LookupType::Variable,
                            },
                            span,
                        )),
                        ty,
                        span,
                    )
                }
            }
            mollie_parser::Expr::Nothing => todo!(),
        }
    }
}

impl IntoConstVal for ExprRef {
    fn into_const_val(self, ast: &TypedAST<SolvedPass>, type_context: &TypeContext, const_context: &mut ConstantContext) -> Result<ConstantValue, ()> {
        Ok(match &ast[self].value {
            Expr::Lit(literal_expr) => match (literal_expr, &type_context.types[ast[self].ty]) {
                (&LitExpr::Int(value), Type::Primitive(primitive)) => match primitive {
                    PrimitiveType::Int(IntType::ISize) => ConstantValue::ISize(value.try_into().map_err(|_| ())?),
                    PrimitiveType::Int(IntType::I64) => ConstantValue::I64(value),
                    PrimitiveType::Int(IntType::I32) => ConstantValue::I32(value.try_into().map_err(|_| ())?),
                    PrimitiveType::Int(IntType::I16) => ConstantValue::I16(value.try_into().map_err(|_| ())?),
                    PrimitiveType::Int(IntType::I8) => ConstantValue::I8(value.try_into().map_err(|_| ())?),
                    PrimitiveType::UInt(UIntType::USize) => ConstantValue::USize(value.try_into().map_err(|_| ())?),
                    PrimitiveType::UInt(UIntType::U64) => ConstantValue::U64(value.cast_unsigned()),
                    PrimitiveType::UInt(UIntType::U32) => ConstantValue::U32(value.try_into().map_err(|_| ())?),
                    PrimitiveType::UInt(UIntType::U16) => ConstantValue::U16(value.try_into().map_err(|_| ())?),
                    PrimitiveType::UInt(UIntType::U8) => ConstantValue::U8(value.try_into().map_err(|_| ())?),
                    _ => panic!("wrong type for integer"),
                },
                (&LitExpr::F32(value), Type::Primitive(PrimitiveType::F32)) => ConstantValue::F32(value),
                (&LitExpr::Bool(value), Type::Primitive(PrimitiveType::Bool)) => ConstantValue::Bool(value),
                (LitExpr::String(value), Type::Primitive(PrimitiveType::String)) => ConstantValue::String(value.clone()),
                (literal, _) => panic!("wrong type for literal: expected {} for {literal:?}", type_context.display_of(ast[self].ty)),
            },
            Expr::IfElse { condition, block, otherwise } => {
                if condition.into_const_val(ast, type_context, const_context)? == ConstantValue::Bool(true) {
                    block.into_const_val(ast, type_context, const_context)?
                } else if let Some(block) = otherwise {
                    block.into_const_val(ast, type_context, const_context)?
                } else {
                    ConstantValue::Nothing
                }
            }
            Expr::Block(block) => {
                const_context.push_frame();

                let value = block.into_const_val(ast, type_context, const_context)?;

                const_context.pop_frame();

                value
            }
            Expr::Var(name) => const_context.search_var(name).map_or_else(|| panic!("no variable called {name}"), Clone::clone),
            Expr::While { condition, block } => {
                while condition.into_const_val(ast, type_context, const_context)? == ConstantValue::Bool(true) {
                    block.into_const_val(ast, type_context, const_context)?;
                }

                ConstantValue::Nothing
            }
            Expr::Array { elements, .. } => ConstantValue::Array(
                elements
                    .iter()
                    .map(|expr| expr.into_const_val(ast, type_context, const_context))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::Binary { operator, lhs, rhs } => match (
                lhs.into_const_val(ast, type_context, const_context)?,
                operator.value,
                rhs.into_const_val(ast, type_context, const_context)?,
            ) {
                (ConstantValue::I8(a), Operator::Add, ConstantValue::I8(b)) => ConstantValue::I8(a + b),
                (ConstantValue::U8(a), Operator::Add, ConstantValue::U8(b)) => ConstantValue::U8(a + b),
                (ConstantValue::I16(a), Operator::Add, ConstantValue::I16(b)) => ConstantValue::I16(a + b),
                (ConstantValue::U16(a), Operator::Add, ConstantValue::U16(b)) => ConstantValue::U16(a + b),
                (ConstantValue::I32(a), Operator::Add, ConstantValue::I32(b)) => ConstantValue::I32(a + b),
                (ConstantValue::U32(a), Operator::Add, ConstantValue::U32(b)) => ConstantValue::U32(a + b),
                (ConstantValue::I64(a), Operator::Add, ConstantValue::I64(b)) => ConstantValue::I64(a + b),
                (ConstantValue::U64(a), Operator::Add, ConstantValue::U64(b)) => ConstantValue::U64(a + b),
                (ConstantValue::ISize(a), Operator::Add, ConstantValue::ISize(b)) => ConstantValue::ISize(a + b),
                (ConstantValue::USize(a), Operator::Add, ConstantValue::USize(b)) => ConstantValue::USize(a + b),
                (ConstantValue::F32(a), Operator::Add, ConstantValue::F32(b)) => ConstantValue::F32(a + b),
                (ConstantValue::I8(a), Operator::Sub, ConstantValue::I8(b)) => ConstantValue::I8(a - b),
                (ConstantValue::U8(a), Operator::Sub, ConstantValue::U8(b)) => ConstantValue::U8(a - b),
                (ConstantValue::I16(a), Operator::Sub, ConstantValue::I16(b)) => ConstantValue::I16(a - b),
                (ConstantValue::U16(a), Operator::Sub, ConstantValue::U16(b)) => ConstantValue::U16(a - b),
                (ConstantValue::I32(a), Operator::Sub, ConstantValue::I32(b)) => ConstantValue::I32(a - b),
                (ConstantValue::U32(a), Operator::Sub, ConstantValue::U32(b)) => ConstantValue::U32(a - b),
                (ConstantValue::I64(a), Operator::Sub, ConstantValue::I64(b)) => ConstantValue::I64(a - b),
                (ConstantValue::U64(a), Operator::Sub, ConstantValue::U64(b)) => ConstantValue::U64(a - b),
                (ConstantValue::ISize(a), Operator::Sub, ConstantValue::ISize(b)) => ConstantValue::ISize(a - b),
                (ConstantValue::USize(a), Operator::Sub, ConstantValue::USize(b)) => ConstantValue::USize(a - b),
                (ConstantValue::F32(a), Operator::Sub, ConstantValue::F32(b)) => ConstantValue::F32(a - b),
                (ConstantValue::I8(a), Operator::Mul, ConstantValue::I8(b)) => ConstantValue::I8(a * b),
                (ConstantValue::U8(a), Operator::Mul, ConstantValue::U8(b)) => ConstantValue::U8(a * b),
                (ConstantValue::I16(a), Operator::Mul, ConstantValue::I16(b)) => ConstantValue::I16(a * b),
                (ConstantValue::U16(a), Operator::Mul, ConstantValue::U16(b)) => ConstantValue::U16(a * b),
                (ConstantValue::I32(a), Operator::Mul, ConstantValue::I32(b)) => ConstantValue::I32(a * b),
                (ConstantValue::U32(a), Operator::Mul, ConstantValue::U32(b)) => ConstantValue::U32(a * b),
                (ConstantValue::I64(a), Operator::Mul, ConstantValue::I64(b)) => ConstantValue::I64(a * b),
                (ConstantValue::U64(a), Operator::Mul, ConstantValue::U64(b)) => ConstantValue::U64(a * b),
                (ConstantValue::ISize(a), Operator::Mul, ConstantValue::ISize(b)) => ConstantValue::ISize(a * b),
                (ConstantValue::USize(a), Operator::Mul, ConstantValue::USize(b)) => ConstantValue::USize(a * b),
                (ConstantValue::F32(a), Operator::Mul, ConstantValue::F32(b)) => ConstantValue::F32(a * b),
                (ConstantValue::I8(a), Operator::Div, ConstantValue::I8(b)) => ConstantValue::I8(a / b),
                (ConstantValue::U8(a), Operator::Div, ConstantValue::U8(b)) => ConstantValue::U8(a / b),
                (ConstantValue::I16(a), Operator::Div, ConstantValue::I16(b)) => ConstantValue::I16(a / b),
                (ConstantValue::U16(a), Operator::Div, ConstantValue::U16(b)) => ConstantValue::U16(a / b),
                (ConstantValue::I32(a), Operator::Div, ConstantValue::I32(b)) => ConstantValue::I32(a / b),
                (ConstantValue::U32(a), Operator::Div, ConstantValue::U32(b)) => ConstantValue::U32(a / b),
                (ConstantValue::I64(a), Operator::Div, ConstantValue::I64(b)) => ConstantValue::I64(a / b),
                (ConstantValue::U64(a), Operator::Div, ConstantValue::U64(b)) => ConstantValue::U64(a / b),
                (ConstantValue::ISize(a), Operator::Div, ConstantValue::ISize(b)) => ConstantValue::ISize(a / b),
                (ConstantValue::USize(a), Operator::Div, ConstantValue::USize(b)) => ConstantValue::USize(a / b),
                (ConstantValue::F32(a), Operator::Div, ConstantValue::F32(b)) => ConstantValue::F32(a / b),
                (ConstantValue::Bool(a), Operator::And, ConstantValue::Bool(b)) => ConstantValue::Bool(a && b),
                (ConstantValue::Bool(a), Operator::Or, ConstantValue::Bool(b)) => ConstantValue::Bool(a || b),
                (a, Operator::Equal, b) => ConstantValue::Bool(a == b),
                (a, Operator::NotEqual, b) => ConstantValue::Bool(a != b),
                (a, Operator::GreaterThan, b) => ConstantValue::Bool(a > b),
                (a, Operator::LessThan, b) => ConstantValue::Bool(a < b),
                (a, op, b) => panic!("wrong binary operation: {a:?} {op} {b:?}"),
            },
            Expr::Call { .. } => todo!(),
            Expr::Closure { .. } => todo!(),
            Expr::Construct { variant, fields, .. } => ConstantValue::Construct {
                ty: ast[self].ty.index(),
                variant: variant.index(),
                fields: fields
                    .iter()
                    .map(|v| {
                        (
                            v.0.index(),
                            if v.2 == Self::INVALID {
                                None
                            } else {
                                v.2.into_const_val(ast, type_context, const_context).ok()
                            },
                        )
                    })
                    .collect(),
            },
            Expr::IsPattern { .. } => todo!(),
            Expr::Error(_) => ConstantValue::Nothing,
            Expr::AdtIndex { .. } => todo!(),
            Expr::VTableIndex { .. } => todo!(),
            Expr::ArrayIndex { .. } => todo!(),
            Expr::Func(_) => todo!(),
            Expr::TypeCast(..) => todo!(),
            Expr::TraitFunc { .. } => todo!(),
        })
    }
}
