use mollie_shared::Span;
use mollie_typing::{
    AdtRef, AdtVariantRef, FuncRef, IntrinsicKind, LookupType, ModuleId, ModuleItem, PrimitiveType, SpecialAdtKind, TraitRef, Type, TypeError, TypeErrorRef,
    TypeErrorValue, TypeInfo, TypeInfoRef, TypeRef, TypeSolver, VFuncRef, VTableRef,
};

use crate::{FirstPass, FromParsed, ModuleLoader, TypedAST, TypedASTContextRef};

pub enum TypePathResult {
    VFunc(AdtRef, Box<[TypeInfoRef]>, VTableRef, VFuncRef),
    Adt(AdtRef, Box<[TypeInfoRef]>, Option<AdtVariantRef>),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Func(FuncRef),
    Intrinsic(IntrinsicKind),
    Generic(TypeInfoRef),
    Module(ModuleId),
    Error(TypeErrorRef, Span),
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::TypePathExpr> for ModuleItem {
    fn from_parsed(path: mollie_parser::TypePathExpr, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, _span: Span) -> Self {
        let mut span = None;
        let mut result = Self::SubModule(ast.module);

        for segment in path.segments {
            match result {
                Self::SubModule(current_module) => {
                    if let Some(item) = context.solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                        result = *item;

                        span.replace(segment.span);
                    } else {
                        println!("error: there's no item called `{}` in module", segment.value.name.value);
                    }
                }
                result => {
                    let found = match result {
                        Self::SubModule(_) => TypeErrorValue::Module,
                        Self::Adt(adt_ref) => TypeErrorValue::Adt(SpecialAdtKind::Specific(context.solver.context.adt_types[adt_ref].kind)),
                        Self::Trait(_) => TypeErrorValue::Trait,
                        Self::Func(_) | Self::Intrinsic(..) => TypeErrorValue::Function,
                    };

                    match span {
                        Some(span) => {
                            context.solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::Module,
                                    found,
                                },
                                span,
                            );
                        }
                        None => todo!(),
                    }

                    break;
                }
            }
        }

        result
    }
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::TypePathExpr> for TypePathResult {
    fn from_parsed(path: mollie_parser::TypePathExpr, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, _span: Span) -> Self {
        let mut result = Self::Module(ast.module);
        let segment_count = path.segments.len();

        for (i, segment) in path.segments.into_iter().enumerate() {
            if let Some((typo, _)) = context.solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = Self::Generic(typo);

                break;
            }

            match result {
                Self::Adt(adt, type_args, None) => {
                    if let Some(variant) = context.solver.context.adt_types[adt].variants.iter().find_map(|(variant_ref, variant)| {
                        if variant.name.as_deref() == Some(segment.value.name.value.0.as_str()) {
                            Some(variant_ref)
                        } else {
                            None
                        }
                    }) {
                        result = Self::Adt(adt, type_args, Some(variant));
                    } else {
                        let storage_type_args = type_args.iter().map(|&type_arg| context.solver.solve(type_arg)).collect();
                        let storage_ty = context.solver.context.types.get_or_add(Type::Adt(adt, storage_type_args));

                        if let Some((vtable, vfunc)) = context.solver.context.find_vtable_by_func(storage_ty, &segment.value.name.value.0) {
                            result = Self::VFunc(adt, type_args, vtable, vfunc);
                        } else {
                            result = Self::Adt(adt, type_args, None);
                        }
                    }
                }
                Self::Module(current_module) => {
                    if let Some(item) = context.solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                        match *item {
                            ModuleItem::SubModule(module_id) => result = Self::Module(module_id),
                            ModuleItem::Adt(adt_ref) => {
                                let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                    type_args
                                        .value
                                        .0
                                        .into_iter()
                                        .map(|arg| TypeInfo::from_parsed(arg.value, ast, context, arg.span))
                                        .collect()
                                });

                                let args = (0..context.solver.context.adt_types[adt_ref].generics)
                                    .rev()
                                    .map(|generic| type_args.pop().unwrap_or_else(|| context.solver.add_info(TypeInfo::Generic(generic), None)))
                                    .rev()
                                    .collect();

                                result = Self::Adt(adt_ref, args, None);
                            }
                            ModuleItem::Trait(trait_ref) => {
                                let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                    type_args
                                        .value
                                        .0
                                        .into_iter()
                                        .map(|arg| TypeInfo::from_parsed(arg.value, ast, context, arg.span))
                                        .collect()
                                });

                                let args = (0..context.solver.context.traits[trait_ref].generics)
                                    .rev()
                                    .map(|generic| type_args.pop().unwrap_or_else(|| context.solver.add_info(TypeInfo::Generic(generic), None)))
                                    .rev()
                                    .collect();

                                result = Self::Trait(trait_ref, args);
                            }
                            ModuleItem::Func(func_ref) => {
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

                                // ast.use_item(UsedItem::Func(func_ref));

                                result = Self::Func(func_ref);
                            }
                            ModuleItem::Intrinsic(kind, _) => {
                                result = Self::Intrinsic(kind);
                            }
                        }
                    } else {
                        if (segment_count - 1 - i) > 0 {
                            result = Self::Error(
                                context.solver.context.error(
                                    TypeError::NotFound {
                                        name: segment.value.name.value.0,
                                        was_looking_for: LookupType::Module { inside: current_module },
                                    },
                                    segment.value.name.span,
                                ),
                                segment.value.name.span,
                            );
                        } else {
                            result = Self::Error(
                                context.solver.context.error(
                                    TypeError::NotFound {
                                        name: segment.value.name.value.0,
                                        was_looking_for: LookupType::Type { inside: current_module },
                                    },
                                    segment.value.name.span,
                                ),
                                segment.value.name.span,
                            );
                        }

                        break;
                    }
                }
                _ => (),
            }
        }

        // if let &TypePath::Adt(info_ref, ..) = &result
        //     && let TypeInfo::Adt(ty, _, args) = checker.solver.get_info(info_ref)
        // {
        //     ast.use_item(UsedItem::Adt(*ty, args.clone()));
        // }

        result
    }
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::TypePathExpr, TypeInfoRef> for TypeInfo {
    fn from_parsed(path: mollie_parser::TypePathExpr, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, _span: Span) -> TypeInfoRef {
        enum TypePathResult {
            Type(TypeInfoRef),
            Module(ModuleId),
        }

        let mut span = None;
        let mut result = TypePathResult::Module(ast.module);

        for segment in path.segments {
            if let Some((typo, _)) = context.solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let TypePathResult::Module(current_module) = result {
                if let Some(item) = context.solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                    span.replace(segment.span);

                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed(arg.value, ast, context, arg.span))
                                    .collect()
                            });

                            let args = (0..context.solver.context.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| context.solver.add_info(Self::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(context.solver.add_info(Self::Adt(adt_ref, args), None));
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed(arg.value, ast, context, arg.span))
                                    .collect()
                            });

                            let args = (0..context.solver.context.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| context.solver.add_info(Self::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(context.solver.add_info(Self::Trait(trait_ref, args), None));
                        }
                        ModuleItem::Func(func) => {
                            // if let TypeInfo::Func(args, returns) =
                            // checker.solver.get_info(checker.
                            // local_functions[func_ref].ty) {
                            //     for arg in args {
                            //         if let TypeInfo::Adt(adt_ref, _, args) =
                            // checker.solver.get_info(arg.inner()) {
                            //             ast.use_item(UsedItem::Adt(*adt_ref,
                            // args.clone()));
                            //         }
                            //     }

                            //     if let TypeInfo::Adt(adt_ref, _, args) =
                            // checker.solver.get_info(*returns) {         ast.
                            // use_item(UsedItem::Adt(*adt_ref, args.clone()));
                            //     }
                            // }

                            // ast.use_item(UsedItem::Func(func_ref));

                            result = TypePathResult::Type(TypeSolver::type_to_info(
                                &mut context.solver.type_infos,
                                context.solver.context,
                                context.solver.context.functions[func].ty,
                                &[],
                            ));
                        }
                        ModuleItem::Intrinsic(..) => {
                            // result = Self::Intrinsic(kind);
                        }
                    }
                } else {
                    context.solver.context.error(
                        TypeError::NotFound {
                            name: segment.value.name.value.0,
                            was_looking_for: LookupType::Type { inside: current_module },
                        },
                        segment.value.name.span,
                    );

                    return context.solver.add_info(Self::Error, None);
                }
            }
        }

        match result {
            TypePathResult::Type(type_info_ref) => type_info_ref,
            TypePathResult::Module(_) => {
                match span {
                    Some(span) => {
                        context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Type,
                                found: TypeErrorValue::Module,
                            },
                            span,
                        );
                    }
                    None => todo!(),
                }

                context.solver.add_info(Self::Error, None)
            }
        }
    }
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::Type, TypeInfoRef> for TypeInfo {
    fn from_parsed(ty: mollie_parser::Type, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, span: Span) -> TypeInfoRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => context.solver.add_info(Self::Primitive(primitive_type), Some(span)),
            mollie_parser::Type::Array(element, size) => {
                let element = Self::from_parsed(element.value, ast, context, element.span);

                context.solver.add_info(Self::Array(element, size.map(|size| size.value)), Some(span))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed(arg.value, ast, context, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed(ty.value, ast, context, ty.span),
                    None => context.solver.add_info(Self::Primitive(PrimitiveType::Void), Some(span)),
                };

                context.solver.add_info(Self::Func(args, returns), Some(span))
            }
            mollie_parser::Type::Path(type_path_expr) => Self::from_parsed(type_path_expr, ast, context, span),
        }
    }
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::TypePathExpr, TypeRef> for Type {
    fn from_parsed(path: mollie_parser::TypePathExpr, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, _span: Span) -> TypeRef {
        enum TypePathResult {
            Type(TypeRef),
            Module(ModuleId),
        }

        let mut span = None;
        let mut result = TypePathResult::Module(ast.module);

        for segment in path.segments {
            if let Some((_, typo)) = context.solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let &TypePathResult::Module(current_module) = &result {
                if let Some(item) = context.solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                    span.replace(segment.span);

                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed(arg.value, ast, context, arg.span))
                                    .collect()
                            });

                            let args = (0..context.solver.context.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| {
                                    type_args
                                        .pop()
                                        .unwrap_or_else(|| context.solver.context.types.get_or_add(Self::Generic(generic)))
                                })
                                .rev()
                                .collect();

                            result = TypePathResult::Type(context.solver.context.types.get_or_add(Self::Adt(adt_ref, args)));
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed(arg.value, ast, context, arg.span))
                                    .collect()
                            });

                            let args = (0..context.solver.context.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| {
                                    type_args
                                        .pop()
                                        .unwrap_or_else(|| context.solver.context.types.get_or_add(Self::Generic(generic)))
                                })
                                .rev()
                                .collect();

                            result = TypePathResult::Type(context.solver.context.types.get_or_add(Self::Trait(trait_ref, args)));
                        }
                        ModuleItem::Func(func_ref) => {
                            // if let TypeInfo::Func(args, returns) =
                            // checker.solver.get_info(checker.
                            // local_functions[func_ref].ty) {
                            //     for arg in args {
                            //         if let TypeInfo::Adt(adt_ref, _,
                            // args) = checker.solver.get_info(arg.
                            // inner()) {
                            //
                            // ast.use_item(UsedItem::Adt(*adt_ref,
                            // args.clone()));
                            //         }
                            //     }

                            //     if let TypeInfo::Adt(adt_ref, _,
                            // args) = checker.solver.get_info(*returns)
                            // {
                            //         ast.use_item(UsedItem::Adt(*
                            // adt_ref, args.clone()));
                            //     }
                            // }

                            // ast.use_item(UsedItem::Func(func_ref));

                            result = TypePathResult::Type(context.solver.context.functions[func_ref].ty);
                        }
                        ModuleItem::Intrinsic(..) => {
                            // result = TypePath::Intrinsic(kind, ty);
                        }
                    }
                } else {
                    context.solver.context.error(
                        TypeError::NotFound {
                            name: segment.value.name.value.0,
                            was_looking_for: LookupType::Type { inside: current_module },
                        },
                        segment.value.name.span,
                    );

                    return context.solver.context.types.get_or_add(Self::Error);
                }
            }
        }

        // if let &TypePath::Adt(info_ref, ..) = &result
        //     && let TypeInfo::Adt(ty, _, args) = checker.solver.get_info(info_ref)
        // {
        //     ast.use_item(UsedItem::Adt(*ty, args.clone()));
        // }

        match result {
            TypePathResult::Type(type_ref) => type_ref,
            TypePathResult::Module(_) => {
                match span {
                    Some(span) => {
                        context.solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Type,
                                found: TypeErrorValue::Module,
                            },
                            span,
                        );
                    }
                    None => todo!(),
                }

                context.solver.context.types.get_or_add(Self::Error)
            }
        }
    }
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::Type, TypeRef> for Type {
    fn from_parsed(ty: mollie_parser::Type, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, span: Span) -> TypeRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => context.solver.context.types.get_or_add(Self::Primitive(primitive_type)),
            mollie_parser::Type::Array(element, size) => {
                let element = Self::from_parsed(element.value, ast, context, element.span);

                context.solver.context.types.get_or_add(Self::Array(element, size.map(|size| size.value)))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed(arg.value, ast, context, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed(ty.value, ast, context, ty.span),
                    None => context.solver.context.types.get_or_add(Self::Primitive(PrimitiveType::Void)),
                };

                context.solver.context.types.get_or_add(Self::Func(args, returns))
            }
            mollie_parser::Type::Path(type_path_expr) => Self::from_parsed(type_path_expr, ast, context, span),
        }
    }
}
