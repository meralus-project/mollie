use std::iter;

use mollie_index::{Idx, IdxEnumerate, IndexVec};
use mollie_shared::Span;
use mollie_typing::{
    Adt, AdtKind, AdtRef, AdtVariant, AdtVariantField, AdtVariantRef, Arg, ArgType, Func, FuncRef, LangItemValue, ModuleItem, PrimitiveType, Trait, TraitFunc,
    Type, TypeError, TypeInfo, TypeSolver, UIntType, VTableFunc, VTableGenerator,
};

use crate::{
    ConstantContext, FirstPass, FromParsed, FunctionBody, IntoConstVal, ModuleLoader, TypedAST, TypedASTContextRef, UsedItem,
    block::Block,
    expr::{Expr, ExprRef},
};

mollie_index::new_idx_type!(StmtRef);

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprRef),
    NewVar { mutable: bool, name: String, value: ExprRef },
}

impl<E, M: ModuleLoader<E>> FromParsed<E, M, mollie_parser::Stmt, Option<StmtRef>> for Stmt {
    fn from_parsed(stmt: mollie_parser::Stmt, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E, M>, span: Span) -> Option<StmtRef> {
        match stmt {
            mollie_parser::Stmt::Expression(expr) => {
                let expr = Expr::from_parsed(expr, ast, context, span);

                Some(ast.add_stmt(Self::Expr(expr)))
            }
            mollie_parser::Stmt::VariableDecl(variable_decl) => {
                let value = Expr::from_parsed(variable_decl.value.value, ast, context, variable_decl.value.span);

                context.solver.set_var(&variable_decl.name.value.0, ast[value].ty);

                Some(ast.add_stmt(Self::NewVar {
                    mutable: variable_decl.mutable.is_some(),
                    name: variable_decl.name.value.0,
                    value,
                }))
            }
            mollie_parser::Stmt::StructDecl(struct_decl) => {
                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    let ty_info = context.solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = context.solver.context.types.get_or_add(Type::Generic(index));

                    context.solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let mut properties = IndexVec::new();

                for property in struct_decl.properties.value {
                    let name = property.value.name.value.0;
                    let ty = Type::from_parsed(property.value.ty.value, ast, context, property.value.ty.span);
                    let default_value = match property.value.default_value {
                        Some(value) => {
                            let mut ast = TypedAST::default();
                            let mut default_context = context.fork();
                            let value = Expr::from_parsed(value.value, &mut ast, &mut default_context, value.span);
                            let expected_ty = TypeSolver::type_to_info(&mut default_context.solver.type_infos, default_context.solver.context, ty, &[]);

                            default_context.solver.unify(ast[value].ty, expected_ty);

                            let (solved, expr) = ast.solve_expr_final(value, &mut default_context);

                            expr.into_const_val(&solved, default_context.solver.context, &mut ConstantContext::default())
                                .map_or_else(
                                    |()| {
                                        context.solver.context.error(TypeError::NonConstantEvaluable, solved[expr].span);

                                        None
                                    },
                                    Some,
                                )
                        }
                        None => None,
                    };

                    properties.push(AdtVariantField { name, ty, default_value });
                }

                variants.push(AdtVariant {
                    name: None,
                    discriminant: 0,
                    fields: properties.into_boxed_slice(),
                });

                for name in &struct_decl.name.value.generics {
                    context.solver.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(context.solver.context.adt_types.len());

                context.solver.context.modules[ast.module]
                    .items
                    .insert(struct_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                context.solver.context.adt_types.push(Adt {
                    name: Some(struct_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Struct,
                    generics: struct_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::ViewDecl(view_decl) => {
                for (index, name) in view_decl.name.value.generics.iter().enumerate() {
                    let ty_info = context.solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = context.solver.context.types.get_or_add(Type::Generic(index));

                    context.solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let mut fields = IndexVec::with_capacity(view_decl.properties.len());

                for property in view_decl.properties {
                    let name = property.value.name.value.0;
                    let ty = Type::from_parsed(property.value.ty.value, ast, context, property.value.ty.span);
                    let default_value = match property.value.default_value {
                        Some(value) => {
                            let mut ast = TypedAST::default();
                            let mut default_context = context.fork();
                            let value = Expr::from_parsed(value.value, &mut ast, &mut default_context, value.span);
                            let expected_ty = TypeSolver::type_to_info(&mut default_context.solver.type_infos, default_context.solver.context, ty, &[]);

                            default_context.solver.unify(ast[value].ty, expected_ty);

                            let (solved, expr) = ast.solve_expr_final(value, &mut default_context);

                            expr.into_const_val(&solved, default_context.solver.context, &mut ConstantContext::default())
                                .map_or_else(
                                    |()| {
                                        context.solver.context.error(TypeError::NonConstantEvaluable, solved[expr].span);

                                        None
                                    },
                                    Some,
                                )
                        }
                        None => None,
                    };

                    fields.push(AdtVariantField { name, ty, default_value });
                }

                variants.push(AdtVariant {
                    name: None,
                    discriminant: 0,
                    fields: fields.into_boxed_slice(),
                });

                for name in &view_decl.name.value.generics {
                    context.solver.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(context.solver.context.adt_types.len());

                context.solver.context.modules[ast.module]
                    .items
                    .insert(view_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                context.solver.context.adt_types.push(Adt {
                    name: Some(view_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::View,
                    generics: view_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::TraitDecl(trait_decl) => {
                let trait_ref = context.solver.context.traits.next_index();

                let this = {
                    let ty_info = context.solver.add_info(TypeInfo::Generic(0), None);
                    let ty = context.solver.context.types.get_or_add(Type::Generic(0));

                    context.solver.available_generics.insert(String::from("<Self>"), (ty_info, ty));

                    (ty_info, ty)
                };

                for (index, name) in trait_decl.name.value.generics.iter().enumerate() {
                    let index = index + 1;
                    let ty_info = context.solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = context.solver.context.types.get_or_add(Type::Generic(index));

                    context.solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut functions = IndexVec::with_capacity(trait_decl.functions.value.len());

                for (func_ref, function) in trait_decl.functions.value.into_iter().enumerate_idx() {
                    if let Some(item) = function.value.attributes.iter().find_map(|attribute| {
                        attribute.value.value.as_ref().and_then(|v| {
                            if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                                Some(item)
                            } else {
                                None
                            }
                        })
                    }) {
                        context
                            .solver
                            .context
                            .language_items
                            .insert(item, LangItemValue::TraitFunc(trait_ref, func_ref));
                    }

                    let name = function.value.name.value.0;
                    let mut args = Vec::with_capacity(function.value.args.len() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        args.push(Arg {
                            name: String::from("self"),
                            kind: ArgType::This,
                            ty: this.1,
                        });
                    }

                    for arg in function.value.args {
                        let name = arg.value.name.value.0;
                        let ty = Type::from_parsed(arg.value.ty.value, ast, context, arg.value.ty.span);

                        args.push(Arg {
                            name,
                            kind: ArgType::Regular,
                            ty,
                        });
                    }

                    let args = args.into_boxed_slice();
                    let returns = match function.value.returns {
                        Some(returns) => Type::from_parsed(returns.value, ast, context, returns.span),
                        None => context.solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void)),
                    };

                    functions.push(TraitFunc { name, args, returns });
                }

                for name in &trait_decl.name.value.generics {
                    context.solver.available_generics.remove(&name.value.0);
                }

                context.solver.available_generics.remove("<Self>");

                if let Some(item) = trait_decl.attributes.iter().find_map(|attribute| {
                    attribute.value.value.as_ref().and_then(|v| {
                        if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                            Some(item)
                        } else {
                            None
                        }
                    })
                }) {
                    context.solver.context.language_items.insert(item, LangItemValue::Trait(trait_ref));
                }

                context.solver.context.traits.push(Trait {
                    name: trait_decl.name.value.name.value.0.clone(),
                    generics: trait_decl.name.value.generics.len(),
                    functions,
                });

                context.solver.context.modules[ast.module]
                    .items
                    .insert(trait_decl.name.value.name.value.0, ModuleItem::Trait(trait_ref));

                None
            }
            mollie_parser::Stmt::EnumDecl(enum_decl) => {
                let adt_ref = AdtRef::new(context.solver.context.adt_types.len());

                if let Some(item) = enum_decl.attributes.iter().find_map(|attribute| {
                    attribute.value.value.as_ref().and_then(|v| {
                        if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                            Some(item)
                        } else {
                            None
                        }
                    })
                }) {
                    context.solver.context.language_items.insert(item, LangItemValue::Adt(adt_ref));
                }

                for (index, name) in enum_decl.name.value.generics.iter().enumerate() {
                    let ty_info = context.solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = context.solver.context.types.get_or_add(Type::Generic(index));

                    context.solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let usize = context.solver.context.types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize)));

                for (discriminant, variant) in enum_decl.variants.value.into_iter().enumerate() {
                    let name = Some(variant.value.name.value.0);
                    let mut fields = IndexVec::with_capacity(variant.value.properties.as_ref().map(|value| value.value.len()).unwrap_or_default());

                    fields.push(AdtVariantField {
                        name: String::from("<discriminant>"),
                        ty: usize,
                        default_value: None,
                    });

                    if let Some(properties) = variant.value.properties {
                        for property in properties.value {
                            let name = property.value.name.value.0;
                            let ty = Type::from_parsed(property.value.ty.value, ast, context, property.value.ty.span);

                            fields.push(AdtVariantField { name, ty, default_value: None });
                        }
                    }

                    if let Some(item) = variant.value.attributes.iter().find_map(|attribute| {
                        attribute.value.value.as_ref().and_then(|v| {
                            if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                                Some(item)
                            } else {
                                None
                            }
                        })
                    }) {
                        context
                            .solver
                            .context
                            .language_items
                            .insert(item, LangItemValue::AdtVariant(adt_ref, AdtVariantRef::new(discriminant)));
                    }

                    variants.push(AdtVariant {
                        name,
                        discriminant,
                        fields: fields.into_boxed_slice(),
                    });
                }

                for name in &enum_decl.name.value.generics {
                    context.solver.available_generics.remove(&name.value.0);
                }

                context.solver.context.modules[ast.module]
                    .items
                    .insert(enum_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                context.solver.context.adt_types.push(Adt {
                    name: Some(enum_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Enum,
                    generics: enum_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::FuncDecl(func_decl) => {
                let mut func_context = context.fork();

                let postfix = func_decl
                    .modifiers
                    .iter()
                    .any(|modifier| matches!(modifier.value, mollie_parser::FuncModifier::Postfix));

                let mut arg_names = Vec::with_capacity(func_decl.args.value.capacity());
                let mut args = Vec::with_capacity(func_decl.args.value.capacity());
                // let mut arg_spans = Vec::with_capacity(func_decl.args.value.capacity());

                let mut ast = TypedAST {
                    module: ast.module,
                    ..TypedAST::default()
                };

                for arg in func_decl.args.value {
                    let ty = Type::from_parsed(arg.value.ty.value, &mut ast, &mut func_context, arg.value.ty.span);
                    let type_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                    if let Type::Adt(adt_ref, adt_args) = &func_context.solver.context.types[ty] {
                        let item = UsedItem::Adt(*adt_ref, adt_args.clone());

                        ast.use_item(
                            &func_context.solver.context.adt_types,
                            func_context.vtables,
                            func_context.functions,
                            &mut func_context.solver.context.types,
                            item,
                            None,
                        );
                    }

                    func_context.solver.set_var(&arg.value.name.value, type_info);

                    arg_names.push(arg.value.name.value.0);
                    // arg_spans.push(arg.span);
                    args.push(ty);
                }

                if postfix {
                    // let mut reasons = Vec::new();

                    // if args.is_empty() {
                    //     reasons.push(func_decl.args.span.
                    // wrap(PostfixRequirement::OneArgument));
                    // } else {
                    //     if !checker.solver.get_info(args[0].inner()).
                    // is_number() {         reasons.
                    // push(arg_spans[0].
                    // wrap(PostfixRequirement::ArgumentType));
                    //     }

                    //     if args.len() > 1 {
                    //         let first = arg_spans[1];

                    //         reasons.push(
                    //             arg_spans
                    //                 .into_iter()
                    //                 .skip(2)
                    //                 .fold(first, |p, c| p.between(c))
                    //
                    // .wrap(PostfixRequirement::OnlyOneArgument),
                    //         );
                    //     }
                    // }

                    // if !reasons.is_empty() {
                    //     checker.add_error(TypeError::InvalidPostfixFunction {
                    // reasons }, span.between(func_decl.args.span));
                    // }
                }

                let returns = if let Some(returns) = func_decl.returns {
                    Type::from_parsed(returns.value, &mut ast, &mut func_context, returns.span)
                } else {
                    func_context.solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                };

                let returns_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, returns, &[]);
                let body = Block::from_parsed(func_decl.body.value, &mut ast, &mut func_context, func_decl.body.span);

                func_context.solver.unify(ast[body].ty, returns_info);

                let (ast, entry) = ast.solve(body, &mut func_context, returns);

                let ty = context.solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));
                let func_ref = FuncRef::new(context.solver.context.functions.len());

                context.solver.context.modules[ast.module]
                    .items
                    .insert(func_decl.name.value.0.clone(), ModuleItem::Func(func_ref));

                context.functions.push(FunctionBody::Local { ast, entry });
                context.solver.context.functions.push(Func {
                    postfix,
                    name: func_decl.name.value.0,
                    arg_names,
                    ty,
                    // kind: VTableFuncKind::Local { ast, entry },
                });

                None
            }
            mollie_parser::Stmt::Impl(mut implementation) => {
                // let generics = std::iter::once_with(|| {
                //     if implementation.trait_name.is_some() {
                //         Some((String::from("<Self>"), None))
                //     } else {
                //         None
                //     }
                // })
                // .flatten()
                // .chain(implementation.generics.iter().map(|name| (name.value.0.clone(),
                // Some(name.span)))) .enumerate()
                // .map(|(index, (name, span))|
                // solver.context.types.get_or_add(Type::Generic(index)))
                // .collect::<Box<[_]>>();
                let generics = (0..(implementation.generics.len() + usize::from(implementation.trait_name.is_some())))
                    .map(|index| context.solver.context.types.get_or_add(Type::Generic(index)))
                    .collect();

                let (trait_name, origin_trait) = match implementation.trait_name {
                    Some(trait_name) => {
                        if let ModuleItem::Trait(trait_ref) = ModuleItem::from_parsed(trait_name.value, ast, context, trait_name.span) {
                            (Some(context.solver.context.traits[trait_ref].name.clone()), Some(trait_ref))
                        } else {
                            (None, None)
                        }
                    }
                    None => (None, None),
                };

                for (index, (name, _)) in iter::once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                    .flatten()
                    .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                    .enumerate()
                {
                    let ty = context.solver.context.types.get_or_add(Type::Generic(index));
                    let ty_info = TypeSolver::type_to_info(&mut context.solver.type_infos, context.solver.context, ty, &[]);

                    context.solver.available_generics.insert(name, (ty_info, ty));
                }

                let ty = Type::from_parsed(implementation.target.value, ast, context, implementation.target.span);

                let vtable = if let Some(vtable) = context.solver.context.get_vtable(ty, origin_trait) {
                    vtable
                } else {
                    context.vtables.push(IndexVec::new());
                    context.solver.context.vtables.insert(VTableGenerator {
                        ty,
                        generics,
                        origin_trait,
                        functions: IndexVec::new(),
                    })
                };

                if let (Some(trait_name), Some(trait_ref)) = (&trait_name, origin_trait) {
                    let trait_functions = context.solver.context.traits[trait_ref]
                        .functions
                        .iter()
                        .map(|(k, func)| (k, func.name.clone()))
                        .collect::<Box<[_]>>();

                    for (func_ref, name) in trait_functions {
                        if let Some(func) = implementation.functions.value.iter().position(|func| func.value.name.value.0 == name) {
                            let function = implementation.functions.value.remove(func);
                            let mut func_context = context.fork();
                            let mut ast = TypedAST {
                                module: ast.module,
                                ..TypedAST::default()
                            };

                            for (index, (name, _)) in iter::once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                                .flatten()
                                .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                                .enumerate()
                            {
                                let ty = func_context.solver.context.types.get_or_add(Type::Generic(index));
                                let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                                func_context.solver.available_generics.insert(name, (ty_info, ty));
                            }

                            let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                            let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                            if function.value.this.is_some() {
                                let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                                func_context.solver.set_var("self", ty_info);

                                arg_names.push("self".to_string());
                                args.push(ty);
                            }

                            for arg in function.value.args {
                                let ty = Type::from_parsed(arg.value.ty.value, &mut ast, &mut func_context, arg.value.ty.span);
                                let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                                func_context.solver.set_var(&arg.value.name.value.0, ty_info);

                                if let Type::Adt(adt_ref, adt_args) = &func_context.solver.context.types[ty] {
                                    let item = UsedItem::Adt(*adt_ref, adt_args.clone());

                                    ast.use_item(
                                        &func_context.solver.context.adt_types,
                                        func_context.vtables,
                                        func_context.functions,
                                        &mut func_context.solver.context.types,
                                        item,
                                        None,
                                    );
                                }

                                arg_names.push(arg.value.name.value.0);
                                args.push(ty);
                            }

                            let returns = if let Some(returns) = function.value.returns {
                                Type::from_parsed(returns.value, &mut ast, &mut func_context, returns.span)
                            } else {
                                func_context.solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                            };

                            let returns_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, returns, &[]);
                            let body = Block::from_parsed(function.value.body.value, &mut ast, &mut func_context, function.value.body.span);

                            func_context.solver.unify(ast[body].ty, returns_info);

                            let ty = func_context.solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                            let (ast, entry) = ast.solve(body, &mut func_context, returns);

                            context.vtables[vtable].push(FunctionBody::Local { ast, entry });
                            context.solver.context.vtables[vtable].functions.push(VTableFunc {
                                trait_func: Some(func_ref),
                                name: function.value.name.value.0,
                                arg_names,
                                ty,
                                // kind: VTableFuncKind::Local { ast, entry },
                            });
                        } else {
                            println!("didn't found implementation for {trait_name}::{name}");
                        }
                    }
                }

                for function in implementation.functions.value {
                    if let Some(trait_name) = &trait_name {
                        println!("there's no function called {} in {trait_name}", function.value.name.value.0);
                    }

                    let mut func_context = context.fork();
                    let mut ast = TypedAST {
                        module: ast.module,
                        ..TypedAST::default()
                    };

                    for (index, (name, _)) in iter::once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                        .flatten()
                        .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                        .enumerate()
                    {
                        let ty = func_context.solver.context.types.get_or_add(Type::Generic(index));
                        let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                        func_context.solver.available_generics.insert(name, (ty_info, ty));
                    }

                    let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                    let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                        func_context.solver.set_var("self", ty_info);

                        arg_names.push("self".to_string());
                        args.push(ty);
                    }

                    for arg in function.value.args {
                        let ty = Type::from_parsed(arg.value.ty.value, &mut ast, &mut func_context, arg.value.ty.span);
                        let ty_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, ty, &[]);

                        func_context.solver.set_var(&arg.value.name.value.0, ty_info);

                        if let Type::Adt(adt_ref, adt_args) = &func_context.solver.context.types[ty] {
                            let item = UsedItem::Adt(*adt_ref, adt_args.clone());

                            ast.use_item(
                                &func_context.solver.context.adt_types,
                                func_context.vtables,
                                func_context.functions,
                                &mut func_context.solver.context.types,
                                item,
                                None,
                            );
                        }

                        arg_names.push(arg.value.name.value.0);
                        args.push(ty);
                    }

                    let returns = if let Some(returns) = function.value.returns {
                        Type::from_parsed(returns.value, &mut ast, &mut func_context, returns.span)
                    } else {
                        func_context.solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                    };

                    let returns_info = TypeSolver::type_to_info(&mut func_context.solver.type_infos, func_context.solver.context, returns, &[]);
                    let body = Block::from_parsed(function.value.body.value, &mut ast, &mut func_context, function.value.body.span);

                    func_context.solver.unify(ast[body].ty, returns_info);

                    let ty = func_context.solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                    let (ast, entry) = ast.solve(body, &mut func_context, returns);

                    context.vtables[vtable].push(FunctionBody::Local { ast, entry });
                    context.solver.context.vtables[vtable].functions.push(VTableFunc {
                        trait_func: None,
                        name: function.value.name.value.0,
                        arg_names,
                        ty,
                        // kind: VTableFuncKind::Local { ast, entry },
                    });
                }

                for generic in implementation.generics {
                    context.solver.available_generics.remove(&generic.value.0);
                }

                if origin_trait.is_some() {
                    context.solver.available_generics.remove("<Self>");
                }

                None
            }
            mollie_parser::Stmt::Import(import) => {
                // let _path_span = import.path.span;
                let path = ModuleItem::from_parsed(import.path.value, ast, context, import.path.span);

                #[allow(clippy::single_match)]
                match path {
                    ModuleItem::SubModule(module) => match import.kind {
                        mollie_parser::ImportKind::Partial(items) => {
                            for item in items.value {
                                let name = item.value.0;

                                if let Some(item) = context.solver.context.modules[module].items.get(&name).copied() {
                                    context.solver.context.modules[ast.module].items.insert(name, item);
                                }
                            }
                        }
                        mollie_parser::ImportKind::Named => {
                            let name = context.solver.context.modules[module].name.clone();

                            context.solver.context.modules[ast.module].items.insert(name, ModuleItem::SubModule(module));
                        }
                    },
                    _ => todo!("error or value depending on import"),
                }

                None
            }
            mollie_parser::Stmt::Module(module) => {
                let module = context.solver.context.register_module_in_module(ast.module, module.name.value);

                M::load_module(context.fork(), module);

                None
            }
        }
    }
}
