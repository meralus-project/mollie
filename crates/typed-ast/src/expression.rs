use std::{iter, mem};

use indexmap::map::Entry;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec};
use mollie_parser::{IndexTarget, TypePattern};
use mollie_shared::{Operator, Span};
use mollie_typing::{
    Adt, AdtKind, AdtRef, AdtVariant, AdtVariantRef, FieldRef, FieldType, FuncArg, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, UIntType, VFuncRef,
    VTableRef, Variable,
};
use serde::Serialize;

use crate::{
    BlockRef, CastedType, ConstantContext, ExprRef, Func, FuncRef, IntoConstantValue, IntoPositionedTypedAST, IntoTypedAST, InvalidTypePathSegmentReason,
    ModuleId, ModuleItem, NonConstructable, NotFunction, NotModule, PostfixRequirement, StmtRef, Trait, TraitFuncRef, TypeChecker, TypeError, TypeErrorRef,
    TypeInfoCastError, TypedAST, VTableFunc, VTableFuncKind, VTableGenerator, statement::Stmt,
};

#[derive(Debug, Serialize)]
pub enum LiteralExpr {
    Integer(i64),
    Float(f32),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum VFunc {
    Known(VTableRef, VFuncRef),
    Unknown(TraitRef, TraitFuncRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum FuncSource {
    Expr(ExprRef),
    Explicit(FuncRef),
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum Expr {
    Literal(LiteralExpr),
    If {
        condition: ExprRef,
        /// Always [`Expr::Block`]
        block: BlockRef,
        /// Always [`Expr::If`] or [`Expr::Block`]
        else_block: Option<ExprRef>,
    },
    Block(BlockRef),
    Var(String),
    Access {
        target: ExprRef,
        field: FieldRef,
    },
    VTableAccess {
        target: ExprRef,
        func: VFunc,
    },
    Index {
        target: ExprRef,
        index: ExprRef,
    },
    While {
        condition: ExprRef,
        block: BlockRef,
    },
    Array(Box<[ExprRef]>),
    Binary {
        operator: Operator,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    Call {
        func: FuncSource,
        args: Box<[ExprRef]>,
    },
    Closure {
        args: Box<[String]>,
        captures: Box<[(String, Variable)]>,
        body: BlockRef,
    },
    Construct {
        ty: TypeInfoRef,
        variant: AdtVariantRef,
        fields: Box<[(FieldRef, String, Option<ExprRef>)]>,
    },
    IsPattern {
        target: ExprRef,
        pattern: IsPattern,
    },
    TypeIndex {
        ty: TypeInfoRef,
        path: TypePath,
    },
    Error(TypeErrorRef),
    Nothing,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum IsPattern {
    Literal(ExprRef),
    EnumVariant {
        target: AdtRef,
        target_args: Box<[TypeInfoRef]>,
        variant: AdtVariantRef,
        values: Box<[(FieldRef, String, Option<Self>)]>,
    },
    TypeName {
        ty: TypeInfoRef,
        name: String,
    },
}

impl IntoTypedAST<ExprRef> for mollie_parser::LiteralExpr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> ExprRef {
        use mollie_parser::{
            LiteralExpr::{Boolean, Number, String},
            Number::{F32, I64},
        };

        let (literal, ty) = match self {
            Number(I64(value), postfix) => (LiteralExpr::Integer(value), match postfix.as_deref() {
                Some("uint_size") => checker.core_types.uint_size,
                Some("uint64") => checker.core_types.uint64,
                Some("uint32") => checker.core_types.uint32,
                Some("uint16") => checker.core_types.uint16,
                Some("uint8") => checker.core_types.uint8,
                Some("int_size") => checker.core_types.int_size,
                Some("int64") => checker.core_types.int64,
                Some("int32") => checker.core_types.int32,
                Some("int16") => checker.core_types.int16,
                Some("int8") => checker.core_types.int8,
                Some(postfix) => {
                    if let Some(func_ref) = checker.modules[ModuleId::ZERO]
                        .items
                        .get(postfix)
                        .and_then(|item| if let &ModuleItem::Func(func) = item { Some(func) } else { None })
                    {
                        let func = &checker.local_functions[func_ref];

                        if func.postfix {
                            let TypeInfo::Func(args, returns) = checker.solver.get_info(func.ty) else {
                                return ast.add_error_expr(
                                    checker.add_error(
                                        TypeError::ExpectedFunction {
                                            found: NotFunction::Type(func.ty),
                                        },
                                        span,
                                    ),
                                    checker.solver.add_info(TypeInfo::Unknown(None)),
                                    span,
                                );
                            };

                            let expr = ast.add_expr(Expr::Literal(LiteralExpr::Integer(value)), args[0].inner(), span);

                            if let TypeInfo::Func(args, returns) = checker.solver.get_info(checker.local_functions[func_ref].ty) {
                                for arg in args {
                                    if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(arg.inner()) {
                                        ast.used_adt_types.push((*adt_ref, args.clone()));
                                    }
                                }

                                if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(*returns) {
                                    ast.used_adt_types.push((*adt_ref, args.clone()));
                                }
                            }

                            ast.used_functions.push(func_ref);

                            return ast.add_expr(
                                Expr::Call {
                                    func: FuncSource::Explicit(func_ref),
                                    args: Box::new([expr]),
                                },
                                *returns,
                                span,
                            );
                        }

                        let number_length = (span.end - span.start) - postfix.len();
                        let span = Span {
                            start: span.start + number_length,
                            end: span.end,
                            line: span.line,
                            column: span.column + number_length,
                        };

                        return ast.add_error_expr(
                            checker.add_error(TypeError::NotPostfix { name: func.name.clone() }, span),
                            checker.solver.add_info(TypeInfo::Unknown(None)),
                            span,
                        );
                    }

                    let number_length = (span.end - span.start) - postfix.len();
                    let span = Span {
                        start: span.start + number_length,
                        end: span.end,
                        line: span.line,
                        column: span.column + number_length,
                    };

                    return ast.add_error_expr(
                        checker.add_error(
                            TypeError::NoFunction {
                                name: postfix.to_string(),
                                postfix: true,
                            },
                            span,
                        ),
                        checker.solver.add_info(TypeInfo::Unknown(None)),
                        span,
                    );
                }
                None => checker.solver.add_info(TypeInfo::Unknown(Some(checker.core_types.int32))),
            }),
            Number(F32(value), postfix) => (LiteralExpr::Float(value), match postfix {
                Some(postfix) => {
                    if let Some(func_ref) = checker.modules[ModuleId::ZERO]
                        .items
                        .get(postfix.as_str())
                        .and_then(|item| if let &ModuleItem::Func(func) = item { Some(func) } else { None })
                    {
                        let func = &checker.local_functions[func_ref];

                        if func.postfix {
                            let TypeInfo::Func(args, returns) = checker.solver.get_info(func.ty) else {
                                return ast.add_error_expr(
                                    checker.add_error(
                                        TypeError::ExpectedFunction {
                                            found: NotFunction::Type(func.ty),
                                        },
                                        span,
                                    ),
                                    checker.solver.add_info(TypeInfo::Unknown(None)),
                                    span,
                                );
                            };

                            let expr = ast.add_expr(Expr::Literal(LiteralExpr::Float(value)), args[0].inner(), span);

                            if let TypeInfo::Func(args, returns) = checker.solver.get_info(checker.local_functions[func_ref].ty) {
                                for arg in args {
                                    if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(arg.inner()) {
                                        ast.used_adt_types.push((*adt_ref, args.clone()));
                                    }
                                }

                                if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(*returns) {
                                    ast.used_adt_types.push((*adt_ref, args.clone()));
                                }
                            }

                            ast.used_functions.push(func_ref);

                            return ast.add_expr(
                                Expr::Call {
                                    func: FuncSource::Explicit(func_ref),
                                    args: Box::new([expr]),
                                },
                                *returns,
                                span,
                            );
                        }

                        let number_length = (span.end - span.start) - postfix.len();
                        let span = Span {
                            start: span.start + number_length,
                            end: span.end,
                            line: span.line,
                            column: span.column + number_length,
                        };

                        return ast.add_error_expr(
                            checker.add_error(TypeError::NotPostfix { name: postfix }, span),
                            checker.solver.add_info(TypeInfo::Unknown(None)),
                            span,
                        );
                    }

                    let number_length = (span.end - span.start) - postfix.len();
                    let span = Span {
                        start: span.start + number_length,
                        end: span.end,
                        line: span.line,
                        column: span.column + number_length,
                    };

                    return ast.add_error_expr(
                        checker.add_error(TypeError::NoFunction { name: postfix, postfix: true }, span),
                        checker.solver.add_info(TypeInfo::Unknown(None)),
                        span,
                    );
                }
                None => checker.core_types.float,
            }),
            Boolean(value) => (LiteralExpr::Boolean(value), checker.core_types.boolean),
            String(value) => (LiteralExpr::String(value), checker.core_types.string),
        };

        ast.add_expr(Expr::Literal(literal), ty, span)
    }
}

impl IntoTypedAST<ExprRef> for mollie_parser::Expr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> ExprRef {
        match self {
            Self::Nothing => ast.add_expr(Expr::Nothing, checker.core_types.void, span),
            Self::Literal(literal_expr) => literal_expr.into_typed_ast(checker, ast, span),
            Self::FunctionCall(func_call_expr) => {
                let func = func_call_expr.function.into_typed_ast(checker, ast);
                let args: Box<[ExprRef]> = func_call_expr.args.value.into_iter().map(|arg| arg.into_typed_ast(checker, ast)).collect();

                if let TypeInfo::Func(expected_args, returns) = checker.solver.get_info(ast[func].ty).clone() {
                    if let &Expr::VTableAccess { target, .. } = &ast[func].value {
                        checker.solver.unify(ast[target].ty, expected_args[0].inner());

                        for (arg, expected_arg) in args.iter().zip(expected_args.into_iter().skip(1)) {
                            checker.solver.unify(ast[*arg].ty, expected_arg.inner());
                        }
                    } else {
                        for (arg, expected_arg) in args.iter().zip(expected_args) {
                            checker.solver.unify(ast[*arg].ty, expected_arg.inner());
                        }
                    }

                    ast.add_expr(
                        Expr::Call {
                            func: FuncSource::Expr(func),
                            args,
                        },
                        returns,
                        span,
                    )
                } else if let Expr::Error(error) = ast[func].value {
                    if let TypeError::NoVariable { name } = mem::replace(&mut checker.errors[error].value, TypeError::ModuleIsNotValue) {
                        if let Ok(ty) = checker.try_cast(ModuleId::ZERO, &name) {
                            checker.errors[error].value = match ty {
                                CastedType::Primitive(primitive_type) => TypeError::ExpectedFunction {
                                    found: NotFunction::Primitive(primitive_type),
                                },
                                CastedType::Adt(adt_ref) => TypeError::ExpectedFunction {
                                    found: NotFunction::Adt(adt_ref),
                                },
                                CastedType::Trait(trait_ref) => TypeError::ExpectedFunction {
                                    found: NotFunction::Trait(trait_ref),
                                },
                                CastedType::Func(_) => unreachable!(),
                            }
                        } else {
                            checker.errors[error].value = TypeError::NoFunction { name, postfix: false };
                        }
                    }

                    func
                } else {
                    ast.add_error_expr(
                        checker.add_error(
                            TypeError::ExpectedFunction {
                                found: NotFunction::Type(ast[func].ty),
                            },
                            ast[func].span,
                        ),
                        checker.solver.add_info(TypeInfo::Unknown(None)),
                        span,
                    )
                }
            }
            Self::Node(mut node_expr) => {
                let name_span = node_expr.name.span;
                let path = node_expr.name.into_typed_ast(checker, ast);
                let mut children = Some(node_expr.children);

                if let TypePath::Adt(ty, struct_ty, variant, None) = path
                    && let TypeInfo::Adt(_, kind, args) = checker.solver.get_info(ty).clone()
                {
                    let fields: Box<[(FieldRef, String, Option<ExprRef>)]> = checker.adt_types[struct_ty].variants[variant.unwrap_or(AdtVariantRef::ZERO)]
                        .fields
                        .iter()
                        .map(|(position, (name, field_type, _))| {
                            let ty = field_type.as_type_info(None, &checker.core_types, &mut checker.solver, args.as_ref());

                            let (name, value) = if matches!(kind, AdtKind::Component) && name == "children" {
                                (
                                    String::from("children"),
                                    if let Some(mut children) = children.take() {
                                        if matches!(checker.solver.get_info(ty), TypeInfo::Array(..)) {
                                            Some(children.map(|elements| {
                                                Self::Array(mollie_parser::ArrayExpr {
                                                    elements: elements.into_iter().map(|node| node.map(Self::Node)).collect(),
                                                })
                                            }))
                                        } else {
                                            children.value.pop().map(|value| value.map(Self::Node))
                                        }
                                    } else {
                                        None
                                    },
                                )
                            } else {
                                node_expr.properties.iter().position(|prop| &prop.value.name.value.0 == name).map_or_else(
                                    || (name.clone(), None),
                                    |prop| {
                                        let prop = node_expr.properties.remove(prop);
                                        let value = prop
                                            .value
                                            .value
                                            .unwrap_or_else(|| prop.value.name.wrap(Self::Ident(prop.value.name.value.clone())));

                                        (prop.value.name.value.0, Some(value))
                                    },
                                )
                            };

                            (position, name, value, ty)
                        })
                        .collect::<Box<_>>()
                        .into_iter()
                        .map(|value| {
                            let (position, name, value, ty) = value;

                            let value = match value {
                                Some(value) => {
                                    let infer = checker.infer.replace(ty);

                                    let value = value.into_typed_ast(checker, ast);

                                    checker.infer = infer;
                                    checker.solver.unify(ast[value].ty, ty);

                                    Some(value)
                                }
                                None => None,
                            };

                            (position, name, value)
                        })
                        .collect();

                    ast.used_adt_types.push((struct_ty, args));

                    if let Some(variant) = variant {
                        ast.add_expr(Expr::Construct { ty, variant, fields }, ty, span)
                    } else {
                        ast.add_expr(
                            Expr::Construct {
                                ty,
                                variant: AdtVariantRef::ZERO,
                                fields,
                            },
                            ty,
                            span,
                        )
                    }
                } else {
                    let (ty, found) = match path {
                        TypePath::Adt(type_info_ref, ..) => (type_info_ref, NonConstructable::Function),
                        TypePath::Trait(type_info_ref, _) => (type_info_ref, NonConstructable::Trait),
                        TypePath::Func(func_ref) => (checker.local_functions[func_ref].ty, NonConstructable::Function),
                        TypePath::Generic(type_info_ref, _) => (type_info_ref, NonConstructable::Generic),
                        TypePath::Module(_) => (checker.solver.add_info(TypeInfo::Unknown(None)), NonConstructable::Module),
                        TypePath::Error(error) => return ast.add_error_expr(error, checker.solver.add_info(TypeInfo::Unknown(None)), name_span),
                    };

                    ast.add_error_expr(checker.add_error(TypeError::ExpectedConstructable { found }, name_span), ty, name_span)
                }
            }
            Self::Index(index_expr) => {
                let target = index_expr.target.into_typed_ast(checker, ast);

                match index_expr.index.value {
                    IndexTarget::Named(property_name) => {
                        if let TypeInfo::Trait(trait_ref, type_args) = checker.solver.get_info(ast[target].ty) {
                            let trait_ref = *trait_ref;
                            let type_args = type_args.clone();

                            for (func, (name, args, returns)) in checker.traits[trait_ref].functions.iter() {
                                if name == &property_name.0 {
                                    let args = args
                                        .iter()
                                        .map(|arg| {
                                            arg.as_ref()
                                                .map(|arg| arg.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, type_args.as_ref()))
                                        })
                                        .collect();

                                    let returns = returns.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, &[]);
                                    let ty = checker.solver.add_info(TypeInfo::Func(args, returns));

                                    return ast.add_expr(
                                        Expr::VTableAccess {
                                            target,
                                            func: VFunc::Unknown(trait_ref, func),
                                        },
                                        ty,
                                        span,
                                    );
                                }
                            }
                        } else {
                            let info = checker.solver.get_info(ast[target].ty);
                            let ty = FieldType::from_type_info(info, Some(ast[target].ty), &checker.solver);

                            if let Some(vtables) = checker.solver.find_vtable(&ty) {
                                for &vtable in vtables.values() {
                                    let func = checker.vtables[vtable].functions.iter().find(|(_, func)| func.name == property_name.0);

                                    if let Some((func_ref, func)) = func {
                                        let func_ty = func.ty;

                                        let args: Box<[TypeInfoRef]> = if let FieldType::Adt(_, _, args) = &ty {
                                            args.iter()
                                                .map(|arg| arg.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, &[]))
                                                .collect()
                                        } else if let FieldType::Array(element, _) = &ty {
                                            Box::new([element.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, &[])])
                                        } else {
                                            Box::new([])
                                        };

                                        if !args.is_empty() {
                                            checker.solver.solve_generic_args(func_ty, args.as_ref());
                                        }

                                        for (adt_ref, type_args) in &checker.vtables[vtable].used_adt_types {
                                            ast.used_adt_types
                                                .push((*adt_ref, if type_args.is_empty() { args.clone() } else { type_args.clone() }));
                                        }

                                        for (ty, vtable, type_args) in &checker.vtables[vtable].used_vtables {
                                            ast.used_vtables.push((*ty, *vtable, type_args.clone()));
                                        }

                                        ast.used_vtables.push((ast[target].ty, vtable, args));

                                        return ast.add_expr(
                                            Expr::VTableAccess {
                                                target,
                                                func: VFunc::Known(vtable, func_ref),
                                            },
                                            func_ty,
                                            span,
                                        );
                                    }
                                }
                            }
                        }

                        match checker.solver.get_info(ast[target].ty) {
                            TypeInfo::Adt(ty, AdtKind::Struct | AdtKind::Component, args) => {
                                let ty = *ty;
                                let args = args.clone();

                                for item in
                                    checker.adt_types[ty].instantiate(AdtVariantRef::new(0), None, &checker.core_types, &mut checker.solver, args.as_ref())
                                {
                                    if item.1 == property_name.0 {
                                        return ast.add_expr(Expr::Access { target, field: item.0 }, item.2, span);
                                    }
                                }

                                ast.add_error_expr(
                                    checker.add_error(
                                        TypeError::NoField {
                                            adt: ty,
                                            name: property_name.0,
                                        },
                                        span,
                                    ),
                                    checker.solver.add_info(TypeInfo::Unknown(None)),
                                    span,
                                )
                            }
                            _ => ast.add_error_expr(
                                checker.add_error(
                                    TypeError::NonIndexable {
                                        ty: ast[target].ty,
                                        name: property_name.0,
                                    },
                                    span,
                                ),
                                checker.solver.add_info(TypeInfo::Unknown(None)),
                                span,
                            ),
                        }
                    }
                    IndexTarget::Expression(expr) => {
                        let index = expr.into_typed_ast(checker, ast, index_expr.index.span);

                        checker.solver.unify(ast[index].ty, checker.core_types.uint_size);

                        match checker.solver.get_info(ast[target].ty) {
                            TypeInfo::Array(element, _) => ast.add_expr(Expr::Index { target, index }, *element, span),
                            _ => ast.add_error_expr(
                                checker.add_error(TypeError::ExpectedArray { found: ast[target].ty }, span),
                                checker.solver.add_info(TypeInfo::Unknown(None)),
                                span,
                            ),
                        }
                    }
                }
            }
            Self::Binary(binary_expr) => {
                let lhs = binary_expr.lhs.into_typed_ast(checker, ast);
                let rhs = binary_expr.rhs.into_typed_ast(checker, ast);

                checker.solver.unify(ast[rhs].ty, ast[lhs].ty);

                let ty = if matches!(
                    binary_expr.operator.value,
                    Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan
                ) {
                    checker.core_types.boolean
                } else {
                    ast[lhs].ty
                };

                ast.add_expr(
                    Expr::Binary {
                        operator: binary_expr.operator.value,
                        lhs,
                        rhs,
                    },
                    ty,
                    span,
                )
            }
            Self::TypeIndex(type_index_expr) => {
                let path = type_index_expr.into_typed_ast(checker, ast, span);

                let (ty, resulting_ty) = match path {
                    TypePath::Adt(type_info_ref, .., vfunc) => vfunc.map_or((type_info_ref, type_info_ref), |vfunc| (type_info_ref, checker[vfunc].ty)),
                    TypePath::Trait(type_info_ref, _) | TypePath::Generic(type_info_ref, _) => (type_info_ref, type_info_ref),
                    TypePath::Func(func_ref) => (checker.local_functions[func_ref].ty, checker.local_functions[func_ref].ty),
                    TypePath::Module(_) => {
                        return ast.add_error_expr(
                            checker.add_error(TypeError::ModuleIsNotValue, span),
                            checker.solver.add_info(TypeInfo::Unknown(None)),
                            span,
                        );
                    }
                    TypePath::Error(error) => {
                        return ast.add_error_expr(error, checker.solver.add_info(TypeInfo::Unknown(None)), span);
                    }
                };

                ast.add_expr(Expr::TypeIndex { ty, path }, resulting_ty, span)
            }
            Self::Array(array_expr) => {
                let element = if let Some(ty) = checker.infer.take_if(|ty| checker.solver.get_info(*ty).is_array()) {
                    let &TypeInfo::Array(element, _) = checker.solver.get_info(ty) else {
                        unreachable!()
                    };

                    element
                } else {
                    checker.solver.add_info(TypeInfo::Generic(0, None))
                };

                let mut elements = Vec::with_capacity(array_expr.elements.capacity());

                for arr_element in array_expr.elements {
                    let arr_element = arr_element.into_typed_ast(checker, ast);

                    if let &TypeInfo::Trait(t, _) = checker.solver.get_info(element)
                        && let Some(vtables) = checker.solver.find_vtable(&FieldType::from_type_info(
                            checker.solver.get_info(ast[arr_element].ty),
                            Some(ast[arr_element].ty),
                            &checker.solver,
                        ))
                        && let Some(&vtable) = vtables.get(&Some(t))
                    {
                        for (adt_ref, type_args) in &checker.vtables[vtable].used_adt_types {
                            let adt = (*adt_ref, type_args.clone());

                            if !ast.used_adt_types.contains(&adt) {
                                ast.used_adt_types.push(adt);
                            }
                        }

                        for (ty, vtable, type_args) in &checker.vtables[vtable].used_vtables {
                            let vtable = (*ty, *vtable, type_args.clone());

                            if !ast.used_vtables.contains(&vtable) {
                                ast.used_vtables.push(vtable);
                            }
                        }

                        let vtable = (ast[arr_element].ty, vtable, Box::new([]) as Box<[_]>);

                        if !ast.used_vtables.contains(&vtable) {
                            ast.used_vtables.push(vtable);
                        }
                    }

                    checker.solver.unify(element, ast[arr_element].ty);

                    elements.push(arr_element);
                }

                let size = elements.len();
                let array_ty = checker.solver.add_info(TypeInfo::Array(element, Some(size)));

                ast.add_expr(Expr::Array(elements.into_boxed_slice()), array_ty, span)
            }
            Self::IfElse(if_else_expr) => {
                checker.solver.push_frame();

                let condition_expected = checker.core_types.boolean;
                let condition = if_else_expr.condition.into_typed_ast(checker, ast);

                checker.solver.unify(condition_expected, ast[condition].ty);

                let block = if_else_expr.block.into_typed_ast(checker, ast);

                checker.solver.pop_frame();

                let ty = ast[block].ty;

                let else_block = if let Some(else_block) = if_else_expr.else_block {
                    let block = else_block.into_typed_ast(checker, ast);

                    checker.solver.unify(ast[block].ty, ty);

                    Some(block)
                } else {
                    None
                };

                ast.add_expr(Expr::If { condition, block, else_block }, ty, span)
            }
            Self::While(while_expr) => {
                checker.solver.push_frame();

                let condition_expected = checker.core_types.boolean;
                let condition = while_expr.condition.into_typed_ast(checker, ast);

                checker.solver.unify(ast[condition].ty, condition_expected);

                let ty = checker.core_types.void;
                let block = while_expr.block.into_typed_ast(checker, ast);

                checker.solver.pop_frame();
                checker.solver.unify(ast[block].ty, ty);

                ast.add_expr(Expr::While { condition, block }, ty, span)
            }
            Self::Block(block_expr) => {
                checker.solver.push_frame();

                let block = block_expr.into_typed_ast(checker, ast, span);
                let ty = ast[block].ty;

                checker.solver.pop_frame();

                ast.add_expr(Expr::Block(block), ty, span)
            }
            Self::Is(is_expr) => {
                let target = is_expr.target.into_typed_ast(checker, ast);
                let pattern = is_expr.pattern.span.wrap((ast[target].ty, is_expr.pattern.value)).into_typed_ast(checker, ast);

                ast.add_expr(Expr::IsPattern { target, pattern }, checker.core_types.boolean, span)
            }
            Self::Closure(closure_expr) => {
                let prev_captures = mem::take(&mut checker.captures);
                let mut args = Vec::with_capacity(closure_expr.args.value.capacity());
                let mut arg_types = Vec::with_capacity(closure_expr.args.value.capacity());

                checker.solver.push_frame();

                for arg in closure_expr.args.value {
                    let ty = checker.solver.add_info(TypeInfo::Unknown(None));

                    checker.solver.add_var(&arg.value.0, ty);

                    args.push(arg.value.0);
                    arg_types.push(FuncArg::Regular(ty));
                }

                let body = closure_expr.body.into_typed_ast(checker, ast);
                let ty = checker.solver.add_info(TypeInfo::Func(arg_types.into_boxed_slice(), ast[body].ty));

                checker.solver.pop_frame();

                let captures = mem::replace(&mut checker.captures, prev_captures);

                ast.add_expr(
                    Expr::Closure {
                        args: args.into_boxed_slice(),
                        captures: captures.into_boxed_slice(),
                        body,
                    },
                    ty,
                    span,
                )
            }
            Self::Ident(ident) => {
                if let Some(var) = checker.solver.get_var(&ident.0) {
                    if var.frame != checker.solver.current_frame() && !checker.solver.get_info(var.ty).is_func() {
                        checker.captures.push((ident.0.clone(), var));
                    }

                    ast.add_expr(Expr::Var(ident.0), var.ty, span)
                } else if let Some(&ModuleItem::Func(func_ref)) = checker.modules[ModuleId::ZERO].items.get(&ident.0) {
                    let ty = checker.local_functions[func_ref].ty;

                    if let TypeInfo::Func(args, returns) = checker.solver.get_info(checker.local_functions[func_ref].ty) {
                        for arg in args {
                            if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(arg.inner()) {
                                ast.used_adt_types.push((*adt_ref, args.clone()));
                            }
                        }

                        if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(*returns) {
                            ast.used_adt_types.push((*adt_ref, args.clone()));
                        }
                    }

                    ast.used_functions.push(func_ref);

                    ast.add_expr(
                        Expr::TypeIndex {
                            ty,
                            path: TypePath::Func(func_ref),
                        },
                        ty,
                        span,
                    )
                } else {
                    ast.add_error_expr(
                        checker.add_error(TypeError::NoVariable { name: ident.0 }, span),
                        checker.solver.add_info(TypeInfo::Unknown(None)),
                        span,
                    )
                }
            }
            Self::This => {
                let name = "self".into();

                if let Some(var) = checker.solver.get_var("self") {
                    ast.add_expr(Expr::Var(name), var.ty, span)
                } else {
                    ast.add_error_expr(
                        checker.add_error(TypeError::NoVariable { name }, span),
                        checker.solver.add_info(TypeInfo::Unknown(None)),
                        span,
                    )
                }
            }
            Self::ForIn(_for_in_expr) => todo!(),
        }
    }
}

impl IntoTypedAST<BlockRef> for mollie_parser::BlockExpr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> BlockRef {
        let stmts = self.stmts.into_iter().fold(Vec::new(), |mut stmts, stmt| {
            if let Some(stmt) = stmt.into_typed_ast(checker, ast) {
                stmts.push(stmt);
            }

            stmts
        });

        let (expr, ty) = match self.final_stmt {
            Some(stmt) => {
                if let mollie_parser::Stmt::Expression(expr) = stmt.value {
                    let expr = expr.into_typed_ast(checker, ast, stmt.span);

                    (Some(expr), ast[expr].ty)
                } else {
                    (None, checker.core_types.void)
                }
            }
            None => (None, checker.core_types.void),
        };

        ast.add_block(
            Block {
                stmts: stmts.into_boxed_slice(),
                expr,
            },
            ty,
            span,
        )
    }
}

impl IntoTypedAST<IsPattern> for (TypeInfoRef, mollie_parser::IsPattern) {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> IsPattern {
        match self.1 {
            mollie_parser::IsPattern::Literal(literal_expr) => IsPattern::Literal(literal_expr.into_typed_ast(checker, ast, span)),
            mollie_parser::IsPattern::Type { ty, pattern } => {
                let path = ty.into_typed_ast(checker, ast);

                match path {
                    TypePath::Adt(target_ty, target, current_variant, None) => {
                        let generic_args = if let TypeInfo::Adt(.., generic_args) = checker.solver.get_info(target_ty) {
                            generic_args.clone()
                        } else {
                            Box::default()
                        };

                        if let TypeInfo::Adt(_, _, type_args) = checker.solver.get_info(self.0) {
                            for (expected, got) in type_args.clone().into_iter().zip(&generic_args) {
                                checker.solver.unify(*got, expected);
                            }
                        }

                        match (current_variant, pattern.value) {
                            (Some(variant), TypePattern::Values(values)) => {
                                let fields = checker.adt_types[target]
                                    .instantiate(variant, None, &checker.core_types, &mut checker.solver, generic_args.as_ref())
                                    .map(|(field, name, ty)| (field, name.to_string(), ty))
                                    .collect::<Box<[_]>>();

                                let mut new_values = Vec::new();

                                for value in values {
                                    if let Some(prop) = fields.iter().find(|prop| prop.1 == value.value.name.value.0) {
                                        let pattern = if let Some(value) = value.value.value {
                                            Some(value.span.wrap((prop.2, value.value)).into_typed_ast(checker, ast))
                                        } else {
                                            None
                                        };

                                        if pattern.is_none() {
                                            checker.solver.add_var(&prop.1, prop.2);
                                        }

                                        new_values.push((prop.0, value.value.name.value.0, pattern));
                                    }
                                }

                                IsPattern::EnumVariant {
                                    target,
                                    target_args: generic_args,
                                    variant,
                                    values: new_values.into_boxed_slice(),
                                }
                            }
                            (None, TypePattern::Name(name)) => {
                                checker.solver.add_var(&name.0, target_ty);

                                IsPattern::TypeName { ty: target_ty, name: name.0 }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    TypePath::Trait(..) => todo!(),
                    TypePath::Module(_) | TypePath::Adt(.., Some(_)) | TypePath::Generic(..) | TypePath::Func(_) | TypePath::Error(_) => unimplemented!(),
                }
            }
        }
    }
}

impl IntoTypedAST<Option<StmtRef>> for mollie_parser::Stmt {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Option<StmtRef> {
        match self {
            Self::Expression(expr) => {
                let expr = expr.into_typed_ast(checker, ast, span);

                Some(ast.add_stmt(Stmt::Expr(expr)))
            }
            Self::VariableDecl(variable_decl) => {
                let value = variable_decl.value.into_typed_ast(checker, ast);
                let value_ty = ast[value].ty;

                if let Some(ty) = variable_decl.ty {
                    let ty = ty.into_typed_ast(checker, ast);
                    let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                    assert!(!checker.solver.contains_unknown(ty), "explicit type can't have non-explicit generics");

                    checker.solver.unify(value_ty, ty);
                    checker.solver.add_var(&variable_decl.name.value.0, ty);
                } else {
                    checker.solver.add_var(&variable_decl.name.value.0, value_ty);
                }

                Some(ast.add_stmt(Stmt::VariableDecl {
                    name: variable_decl.name.value.0,
                    value,
                }))
            }
            Self::StructDecl(struct_decl) => {
                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), (index, None));
                }

                let mut variants = IndexVec::new();
                let mut properties = IndexVec::new();

                for property in struct_decl.properties.value {
                    let name = property.value.name.value.0;
                    let ty = property.value.ty.into_typed_ast(checker, ast);
                    let constant = match property.value.default_value {
                        Some(value) => {
                            let expected_ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                            let value = value.into_typed_ast(checker, ast);

                            checker.solver.unify(ast[value].ty, expected_ty);

                            value.into_constant_value(checker, ast, &mut ConstantContext::default()).map_or_else(
                                |()| {
                                    checker.add_error(TypeError::NonConstantEvaluable, ast[value].span);

                                    None
                                },
                                Some,
                            )
                        }
                        None => None,
                    };

                    properties.push((name, ty, constant));
                }

                variants.push(AdtVariant {
                    name: None,
                    discriminant: 0,
                    fields: properties.into_boxed_slice(),
                });

                for name in &struct_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(checker.adt_types.len());

                checker.modules[ast.module]
                    .items
                    .insert(struct_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                checker.adt_types.push(Adt {
                    name: Some(struct_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Struct,
                    generics: struct_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            Self::ComponentDecl(component_decl) => {
                for (index, name) in component_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), (index, None));
                }

                let mut variants = IndexVec::new();
                let mut fields = IndexVec::with_capacity(component_decl.properties.len());

                for property in component_decl.properties {
                    let name = property.value.name.value.0;
                    let ty = property.value.ty.into_typed_ast(checker, ast);
                    let constant = match property.value.default_value {
                        Some(value) => {
                            let expected_ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                            let value = value.into_typed_ast(checker, ast);

                            checker.solver.unify(ast[value].ty, expected_ty);

                            value.into_constant_value(checker, ast, &mut ConstantContext::default()).map_or_else(
                                |()| {
                                    checker.add_error(TypeError::NonConstantEvaluable, ast[value].span);

                                    None
                                },
                                Some,
                            )
                        }
                        None => None,
                    };

                    fields.push((name, ty, constant));
                }

                variants.push(AdtVariant {
                    name: None,
                    discriminant: 0,
                    fields: fields.into_boxed_slice(),
                });

                for name in &component_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(checker.adt_types.len());

                checker.modules[ast.module]
                    .items
                    .insert(component_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                checker.adt_types.push(Adt {
                    name: Some(component_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Component,
                    generics: component_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            Self::TraitDecl(trait_decl) => {
                for (index, name) in trait_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), (index, None));
                }

                let mut functions = IndexVec::with_capacity(trait_decl.functions.value.len());

                for function in trait_decl.functions.value {
                    let name = function.value.name.value.0;
                    let mut args = Vec::with_capacity(function.value.args.len() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        args.push(FuncArg::This(FieldType::This));
                    }

                    for arg in function.value.args {
                        args.push(FuncArg::Regular(arg.value.ty.into_typed_ast(checker, ast)));
                    }

                    let output = function
                        .value
                        .returns
                        .map_or(FieldType::Primitive(PrimitiveType::Void), |returns| returns.into_typed_ast(checker, ast));

                    functions.push((name, args, output));
                }

                for name in &trait_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let trait_ref = TraitRef::new(checker.traits.len());

                checker.traits.push(Trait {
                    name: trait_decl.name.value.name.value.0.clone(),
                    generics: trait_decl.name.value.generics.len(),
                    functions,
                });

                checker.modules[ast.module]
                    .items
                    .insert(trait_decl.name.value.name.value.0, ModuleItem::Trait(trait_ref));

                None
            }
            Self::EnumDecl(enum_decl) => {
                for (index, name) in enum_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), (index, None));
                }

                let mut variants = IndexVec::new();

                for (discriminant, variant) in enum_decl.variants.value.into_iter().enumerate() {
                    let name = Some(variant.value.name.value.0);
                    let mut fields = IndexVec::with_capacity(variant.value.properties.as_ref().map(|value| value.value.len()).unwrap_or_default());

                    fields.push((String::from("<discriminant>"), FieldType::Primitive(PrimitiveType::UInt(UIntType::USize)), None));

                    if let Some(properties) = variant.value.properties {
                        for property in properties.value {
                            let name = property.value.name.value.0;
                            let ty = property.value.ty.into_typed_ast(checker, ast);

                            fields.push((name, ty, None));
                        }
                    }

                    variants.push(AdtVariant {
                        name,
                        discriminant,
                        fields: fields.into_boxed_slice(),
                    });
                }

                for name in &enum_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(checker.adt_types.len());

                checker.modules[ast.module]
                    .items
                    .insert(enum_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                checker.adt_types.push(Adt {
                    name: Some(enum_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Enum,
                    generics: enum_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            Self::FuncDecl(func_decl) => {
                checker.solver.push_frame();

                let postfix = func_decl
                    .modifiers
                    .iter()
                    .any(|modifier| matches!(modifier.value, mollie_parser::FuncModifier::Postfix));

                let mut arg_names = Vec::with_capacity(func_decl.args.value.capacity());
                let mut args = Vec::with_capacity(func_decl.args.value.capacity());
                let mut arg_spans = Vec::with_capacity(func_decl.args.value.capacity());

                for arg in func_decl.args.value {
                    let ty = arg.value.ty.into_typed_ast(checker, ast);
                    let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                    checker.solver.add_var(&arg.value.name.value.0, ty);

                    arg_names.push(arg.value.name.value.0);
                    args.push(FuncArg::Regular(ty));
                    arg_spans.push(arg.span);
                }

                if postfix {
                    let mut reasons = Vec::new();

                    if args.is_empty() {
                        reasons.push(func_decl.args.span.wrap(PostfixRequirement::OneArgument));
                    } else {
                        if !checker.solver.get_info(args[0].inner()).is_number() {
                            reasons.push(arg_spans[0].wrap(PostfixRequirement::ArgumentType));
                        }

                        if args.len() > 1 {
                            let first = arg_spans[1];

                            reasons.push(
                                arg_spans
                                    .into_iter()
                                    .skip(2)
                                    .fold(first, |p, c| p.between(c))
                                    .wrap(PostfixRequirement::OnlyOneArgument),
                            );
                        }
                    }

                    if !reasons.is_empty() {
                        checker.add_error(TypeError::InvalidPostfixFunction { reasons }, span.between(func_decl.args.span));
                    }
                }

                let returns = if let Some(returns) = func_decl.returns {
                    returns
                        .into_typed_ast(checker, ast)
                        .as_type_info(None, &checker.core_types, &mut checker.solver, &[])
                } else {
                    checker.core_types.void
                };

                let body = func_decl.body.into_typed_ast(checker, ast);

                checker.solver.unify(ast[body].ty, returns);
                checker.solver.pop_frame();

                let ty = checker.solver.add_info(TypeInfo::Func(args.into_boxed_slice(), returns));

                let func_ref = FuncRef::new(checker.local_functions.len());

                checker.modules[ast.module]
                    .items
                    .insert(func_decl.name.value.0.clone(), ModuleItem::Func(func_ref));

                checker.local_functions.push(Func {
                    postfix,
                    name: func_decl.name.value.0,
                    arg_names,
                    ty,
                    kind: VTableFuncKind::Local(body),
                });

                None
            }
            Self::Impl(mut implementation) => {
                for (index, name) in implementation.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), (index, None));
                }

                let mut applied_generics: Box<[TypeInfoRef]> = Box::default();

                let trait_ref = match implementation.trait_name {
                    Some(trait_name) => {
                        if let TypePath::Trait(ty, trait_ref) = trait_name.into_typed_ast(checker, ast) {
                            if let TypeInfo::Trait(_, args) = checker.solver.get_info(ty) {
                                applied_generics.clone_from(args);
                            }

                            Some((checker.traits[trait_ref].name.clone(), trait_ref))
                        } else {
                            None
                        }
                    }
                    None => None,
                };

                let target = implementation.target.into_typed_ast(checker, ast);
                let target_info = target.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                let mut functions = IndexVec::with_capacity(
                    implementation
                        .functions
                        .value
                        .capacity()
                        .max(trait_ref.as_ref().map(|(_, t)| checker.traits[*t].functions.len()).unwrap_or_default()),
                );

                let used_adt_types = ast.used_adt_types.len();
                let used_vtables = ast.used_vtables.len();

                if let Some((trait_name, trait_ref)) = &trait_ref {
                    let trait_functions = checker.traits[*trait_ref]
                        .functions
                        .iter()
                        .map(|(k, (name, ..))| (k, name.clone()))
                        .collect::<Box<[_]>>();

                    for (func_ref, name) in trait_functions {
                        if let Some(func) = implementation.functions.value.iter().position(|func| func.value.name.value.0 == name) {
                            let function = implementation.functions.value.remove(func);

                            checker.solver.push_frame();

                            let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                            let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                            if function.value.this.is_some() {
                                checker.solver.add_var("self", target_info);

                                arg_names.push("self".to_string());
                                args.push(FuncArg::This(target_info));
                            }

                            for arg in function.value.args {
                                let ty = arg.value.ty.into_typed_ast(checker, ast);
                                let ty = ty.as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &[]);

                                checker.solver.add_var(&arg.value.name.value.0, ty);

                                if let TypeInfo::Adt(adt_ref, _, adt_args) = checker.solver.get_info(ty) {
                                    let value = (*adt_ref, adt_args.clone());

                                    if !ast.used_adt_types.contains(&value) {
                                        ast.used_adt_types.push(value);
                                    }
                                }

                                arg_names.push(arg.value.name.value.0);
                                args.push(FuncArg::Regular(ty));
                            }

                            let returns = if let Some(returns) = function.value.returns {
                                returns
                                    .into_typed_ast(checker, ast)
                                    .as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &[])
                            } else {
                                checker.core_types.void
                            };

                            let body = function.value.body.into_typed_ast(checker, ast);

                            checker.solver.unify(ast[body].ty, returns);
                            checker.solver.pop_frame();

                            functions.push(VTableFunc {
                                trait_func: Some(func_ref),
                                name: function.value.name.value.0,
                                arg_names,
                                ty: checker.solver.add_info(TypeInfo::Func(args.into_boxed_slice(), returns)),
                                kind: VTableFuncKind::Local(body),
                            });
                        } else {
                            println!("didn't found implementation for {trait_name}::{name}");
                        }
                    }
                }

                for function in implementation.functions.value {
                    if let Some((trait_name, _)) = &trait_ref {
                        println!("there's no function called {} in {trait_name}", function.value.name.value.0);
                    }

                    checker.solver.push_frame();

                    let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                    let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        checker.solver.add_var("self", target_info);

                        arg_names.push("self".to_string());
                        args.push(FuncArg::This(target_info));
                    }

                    for arg in function.value.args {
                        let ty = arg.value.ty.into_typed_ast(checker, ast);
                        let ty = ty.as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &[]);

                        checker.solver.add_var(&arg.value.name.value.0, ty);

                        arg_names.push(arg.value.name.value.0);
                        args.push(FuncArg::Regular(ty));
                    }

                    let returns = if let Some(returns) = function.value.returns {
                        returns
                            .into_typed_ast(checker, ast)
                            .as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &[])
                    } else {
                        checker.core_types.void
                    };

                    let body = function.value.body.into_typed_ast(checker, ast);

                    checker.solver.unify(ast[body].ty, returns);
                    checker.solver.pop_frame();

                    functions.push(VTableFunc {
                        trait_func: None,
                        name: function.value.name.value.0,
                        arg_names,
                        ty: checker.solver.add_info(TypeInfo::Func(args.into_boxed_slice(), returns)),
                        kind: VTableFuncKind::Local(body),
                    });
                }

                for name in &implementation.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let used_adt_types = ast.used_adt_types.split_off(used_adt_types);
                let used_vtables = ast.used_vtables.split_off(used_vtables);

                let (trait_name, trait_ref) = match trait_ref {
                    Some((name, t_ref)) => (Some(name), Some(t_ref)),
                    None => (None, None),
                };

                match checker.solver.vtables.entry(target) {
                    Entry::Occupied(entry) => match entry.into_mut().entry(trait_ref) {
                        Entry::Occupied(entry) => {
                            if let Some(trait_name) = &trait_name {
                                println!("duplicate {trait_name} implementation!");
                            } else {
                                for func in functions.into_values() {
                                    checker.vtables[*entry.get()].functions.push(func);
                                }
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(checker.vtables.insert(VTableGenerator {
                                origin_trait: trait_ref,
                                generics: applied_generics,
                                used_adt_types,
                                used_vtables,
                                functions,
                            }));
                        }
                    },
                    Entry::Vacant(entry) => {
                        entry.insert(
                            iter::once((
                                trait_ref,
                                checker.vtables.insert(VTableGenerator {
                                    origin_trait: trait_ref,
                                    generics: applied_generics,
                                    used_adt_types,
                                    used_vtables,
                                    functions,
                                }),
                            ))
                            .collect(),
                        );
                    }
                }

                None
            }
            Self::Import(import) => {
                let path_span = import.path.span;
                let path = import.path.into_typed_ast(checker, ast);

                match path {
                    TypePath::Module(module) => match import.kind {
                        mollie_parser::ImportKind::Partial(items) => {
                            for item in items.value {
                                let name = item.value.0;

                                if let Some(item) = checker.modules[module].items.get(&name).copied() {
                                    checker.modules[ModuleId::ZERO].items.insert(name, item);
                                }
                            }
                        }
                        mollie_parser::ImportKind::Named => {
                            let name = checker.modules[module].name.clone();

                            checker.modules[ModuleId::ZERO].items.insert(name, ModuleItem::SubModule(module));
                        }
                    },
                    TypePath::Adt(..) => {
                        checker.add_error(TypeError::ExpectedModule { found: NotModule::Adt }, path_span);
                    }
                    TypePath::Trait(..) => {
                        checker.add_error(TypeError::ExpectedModule { found: NotModule::Trait }, path_span);
                    }
                    TypePath::Func(_) => {
                        checker.add_error(TypeError::ExpectedModule { found: NotModule::Function }, path_span);
                    }
                    TypePath::Generic(..) => {
                        checker.add_error(TypeError::ExpectedModule { found: NotModule::Generic }, path_span);
                    }
                    TypePath::Error(_) => (),
                }

                None
            }
        }
    }
}

impl IntoTypedAST<Vec<FieldType>> for mollie_parser::TypeArgs {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, _: Span) -> Vec<FieldType> {
        self.0.into_iter().map(|generic| generic.into_typed_ast(checker, ast)).collect()
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum TypePath {
    Adt(TypeInfoRef, AdtRef, Option<AdtVariantRef>, Option<(VTableRef, VFuncRef)>),
    Trait(TypeInfoRef, TraitRef),
    Func(FuncRef),
    Generic(TypeInfoRef, usize),
    Module(ModuleId),
    Error(TypeErrorRef),
}

impl IntoTypedAST<TypePath> for mollie_parser::TypePathExpr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, _: Span) -> TypePath {
        let mut result = TypePath::Module(ModuleId::ZERO);

        for segment in self.segments {
            if let TypePath::Adt(target, ty, ty_variant, vfunc) = &mut result {
                if ty_variant.is_none() {
                    for (variant_ref, variant) in checker.adt_types[*ty].variants.iter() {
                        if variant.name.as_deref() == Some(segment.value.name.value.0.as_str()) {
                            ty_variant.replace(variant_ref);

                            break;
                        }
                    }
                }

                if vfunc.is_none() {
                    let field_type = FieldType::from_type_info(checker.solver.get_info(*target), Some(*target), &checker.solver);

                    if let Some(vtables) = checker.solver.find_vtable(&field_type) {
                        for &vtable in vtables.values() {
                            for (func_ref, func) in checker.vtables[vtable].functions.iter() {
                                if func.name == segment.value.name.value.0 {
                                    ast.used_vtables.push((*target, vtable, Box::new([])));

                                    vfunc.replace((vtable, func_ref));
                                }
                            }
                        }
                    }
                }
            } else if let Some((g, ty)) = checker.available_generics.get(&segment.value.name.value.0).copied() {
                let ty = ty.unwrap_or_else(|| checker.solver.add_info(TypeInfo::Generic(g, None)));

                checker.available_generics.insert(segment.value.name.value.0, (g, Some(ty)));

                result = TypePath::Generic(ty, g);
            } else if let &TypePath::Module(current_module) = &result {
                if let Some(item) = checker.modules[current_module].items.get(&segment.value.name.value.0) {
                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePath::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| type_args.into_typed_ast(checker, ast));

                            let args = (0..checker.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or(FieldType::Generic(generic, None)))
                                .rev()
                                .map(|arg| arg.as_type_info(None, &checker.core_types, &mut checker.solver, &[]))
                                .collect();

                            result = TypePath::Adt(
                                checker.solver.add_info(TypeInfo::Adt(adt_ref, checker.adt_types[adt_ref].kind, args)),
                                adt_ref,
                                None,
                                None,
                            );
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| type_args.into_typed_ast(checker, ast));

                            let args = (0..checker.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or(FieldType::Generic(generic, None)))
                                .rev()
                                .map(|arg| arg.as_type_info(None, &checker.core_types, &mut checker.solver, &[]))
                                .collect();

                            result = TypePath::Trait(checker.solver.add_info(TypeInfo::Trait(trait_ref, args)), trait_ref);
                        }
                        ModuleItem::Func(func_ref) => {
                            if let TypeInfo::Func(args, returns) = checker.solver.get_info(checker.local_functions[func_ref].ty) {
                                for arg in args {
                                    if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(arg.inner()) {
                                        ast.used_adt_types.push((*adt_ref, args.clone()));
                                    }
                                }

                                if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(*returns) {
                                    ast.used_adt_types.push((*adt_ref, args.clone()));
                                }
                            }

                            ast.used_functions.push(func_ref);

                            result = TypePath::Func(func_ref);
                        }
                    }
                } else {
                    return TypePath::Error(checker.add_error(
                        match checker.try_cast(current_module, &segment.value.name.value.0) {
                            Ok(ty) => match ty {
                                CastedType::Primitive(primitive) => TypeError::InvalidTypePathSegment {
                                    reason: InvalidTypePathSegmentReason::Primitive(primitive),
                                    module: current_module,
                                },
                                CastedType::Func(_) | CastedType::Trait(_) | CastedType::Adt(_) => unreachable!(),
                            },
                            Err(err) => match err {
                                TypeInfoCastError::IsModule => TypeError::ModuleIsNotValue,
                                TypeInfoCastError::IsVariable => TypeError::InvalidTypePathSegment {
                                    reason: InvalidTypePathSegmentReason::Variable(segment.value.name.value.0),
                                    module: current_module,
                                },
                                TypeInfoCastError::TypeInPath(primitive) => TypeError::InvalidTypePathSegment {
                                    reason: InvalidTypePathSegmentReason::Primitive(primitive),
                                    module: current_module,
                                },
                                TypeInfoCastError::NotFound => TypeError::TypeNotFound {
                                    name: segment.value.name.value.0,
                                    module: current_module,
                                },
                            },
                        },
                        segment.span,
                    ));
                }
            }
        }

        if let &TypePath::Adt(info_ref, ..) = &result
            && let TypeInfo::Adt(ty, _, args) = checker.solver.get_info(info_ref)
        {
            ast.used_adt_types.push((*ty, args.clone()));
        }

        result
    }
}

impl IntoTypedAST<FieldType> for mollie_parser::Type {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> FieldType {
        match self {
            Self::Primitive(primitive_type) => {
                use mollie_parser::PrimitiveType::{
                    Boolean, Component, Float, Int8, Int16, Int32, Int64, IntSize, String, UInt8, UInt16, UInt32, UInt64, UIntSize, Void,
                };

                match primitive_type {
                    IntSize => FieldType::Primitive(PrimitiveType::Int(IntType::ISize)),
                    Int64 => FieldType::Primitive(PrimitiveType::Int(IntType::I64)),
                    Int32 => FieldType::Primitive(PrimitiveType::Int(IntType::I32)),
                    Int16 => FieldType::Primitive(PrimitiveType::Int(IntType::I16)),
                    Int8 => FieldType::Primitive(PrimitiveType::Int(IntType::I8)),
                    UIntSize => FieldType::Primitive(PrimitiveType::UInt(UIntType::USize)),
                    UInt64 => FieldType::Primitive(PrimitiveType::UInt(UIntType::U64)),
                    UInt32 => FieldType::Primitive(PrimitiveType::UInt(UIntType::U32)),
                    UInt16 => FieldType::Primitive(PrimitiveType::UInt(UIntType::U16)),
                    UInt8 => FieldType::Primitive(PrimitiveType::UInt(UIntType::U8)),
                    Float => FieldType::Primitive(PrimitiveType::Float),
                    Boolean => FieldType::Primitive(PrimitiveType::Boolean),
                    String => FieldType::Primitive(PrimitiveType::String),
                    Component => FieldType::Primitive(PrimitiveType::Component),
                    Void => FieldType::Primitive(PrimitiveType::Void),
                }
            }
            Self::Array(element, size) => {
                let element = element.into_typed_ast(checker, ast);

                FieldType::Array(Box::new(element), size.map(|size| size.value))
            }
            Self::Func(args, output) => {
                let args = args.into_iter().map(|arg| FuncArg::Regular(arg.into_typed_ast(checker, ast))).collect();

                let output = output.map_or(FieldType::Primitive(PrimitiveType::Void), |output| output.into_typed_ast(checker, ast));

                FieldType::Func(args, Box::new(output))
            }
            Self::Path(path) => {
                let mut result = None;
                let mut current_module = ModuleId::ZERO;

                for segment in path.segments {
                    if let Some((g, ty)) = checker.available_generics.get(&segment.value.name.value.0).copied() {
                        let ty = ty.unwrap_or_else(|| checker.solver.add_info(TypeInfo::Generic(g, None)));

                        checker.available_generics.insert(segment.value.name.value.0, (g, Some(ty)));

                        result = Some(FieldType::Generic(g, Some(ty)));
                    } else if result.is_none() {
                        if let Some(item) = checker.modules[current_module].items.get(&segment.value.name.value.0) {
                            match *item {
                                ModuleItem::SubModule(module_id) => current_module = module_id,
                                ModuleItem::Adt(adt_ref) => {
                                    let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| type_args.into_typed_ast(checker, ast));

                                    let args = (0..checker.adt_types[adt_ref].generics)
                                        .rev()
                                        .map(|generic| type_args.pop().unwrap_or(FieldType::Generic(generic, None)))
                                        .rev()
                                        .collect();

                                    result = Some(FieldType::Adt(adt_ref, checker.adt_types[adt_ref].kind, args));
                                }
                                ModuleItem::Trait(trait_ref) => {
                                    let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| type_args.into_typed_ast(checker, ast));

                                    let args = (0..checker.traits[trait_ref].generics)
                                        .rev()
                                        .map(|generic| type_args.pop().unwrap_or(FieldType::Generic(generic, None)))
                                        .rev()
                                        .collect();

                                    result = Some(FieldType::Trait(trait_ref, args));
                                }
                                ModuleItem::Func(func_ref) => {
                                    if let TypeInfo::Func(args, returns) = checker.solver.get_info(checker.local_functions[func_ref].ty) {
                                        for arg in args {
                                            if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(arg.inner()) {
                                                ast.used_adt_types.push((*adt_ref, args.clone()));
                                            }
                                        }

                                        if let TypeInfo::Adt(adt_ref, _, args) = checker.solver.get_info(*returns) {
                                            ast.used_adt_types.push((*adt_ref, args.clone()));
                                        }
                                    }

                                    ast.used_functions.push(func_ref);

                                    result = Some(FieldType::from_type_info(
                                        checker.solver.get_info(checker.local_functions[func_ref].ty),
                                        Some(checker.local_functions[func_ref].ty),
                                        &checker.solver,
                                    ));
                                }
                            }
                        } else {
                            checker.add_error(
                                TypeError::TypeNotFound {
                                    name: segment.value.name.value.0,
                                    module: current_module,
                                },
                                span,
                            );

                            return FieldType::Unknown(None);
                        }
                    }
                }

                result.unwrap_or_else(|| {
                    checker.add_error(TypeError::ModuleIsNotValue, span);

                    FieldType::Unknown(None)
                })
            }
        }
    }
}

impl TypedAST {
    pub fn is_constant_block(&self, block: BlockRef) -> bool {
        let block = &self[block].value;

        for &stmt in block.stmts.as_ref() {
            if !self.is_constant_stmt(stmt) {
                return false;
            }
        }

        block.expr.is_none_or(|expr| self.is_constant_expr(expr))
    }

    pub fn is_constant_stmt(&self, stmt: StmtRef) -> bool {
        if let &Stmt::Expr(expr) = &self[stmt] {
            self.is_constant_expr(expr)
        } else {
            false
        }
    }

    pub fn is_constant_expr(&self, expr: ExprRef) -> bool {
        match &self[expr].value {
            Expr::Literal(_) => true,
            &Expr::If { condition, block, else_block } => {
                self.is_constant_expr(condition) && self.is_constant_block(block) && else_block.is_none_or(|expr| self.is_constant_expr(expr))
            }
            &Expr::Block(block) => self.is_constant_block(block),
            Expr::Array(elements) => elements.iter().all(|&element| self.is_constant_expr(element)),
            &Expr::Binary { operator: _, lhs, rhs } => self.is_constant_expr(lhs) && self.is_constant_expr(rhs),
            _ => false,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Block {
    pub stmts: Box<[StmtRef]>,
    /// Final expression (returning value)
    pub expr: Option<ExprRef>,
}

impl IntoConstantValue for BlockRef {
    fn into_constant_value(self, checker: &TypeChecker, ast: &TypedAST, context: &mut ConstantContext) -> Result<ConstantValue, ()> {
        for &stmt in &ast[self].value.stmts {
            match &ast[stmt] {
                Stmt::Expr(expr_ref) => {
                    expr_ref.into_constant_value(checker, ast, context)?;
                }
                Stmt::VariableDecl { name, value } => {
                    let value = value.into_constant_value(checker, ast, context)?;

                    context.set_var(name, value);
                }
                Stmt::Import(_) => todo!(),
            }
        }

        ast[self]
            .value
            .expr
            .map_or(Ok(ConstantValue::Nothing), |expr| expr.into_constant_value(checker, ast, context))
    }
}

impl IntoConstantValue for ExprRef {
    fn into_constant_value(self, checker: &TypeChecker, ast: &TypedAST, context: &mut ConstantContext) -> Result<ConstantValue, ()> {
        Ok(match &ast[self].value {
            Expr::Literal(literal_expr) => match (literal_expr, checker.solver.get_info2(ast[self].ty)) {
                (&LiteralExpr::Integer(value), TypeInfo::Primitive(primitive)) => match primitive {
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
                (&LiteralExpr::Float(value), TypeInfo::Primitive(PrimitiveType::Float)) => ConstantValue::Float(value),
                (&LiteralExpr::Boolean(value), TypeInfo::Primitive(PrimitiveType::Boolean)) => ConstantValue::Boolean(value),
                (LiteralExpr::String(value), TypeInfo::Primitive(PrimitiveType::String)) => ConstantValue::String(value.clone()),
                (literal, _) => panic!(
                    "wrong type for literal: expected {} for {literal:?}",
                    checker.display_of_type(ast[self].ty, None)
                ),
            },
            Expr::If { condition, block, else_block } => {
                if condition.into_constant_value(checker, ast, context)? == ConstantValue::Boolean(true) {
                    block.into_constant_value(checker, ast, context)?
                } else if let Some(block) = else_block {
                    block.into_constant_value(checker, ast, context)?
                } else {
                    ConstantValue::Nothing
                }
            }
            Expr::Block(block_ref) => {
                context.push_frame();

                let value = block_ref.into_constant_value(checker, ast, context)?;

                context.pop_frame();

                value
            }
            Expr::Var(name) => context.search_var(name).map_or_else(|| panic!("no variable called {name}"), Clone::clone),
            Expr::Access { .. } => todo!(),
            Expr::VTableAccess { .. } => todo!(),
            Expr::Index { .. } => todo!(),
            Expr::While { condition, block } => {
                while condition.into_constant_value(checker, ast, context)? == ConstantValue::Boolean(true) {
                    block.into_constant_value(checker, ast, context)?;
                }

                ConstantValue::Nothing
            }
            Expr::Array(expr_refs) => ConstantValue::Array(
                expr_refs
                    .iter()
                    .map(|expr| expr.into_constant_value(checker, ast, context))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::Binary { operator, lhs, rhs } => match (
                lhs.into_constant_value(checker, ast, context)?,
                operator,
                rhs.into_constant_value(checker, ast, context)?,
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
                (ConstantValue::Float(a), Operator::Add, ConstantValue::Float(b)) => ConstantValue::Float(a + b),
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
                (ConstantValue::Float(a), Operator::Sub, ConstantValue::Float(b)) => ConstantValue::Float(a - b),
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
                (ConstantValue::Float(a), Operator::Mul, ConstantValue::Float(b)) => ConstantValue::Float(a * b),
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
                (ConstantValue::Float(a), Operator::Div, ConstantValue::Float(b)) => ConstantValue::Float(a / b),
                (ConstantValue::Boolean(a), Operator::And, ConstantValue::Boolean(b)) => ConstantValue::Boolean(a && b),
                (ConstantValue::Boolean(a), Operator::Or, ConstantValue::Boolean(b)) => ConstantValue::Boolean(a || b),
                (a, Operator::Equal, b) => ConstantValue::Boolean(a == b),
                (a, Operator::NotEqual, b) => ConstantValue::Boolean(a != b),
                (a, Operator::GreaterThan, b) => ConstantValue::Boolean(a > b),
                (a, Operator::LessThan, b) => ConstantValue::Boolean(a < b),
                (a, op, b) => panic!("wrong binary operation: {a:?} {op} {b:?}"),
            },
            Expr::Call { .. } => todo!(),
            Expr::Closure { .. } => todo!(),
            Expr::Construct { .. } => todo!(),
            Expr::IsPattern { .. } => todo!(),
            Expr::TypeIndex { .. } => todo!(),
            Expr::Error(_) | Expr::Nothing => ConstantValue::Nothing,
        })
    }
}
