use std::iter;

use indexmap::map::Entry;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec};
use mollie_parser::IndexTarget;
use mollie_shared::{Operator, Span};
use mollie_typing::{
    ComplexType, ComplexTypeKind, ComplexTypeRef, ComplexTypeVariant, ComplexTypeVariantRef, FieldRef, FieldType, PrimitiveType, TraitRef, TypeInfo,
    TypeInfoRef, VFuncRef, VTableRef,
};
use serde::Serialize;

use crate::{
    BlockRef, ConstantContext, ExprRef, Func, IntoConstantValue, IntoPositionedTypedAST, IntoTypedAST, StmtRef, TraitFuncRef, TypeChecker, TypedAST,
    VTableFunc, VTableFuncKind, statement::Stmt,
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
        func: ExprRef,
        args: Box<[ExprRef]>,
    },
    Closure {
        args: Box<[String]>,
        body: BlockRef,
    },
    Construct {
        ty: TypeInfoRef,
        fields: Box<[(FieldRef, String, Option<ExprRef>)]>,
    },
    ConstructEnum {
        ty: TypeInfoRef,
        variant: usize,
        fields: Option<Box<[(FieldRef, String, ExprRef)]>>,
    },
    ConstructComponent {
        ty: TypeInfoRef,
        fields: Box<[(String, ExprRef)]>,
        children: Box<[ExprRef]>,
    },
    IsPattern {
        target: ExprRef,
        pattern: IsPattern,
    },
    TypeIndex {
        target: TypeInfoRef,
        func: VFunc,
    },
    Nothing,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum IsPattern {
    Literal(ExprRef),
    EnumVariant {
        target: ComplexTypeRef,
        target_args: Box<[TypeInfoRef]>,
        variant: ComplexTypeVariantRef,
        values: Option<Box<[(FieldRef, String, Option<Self>)]>>,
    },
    TypeName {
        ty: TypeInfoRef,
        name: String,
    },
}

impl IntoTypedAST<ExprRef> for mollie_parser::LiteralExpr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<ExprRef, ()> {
        use mollie_parser::{
            LiteralExpr::{Boolean, Null, Number, SizeUnit, String},
            Number::{F32, I64},
        };

        let (literal, ty) = match self {
            SizeUnit(..) => unimplemented!(),
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
                _ => checker.solver.add_info(TypeInfo::Unknown(Some(checker.core_types.int32))),
            }),
            Number(F32(value), _) => (LiteralExpr::Float(value), checker.core_types.float),
            Boolean(value) => (LiteralExpr::Boolean(value), checker.core_types.boolean),
            String(value) => (LiteralExpr::String(value), checker.core_types.string),
            Null => unimplemented!(),
        };

        Ok(ast.add_expr(Expr::Literal(literal), ty, span))
    }
}

impl IntoTypedAST<ExprRef> for mollie_parser::Expr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<ExprRef, ()> {
        match self {
            Self::Nothing => Ok(ast.add_expr(Expr::Nothing, checker.core_types.void, span)),
            Self::Literal(literal_expr) => literal_expr.into_typed_ast(checker, ast, span),
            Self::FunctionCall(func_call_expr) => {
                let func = func_call_expr.function.into_typed_ast(checker, ast)?;
                let args = func_call_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| arg.into_typed_ast(checker, ast))
                    .collect::<Result<Box<_>, _>>()?;

                if let TypeInfo::Func(expected_args, returns) = checker.solver.get_info(ast[func].ty).clone() {
                    if let &Expr::VTableAccess { target, .. } = &ast[func].value {
                        checker.solver.unify(ast[target].ty, expected_args[0]);

                        for (arg, expected_arg) in args.iter().zip(expected_args.into_iter().skip(1)) {
                            checker.solver.unify(ast[*arg].ty, expected_arg);
                        }
                    } else {
                        for (arg, expected_arg) in args.iter().zip(expected_args) {
                            checker.solver.unify(ast[*arg].ty, expected_arg);
                        }
                    }

                    Ok(ast.add_expr(Expr::Call { func, args }, returns, span))
                } else {
                    unreachable!()
                }
            }
            Self::Node(mut node_expr) => {
                let ty = node_expr.name.into_typed_ast(checker, ast)?;
                let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                let mut children = Some(node_expr.children);

                if let TypeInfo::Complex(struct_ty, kind, args) = checker.solver.get_info(ty).clone() {
                    let fields = checker.complex_types[struct_ty].variants[ComplexTypeVariantRef::new(0)]
                        .fields
                        .iter()
                        .map(|(position, (name, field_type, _))| {
                            let ty = field_type.as_type_info(None, &checker.core_types, &mut checker.solver, args.as_ref());

                            let (name, value) = if matches!(kind, ComplexTypeKind::Component) && name == "children" {
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

                                        (prop.value.name.value.0, Some(prop.value.value))
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
                                    let value = value.into_typed_ast(checker, ast)?;

                                    checker.solver.unify(ast[value].ty, ty);

                                    Some(value)
                                }
                                None => None,
                            };

                            Ok((position, name, value))
                        })
                        .collect::<Result<_, _>>()?;

                    Ok(ast.add_expr(Expr::Construct { ty, fields }, ty, span))
                } else {
                    unimplemented!()
                }
            }
            Self::Index(index_expr) => {
                let target = index_expr.target.into_typed_ast(checker, ast)?;

                match index_expr.index.value {
                    IndexTarget::Named(property_name) => {
                        if let TypeInfo::Trait(trait_ref, type_args) = checker.solver.get_info(ast[target].ty).clone() {
                            for (func, (name, args, returns)) in checker.traits[trait_ref].1.iter() {
                                if name == &property_name.0 {
                                    let args = args
                                        .iter()
                                        .map(|arg| arg.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, type_args.as_ref()))
                                        .collect();

                                    let returns = returns.as_type_info(Some(ast[target].ty), &checker.core_types, &mut checker.solver, &[]);
                                    let ty = checker.solver.add_info(TypeInfo::Func(args, returns));

                                    return Ok(ast.add_expr(
                                        Expr::VTableAccess {
                                            target,
                                            func: VFunc::Unknown(trait_ref, func),
                                        },
                                        ty,
                                        span,
                                    ));
                                }
                            }
                        } else if let Some(vtables) = checker
                            .solver
                            .vtables
                            .get(&FieldType::from_type_info(checker.solver.get_info(ast[target].ty), &checker.solver))
                        {
                            for &vtable in vtables.values() {
                                for (func_ref, func) in checker.vtables[vtable].iter() {
                                    if func.name == property_name.0 {
                                        return Ok(ast.add_expr(
                                            Expr::VTableAccess {
                                                target,
                                                func: VFunc::Known(vtable, func_ref),
                                            },
                                            func.ty,
                                            span,
                                        ));
                                    }
                                }
                            }
                        }

                        match checker.solver.get_info(ast[target].ty) {
                            TypeInfo::Complex(ty, _, args) => {
                                let ty = *ty;
                                let args = args.clone();

                                for item in checker.complex_types[ty].instantiate(
                                    ComplexTypeVariantRef::new(0),
                                    None,
                                    &checker.core_types,
                                    &mut checker.solver,
                                    args.as_ref(),
                                ) {
                                    if item.1 == property_name.0 {
                                        return Ok(ast.add_expr(Expr::Access { target, field: item.0 }, item.2, span));
                                    }
                                }

                                panic!("can't index {ty:?}: no field called {}", property_name.0);
                            }
                            ty => unimplemented!("{ty:?}"),
                        }
                    }
                    IndexTarget::Expression(expr) => {
                        let index = expr.into_typed_ast(checker, ast, index_expr.index.span)?;

                        checker.solver.unify(ast[index].ty, checker.core_types.uint_size);

                        match checker.solver.get_info(ast[target].ty) {
                            TypeInfo::Array(element, _) => Ok(ast.add_expr(Expr::Index { target, index }, *element, span)),
                            _ => unimplemented!(),
                        }
                    }
                }
            }
            Self::Binary(binary_expr) => {
                let lhs = binary_expr.lhs.into_typed_ast(checker, ast)?;
                let rhs = binary_expr.rhs.into_typed_ast(checker, ast)?;

                checker.solver.unify(ast[rhs].ty, ast[lhs].ty);

                let ty = if matches!(
                    binary_expr.operator.value,
                    Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan
                ) {
                    checker.core_types.boolean
                } else {
                    ast[lhs].ty
                };

                Ok(ast.add_expr(
                    Expr::Binary {
                        operator: binary_expr.operator.value,
                        lhs,
                        rhs,
                    },
                    ty,
                    span,
                ))
            }
            Self::TypeIndex(type_index_expr) => {
                let target_field = type_index_expr.target.into_typed_ast(checker, ast)?;
                let target = target_field.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                if let Some(vtables) = checker.solver.vtables.get(&target_field) {
                    for &vtable in vtables.values() {
                        for (func_ref, func) in checker.vtables[vtable].iter() {
                            if func.name == type_index_expr.index.value.0 {
                                return Ok(ast.add_expr(
                                    Expr::TypeIndex {
                                        target,
                                        func: VFunc::Known(vtable, func_ref),
                                    },
                                    func.ty,
                                    span,
                                ));
                            }
                        }
                    }
                }

                unimplemented!()
            }
            Self::Array(array_expr) => {
                let element = checker.solver.add_info(TypeInfo::Unknown(None));
                let mut elements = Vec::with_capacity(array_expr.elements.capacity());

                for arr_element in array_expr.elements {
                    let arr_element = arr_element.into_typed_ast(checker, ast)?;

                    checker.solver.unify(element, ast[arr_element].ty);

                    elements.push(arr_element);
                }

                let size = elements.len();

                Ok(ast.add_expr(
                    Expr::Array(elements.into_boxed_slice()),
                    checker.solver.add_info(TypeInfo::Array(element, Some(size))),
                    span,
                ))
            }
            Self::IfElse(if_else_expr) => {
                checker.solver.push_frame();

                let condition_expected = checker.core_types.boolean;
                let condition = if_else_expr.condition.into_typed_ast(checker, ast)?;

                checker.solver.unify(condition_expected, ast[condition].ty);

                let block = if_else_expr.block.into_typed_ast(checker, ast)?;

                checker.solver.pop_frame();

                let ty = ast[block].ty;

                let else_block = if let Some(else_block) = if_else_expr.else_block {
                    let block = else_block.into_typed_ast(checker, ast)?;

                    checker.solver.unify(ast[block].ty, ty);

                    Some(block)
                } else {
                    None
                };

                Ok(ast.add_expr(Expr::If { condition, block, else_block }, ty, span))
            }
            Self::While(while_expr) => {
                checker.solver.push_frame();

                let condition_expected = checker.core_types.boolean;
                let condition = while_expr.condition.into_typed_ast(checker, ast)?;

                checker.solver.unify(ast[condition].ty, condition_expected);

                let ty = checker.core_types.void;
                let block = while_expr.block.into_typed_ast(checker, ast)?;

                checker.solver.pop_frame();
                checker.solver.unify(ast[block].ty, ty);

                Ok(ast.add_expr(Expr::While { condition, block }, ty, span))
            }
            Self::Block(block_expr) => {
                checker.solver.push_frame();

                let block = block_expr.into_typed_ast(checker, ast, span)?;
                let ty = ast[block].ty;

                checker.solver.pop_frame();

                Ok(ast.add_expr(Expr::Block(block), ty, span))
            }
            Self::EnumPath(enum_path_expr) => {
                let target = *checker.name_to_complex_type.get(&enum_path_expr.target.value.0).unwrap();
                let mut applied_generics = enum_path_expr
                    .index
                    .value
                    .generics
                    .into_iter()
                    .map(|generic| generic.into_typed_ast(checker, ast).ok())
                    .collect::<Option<Vec<_>>>()
                    .unwrap();

                let target_ty = &checker.complex_types[target];
                let mut current_variant = None;
                let mut resulting_fields = None;
                let generic_args = (0..target_ty.generics)
                    .rev()
                    .map(|generic| {
                        applied_generics
                            .pop()
                            .unwrap_or(FieldType::Generic(generic))
                            .as_type_info(None, &checker.core_types, &mut checker.solver, &[])
                    })
                    .rev()
                    .collect::<Box<_>>();

                if matches!(target_ty.kind, ComplexTypeKind::Enum) {
                    for (variant, variant_value) in target_ty.variants.raw.iter().enumerate() {
                        if variant_value.name.as_deref() == Some(enum_path_expr.index.value.name.value.0.as_str()) {
                            current_variant.replace(ComplexTypeVariantRef::new(variant));

                            break;
                        }
                    }

                    if let Some(current_variant) = current_variant
                        && let Some(fields) = enum_path_expr.properties
                    {
                        let mut new_fields = Vec::new();

                        for value in fields.value {
                            let name = value.value.name.value.0;

                            let field = checker.complex_types[target].variants[current_variant]
                                .fields
                                .iter()
                                .find(|(_, (field_name, ..))| field_name == &name)
                                .map(|(field, (_, ty, _))| (field, ty.as_type_info(None, &checker.core_types, &mut checker.solver, &generic_args)));

                            if let Some((field, ty)) = field {
                                let value = value.value.value.into_typed_ast(checker, ast)?;

                                checker.solver.unify(ast[value].ty, ty);

                                new_fields.push((field, name, value));
                            }
                        }

                        resulting_fields.replace(new_fields);
                    }
                }

                let ty = checker.solver.add_info(TypeInfo::Complex(target, ComplexTypeKind::Enum, generic_args));

                Ok(ast.add_expr(
                    Expr::ConstructEnum {
                        ty,
                        variant: current_variant.unwrap().index(),
                        fields: resulting_fields.map(|value| value.into_boxed_slice()),
                    },
                    ty,
                    span,
                ))
            }
            Self::Is(is_expr) => {
                let target = is_expr.target.into_typed_ast(checker, ast)?;
                let pattern = is_expr
                    .pattern
                    .span
                    .wrap((ast[target].ty, is_expr.pattern.value))
                    .into_typed_ast(checker, ast)?;

                Ok(ast.add_expr(Expr::IsPattern { target, pattern }, checker.core_types.boolean, span))
            }
            Self::Closure(closure_expr) => {
                let mut args = Vec::with_capacity(closure_expr.args.value.capacity());
                let mut arg_types = Vec::with_capacity(closure_expr.args.value.capacity());

                checker.solver.push_frame();

                for arg in closure_expr.args.value {
                    let ty = checker.solver.add_info(TypeInfo::Unknown(None));

                    checker.solver.add_var(&arg.value.0, ty);

                    args.push(arg.value.0);
                    arg_types.push(ty);
                }

                let body = closure_expr.body.into_typed_ast(checker, ast)?;
                let ty = checker.solver.add_info(TypeInfo::Func(arg_types.into_boxed_slice(), ast[body].ty));

                checker.solver.pop_frame();

                Ok(ast.add_expr(
                    Expr::Closure {
                        args: args.into_boxed_slice(),
                        body,
                    },
                    ty,
                    span,
                ))
            }
            Self::Ident(ident) => {
                let ty = /*checker
                    .types
                    .get_named_type_ref(&ident.0)
                    .map(|ty| match unsafe { checker.types.get_type(ty).unwrap_unchecked() }.variant.as_complex() {
                        Some(mollie_typing::ComplexType::Array(arr)) => {
                            let element = checker
                                .solver
                                .add_info(TypeInfo::Type(checker.types.ref_of_type(&arr.element).unwrap()));

                            checker.solver.add_info(TypeInfo::Array(element))
                        }
                        Some(mollie_typing::ComplexType::Function(func)) => {
                            let args = func
                                .args
                                .iter()
                                .map(|arg| checker.solver.add_info(TypeInfo::Type(checker.types.ref_of_type(arg).unwrap())))
                                .collect();

                            let output = checker
                                .solver
                                .add_info(TypeInfo::Type(checker.types.ref_of_type(func.returns.as_ref()).unwrap()));

                            checker.solver.add_info(TypeInfo::Func(args, output))
                        }
                        _ => checker.solver.add_info(TypeInfo::Type(ty)),
                    })
                    .or_else(|| */ checker.solver.get_var(&ident.0).map(|var| var.ty)/* ) */
                    .unwrap();

                Ok(ast.add_expr(Expr::Var(ident.0), ty, span))
            }
            Self::This => Ok(ast.add_expr(Expr::Var("self".into()), checker.solver.get_var("self").unwrap().ty, span)),
        }
    }
}

impl IntoTypedAST<BlockRef> for mollie_parser::BlockExpr {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<BlockRef, ()> {
        let stmts = self.stmts.into_iter().try_fold(Vec::new(), |mut stmts, stmt| {
            if let Some(stmt) = stmt.into_typed_ast(checker, ast)? {
                stmts.push(stmt);
            }

            Ok(stmts)
        })?;

        let (expr, ty) = match self.final_stmt {
            Some(stmt) => {
                if let mollie_parser::Stmt::Expression(expr) = stmt.value {
                    let expr = expr.into_typed_ast(checker, ast, stmt.span)?;

                    (Some(expr), ast[expr].ty)
                } else {
                    (None, checker.core_types.void)
                }
            }
            None => (None, checker.core_types.void),
        };

        Ok(ast.add_block(
            Block {
                stmts: stmts.into_boxed_slice(),
                expr,
            },
            ty,
            span,
        ))
    }
}

impl IntoTypedAST<IsPattern> for (TypeInfoRef, mollie_parser::IsPattern) {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<IsPattern, ()> {
        Ok(match self.1 {
            mollie_parser::IsPattern::Literal(literal_expr) => IsPattern::Literal(literal_expr.into_typed_ast(checker, ast, span)?),
            mollie_parser::IsPattern::Enum { target, index, values } => {
                let target = *checker.name_to_complex_type.get(&target.value.0).unwrap();
                let mut current_variant = None;
                let mut resulting_values = None;
                let target_args = if matches!(checker.complex_types[target].kind, ComplexTypeKind::Enum) {
                    for (variant, variant_data) in checker.complex_types[target].variants.iter() {
                        if variant_data.name.as_ref() == Some(&index.value.name.value.0) {
                            current_variant.replace(variant);

                            break;
                        }
                    }

                    let mut applied_generics = index
                        .value
                        .generics
                        .into_iter()
                        .map(|generic| generic.into_typed_ast(checker, ast).ok())
                        .collect::<Option<Vec<_>>>()
                        .unwrap();

                    let ty = &checker.complex_types[target];
                    let generic_args = (0..ty.generics)
                        .rev()
                        .map(|generic| {
                            applied_generics
                                .pop()
                                .unwrap_or(FieldType::Generic(generic))
                                .as_type_info(None, &checker.core_types, &mut checker.solver, &[])
                        })
                        .rev()
                        .collect::<Box<_>>();

                    if let Some(current_variant) = current_variant
                        && let Some(values) = values
                    {
                        let fields = ty
                            .instantiate(current_variant, None, &checker.core_types, &mut checker.solver, &generic_args)
                            .map(|(field, name, ty)| (field, name.to_string(), ty))
                            .collect::<Box<[_]>>();

                        let mut new_values = Vec::new();

                        for value in values.value {
                            if let Some(prop) = fields.iter().find(|prop| prop.1 == value.value.name.value.0) {
                                let pattern = if let Some(value) = value.value.value {
                                    Some(value.span.wrap((prop.2, value.value)).into_typed_ast(checker, ast)?)
                                } else {
                                    None
                                };

                                if pattern.is_none() {
                                    checker.solver.add_var(&prop.1, prop.2);
                                }

                                new_values.push((prop.0, value.value.name.value.0, pattern));
                            }
                        }

                        resulting_values.replace(new_values);
                    }

                    if let TypeInfo::Complex(_, _, type_args) = checker.solver.get_info(self.0) {
                        for (expected, got) in type_args.clone().into_iter().zip(&generic_args) {
                            checker.solver.unify(*got, expected);
                        }
                    }

                    generic_args
                } else {
                    Box::default()
                };

                IsPattern::EnumVariant {
                    target,
                    target_args,
                    variant: current_variant.unwrap(),
                    values: resulting_values.map(|v| v.into_boxed_slice()),
                }
            }
            mollie_parser::IsPattern::TypeName { ty, name } => {
                let ty = ty.into_typed_ast(checker, ast)?;
                let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                checker.solver.add_var(&name.value.0, ty);

                IsPattern::TypeName { ty, name: name.value.0 }
            }
        })
    }
}

impl IntoTypedAST<Option<StmtRef>> for mollie_parser::Stmt {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<Option<StmtRef>, ()> {
        match self {
            Self::Expression(expr) => {
                let expr = expr.into_typed_ast(checker, ast, span)?;

                Ok(Some(ast.add_stmt(Stmt::Expr(expr))))
            }
            Self::VariableDecl(variable_decl) => {
                let value = variable_decl.value.into_typed_ast(checker, ast)?;
                let value_ty = ast[value].ty;

                if let Some(ty) = variable_decl.ty {
                    let ty = ty.into_typed_ast(checker, ast)?;
                    let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                    assert!(!checker.solver.contains_unknown(ty), "explicit type can't have non-explicit generics");

                    checker.solver.unify(value_ty, ty);
                    checker.solver.add_var(&variable_decl.name.value.0, ty);
                } else {
                    checker.solver.add_var(&variable_decl.name.value.0, value_ty);
                }

                Ok(Some(ast.add_stmt(Stmt::VariableDecl {
                    name: variable_decl.name.value.0,
                    value,
                })))
            }
            Self::StructDecl(struct_decl) => {
                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), index);
                }

                let mut variants = IndexVec::new();
                let mut properties = IndexVec::new();

                for property in struct_decl.properties.value {
                    let name = property.value.name.value.0;
                    let ty = property.value.ty.into_typed_ast(checker, ast)?;
                    let constant = match property.value.default_value {
                        Some(value) => {
                            let expected_ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                            let value = value.into_typed_ast(checker, ast)?;

                            checker.solver.unify(ast[value].ty, expected_ty);

                            Some(value.into_constant_value(checker, ast, &mut ConstantContext::default())?)
                        }
                        None => None,
                    };

                    properties.push((name, ty, constant));
                }

                variants.push(ComplexTypeVariant {
                    name: None,
                    discriminant: 0,
                    fields: properties.into_boxed_slice(),
                });

                for name in &struct_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let complex_type = ComplexTypeRef::new(checker.complex_types.len());

                checker.name_to_complex_type.insert(struct_decl.name.value.name.value.0.clone(), complex_type);
                checker.complex_types.push(ComplexType {
                    name: Some(struct_decl.name.value.name.value.0),
                    kind: ComplexTypeKind::Struct,
                    generics: struct_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                Ok(None)
            }
            Self::ComponentDecl(component_decl) => {
                for (index, name) in component_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), index);
                }

                let mut variants = IndexVec::new();
                let mut fields = IndexVec::with_capacity(component_decl.properties.len());

                for property in component_decl.properties {
                    let name = property.value.name.value.0;
                    let ty = property.value.ty.into_typed_ast(checker, ast)?;
                    let constant = match property.value.default_value {
                        Some(value) => {
                            let expected_ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);
                            let value = value.into_typed_ast(checker, ast)?;

                            checker.solver.unify(ast[value].ty, expected_ty);

                            Some(value.into_constant_value(checker, ast, &mut ConstantContext::default())?)
                        }
                        None => None,
                    };

                    fields.push((name, ty, constant));
                }

                variants.push(ComplexTypeVariant {
                    name: None,
                    discriminant: 0,
                    fields: fields.into_boxed_slice(),
                });

                for name in &component_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let complex_type = ComplexTypeRef::new(checker.complex_types.len());

                checker
                    .name_to_complex_type
                    .insert(component_decl.name.value.name.value.0.clone(), complex_type);
                checker.complex_types.push(ComplexType {
                    name: Some(component_decl.name.value.name.value.0),
                    kind: ComplexTypeKind::Component,
                    generics: component_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                Ok(None)
            }
            Self::TraitDecl(trait_decl) => {
                for (index, name) in trait_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), index);
                }

                let mut functions = IndexVec::with_capacity(trait_decl.functions.value.len());

                for function in trait_decl.functions.value {
                    let name = function.value.name.value.0;
                    let mut args = Vec::with_capacity(function.value.args.len() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        args.push(FieldType::This);
                    }

                    for arg in function.value.args {
                        args.push(arg.value.ty.into_typed_ast(checker, ast)?);
                    }

                    let output = function
                        .value
                        .returns
                        .map_or(Ok(FieldType::Primitive(PrimitiveType::Void)), |returns| returns.into_typed_ast(checker, ast))?;

                    functions.push((name, args, output));
                }

                for name in &trait_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let trait_ref = TraitRef::new(checker.traits.len());

                checker.name_to_trait.insert(trait_decl.name.value.name.value.0, trait_ref);
                checker.traits.push((trait_decl.name.value.generics.len(), functions));

                Ok(None)
            }
            Self::EnumDecl(enum_decl) => {
                for (index, name) in enum_decl.name.value.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), index);
                }

                let mut variants = IndexVec::new();

                for (discriminant, variant) in enum_decl.variants.value.into_iter().enumerate() {
                    let name = Some(variant.value.name.value.0);
                    let mut fields = IndexVec::with_capacity(variant.value.properties.as_ref().map(|value| value.value.len()).unwrap_or_default());

                    if let Some(properties) = variant.value.properties {
                        for property in properties.value {
                            let name = property.value.name.value.0;
                            let ty = property.value.ty.into_typed_ast(checker, ast)?;

                            fields.push((name, ty, None));
                        }
                    }

                    variants.push(ComplexTypeVariant {
                        name,
                        discriminant,
                        fields: fields.into_boxed_slice(),
                    });
                }

                for name in &enum_decl.name.value.generics {
                    checker.available_generics.remove(&name.value.0);
                }

                let complex_type = ComplexTypeRef::new(checker.complex_types.len());

                checker.name_to_complex_type.insert(enum_decl.name.value.name.value.0.clone(), complex_type);
                checker.complex_types.push(ComplexType {
                    name: Some(enum_decl.name.value.name.value.0),
                    kind: ComplexTypeKind::Enum,
                    generics: enum_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                Ok(None)
            }
            Self::FuncDecl(func_decl) => {
                checker.solver.push_frame();

                let mut arg_names = Vec::with_capacity(func_decl.args.capacity());
                let mut args = Vec::with_capacity(func_decl.args.capacity());

                for arg in func_decl.args {
                    let ty = arg.value.ty.into_typed_ast(checker, ast)?;
                    let ty = ty.as_type_info(None, &checker.core_types, &mut checker.solver, &[]);

                    checker.solver.add_var(&arg.value.name.value.0, ty);

                    arg_names.push(arg.value.name.value.0);
                    args.push(ty);
                }

                let returns = if let Some(returns) = func_decl.returns {
                    returns
                        .into_typed_ast(checker, ast)?
                        .as_type_info(None, &checker.core_types, &mut checker.solver, &[])
                } else {
                    checker.core_types.void
                };

                let body = func_decl.body.into_typed_ast(checker, ast)?;

                checker.solver.unify(ast[body].ty, returns);
                checker.solver.pop_frame();

                let ty = checker.solver.add_info(TypeInfo::Func(args.into_boxed_slice(), returns));

                checker.solver.add_var(&func_decl.name.value.0, ty);
                checker.local_functions.push(Func {
                    name: func_decl.name.value.0,
                    arg_names,
                    ty,
                    kind: VTableFuncKind::Local(body),
                });

                Ok(None)
            }
            Self::Impl(mut implementation) => {
                for (index, name) in implementation.generics.iter().enumerate() {
                    checker.available_generics.insert(name.value.0.clone(), index);
                }

                let mut applied_generics = Vec::new();

                let trait_ref = match implementation.trait_name {
                    Some(trait_name) => {
                        applied_generics = trait_name
                            .value
                            .generics
                            .into_iter()
                            .map(|generic| {
                                generic
                                    .into_typed_ast(checker, ast)
                                    .map(|v| v.as_type_info(None, &checker.core_types, &mut checker.solver, &[]))
                            })
                            .collect::<Result<_, _>>()?;

                        checker
                            .name_to_trait
                            .get(&trait_name.value.name.value.0)
                            .copied()
                            .map(|t| (trait_name.value.name.value.0, t))
                    }
                    None => None,
                };

                let target = implementation.target.into_typed_ast(checker, ast)?;
                let target_info = target.as_type_info(None, &checker.core_types, &mut checker.solver, &applied_generics);
                let mut functions = IndexVec::with_capacity(
                    implementation
                        .functions
                        .value
                        .capacity()
                        .max(trait_ref.as_ref().map(|(_, t)| checker.traits[*t].1.len()).unwrap_or_default()),
                );

                if let Some((trait_name, trait_ref)) = &trait_ref {
                    let trait_functions = checker.traits[*trait_ref]
                        .1
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
                                args.push(target_info);
                            }

                            for arg in function.value.args {
                                let ty = arg.value.ty.into_typed_ast(checker, ast)?;
                                let ty = ty.as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &applied_generics);

                                checker.solver.add_var(&arg.value.name.value.0, ty);

                                arg_names.push(arg.value.name.value.0);
                                args.push(ty);
                            }

                            let returns = if let Some(returns) = function.value.returns {
                                returns.into_typed_ast(checker, ast)?.as_type_info(
                                    Some(target_info),
                                    &checker.core_types,
                                    &mut checker.solver,
                                    &applied_generics,
                                )
                            } else {
                                checker.core_types.void
                            };

                            let body = function.value.body.into_typed_ast(checker, ast)?;

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
                        args.push(target_info);
                    }

                    for arg in function.value.args {
                        let ty = arg.value.ty.into_typed_ast(checker, ast)?;
                        let ty = ty.as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &applied_generics);

                        checker.solver.add_var(&arg.value.name.value.0, ty);

                        arg_names.push(arg.value.name.value.0);
                        args.push(ty);
                    }

                    let returns = if let Some(returns) = function.value.returns {
                        returns
                            .into_typed_ast(checker, ast)?
                            .as_type_info(Some(target_info), &checker.core_types, &mut checker.solver, &applied_generics)
                    } else {
                        checker.core_types.void
                    };

                    let body = function.value.body.into_typed_ast(checker, ast)?;

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
                                    checker.vtables[*entry.get()].push(func);
                                }
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(checker.vtables.insert(functions));
                        }
                    },
                    Entry::Vacant(entry) => {
                        entry.insert(iter::once((trait_ref, checker.vtables.insert(functions))).collect());
                    }
                }

                Ok(None)
            }
            Self::Import(_) => todo!(),
        }
    }
}

impl IntoTypedAST<FieldType> for mollie_parser::CustomType {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, _: Span) -> Result<FieldType, ()> {
        Ok(checker.available_generics.get(&self.name.value.0).copied().map_or_else(
            || {
                let mut applied_generics = self
                    .generics
                    .into_iter()
                    .map(|generic| generic.into_typed_ast(checker, ast).ok())
                    .collect::<Option<Vec<_>>>()
                    .unwrap();

                checker
                    .name_to_trait
                    .get(&self.name.value.0)
                    .map(|&ty| {
                        FieldType::Trait(
                            ty,
                            (0..checker.traits[ty].0)
                                .rev()
                                .map(|generic| applied_generics.pop().unwrap_or(FieldType::Generic(generic)))
                                .rev()
                                .collect(),
                        )
                    })
                    .or_else(|| {
                        checker.name_to_complex_type.get(&self.name.value.0).map(|&ty| {
                            FieldType::Complex(
                                ty,
                                checker.complex_types[ty].kind,
                                (0..checker.complex_types[ty].generics)
                                    .rev()
                                    .map(|generic| applied_generics.pop().unwrap_or(FieldType::Generic(generic)))
                                    .rev()
                                    .collect(),
                            )
                        })
                    })
                    .unwrap()
            },
            FieldType::Generic,
        ))
    }
}

impl IntoTypedAST<FieldType> for mollie_parser::Type {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<FieldType, ()> {
        Ok(match self {
            Self::Primitive(primitive_type) => {
                use mollie_parser::PrimitiveType::{
                    Boolean, Component, Float, Int8, Int16, Int32, Int64, IntSize, String, UInt8, UInt16, UInt32, UInt64, UIntSize, Void,
                };

                match primitive_type {
                    IntSize => FieldType::Primitive(PrimitiveType::ISize),
                    Int64 => FieldType::Primitive(PrimitiveType::I64),
                    Int32 => FieldType::Primitive(PrimitiveType::I32),
                    Int16 => FieldType::Primitive(PrimitiveType::I16),
                    Int8 => FieldType::Primitive(PrimitiveType::I8),
                    UIntSize => FieldType::Primitive(PrimitiveType::USize),
                    UInt64 => FieldType::Primitive(PrimitiveType::U64),
                    UInt32 => FieldType::Primitive(PrimitiveType::U32),
                    UInt16 => FieldType::Primitive(PrimitiveType::U16),
                    UInt8 => FieldType::Primitive(PrimitiveType::U8),
                    Float => FieldType::Primitive(PrimitiveType::Float),
                    Boolean => FieldType::Primitive(PrimitiveType::Boolean),
                    String => FieldType::Primitive(PrimitiveType::String),
                    Component => FieldType::Primitive(PrimitiveType::Component),
                    Void => FieldType::Primitive(PrimitiveType::Void),
                }
            }
            Self::Custom(custom_type) => custom_type.into_typed_ast(checker, ast, span)?,
            Self::Array(element, size) => {
                let element = element.into_typed_ast(checker, ast)?;

                FieldType::Array(Box::new(element), size.map(|size| size.value))
            }
            Self::OneOf(_) => todo!(),
            Self::Func(args, output) => {
                let args = args.into_iter().map(|arg| arg.into_typed_ast(checker, ast)).collect::<Result<_, _>>()?;
                let output = if let Some(output) = output {
                    output.into_typed_ast(checker, ast)?
                } else {
                    FieldType::Primitive(PrimitiveType::Void)
                };

                FieldType::Func(args, Box::new(output))
            }
        })
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
                    PrimitiveType::ISize => ConstantValue::ISize(value.try_into().map_err(|_| ())?),
                    PrimitiveType::I64 => ConstantValue::I64(value),
                    PrimitiveType::I32 => ConstantValue::I32(value.try_into().map_err(|_| ())?),
                    PrimitiveType::I16 => ConstantValue::I16(value.try_into().map_err(|_| ())?),
                    PrimitiveType::I8 => ConstantValue::I8(value.try_into().map_err(|_| ())?),
                    PrimitiveType::USize => ConstantValue::USize(value.try_into().map_err(|_| ())?),
                    PrimitiveType::U64 => ConstantValue::U64(value.cast_unsigned()),
                    PrimitiveType::U32 => ConstantValue::U32(value.try_into().map_err(|_| ())?),
                    PrimitiveType::U16 => ConstantValue::U16(value.try_into().map_err(|_| ())?),
                    PrimitiveType::U8 => ConstantValue::U8(value.try_into().map_err(|_| ())?),
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
            Expr::ConstructEnum { .. } => todo!(),
            Expr::ConstructComponent { .. } => todo!(),
            Expr::IsPattern { .. } => todo!(),
            Expr::TypeIndex { .. } => todo!(),
            Expr::Nothing => ConstantValue::Nothing,
        })
    }
}
