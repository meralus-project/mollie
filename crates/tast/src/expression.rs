use mollie_parser::IndexTarget;
use mollie_shared::{Operator, Span};
use mollie_typing::{
    ComplexType, StructType, Type, TypeVariant,
    resolver::{TypeInfo, TypeInfoRef, TypeRef, VFuncRef},
};
use serde::Serialize;

use crate::{BlockRef, ExprRef, IntoPositionedTypedAST, IntoTypedAST, StmtRef, TypeChecker, Typed, TypedAST, statement::Stmt};

#[derive(Debug, Serialize)]
pub enum LiteralExpr {
    Integer(i64),
    Float(f32),
    String(String),
    Boolean(bool),
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
        index: String,
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
        fields: Box<[(String, ExprRef)]>,
    },
    ConstructEnum {
        ty: TypeInfoRef,
        variant: usize,
        fields: Option<Box<[(String, ExprRef)]>>,
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
        func: VFuncRef,
    },
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum IsPattern {
    Literal(ExprRef),
    EnumVariant {
        target: TypeRef,
        variant: usize,
        values: Option<Box<[(String, Option<Self>)]>>,
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
            Self::Literal(literal_expr) => literal_expr.into_typed_ast(checker, ast, span),
            Self::FunctionCall(func_call_expr) => {
                let func = func_call_expr.function.into_typed_ast(checker, ast)?;
                let args = func_call_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| arg.into_typed_ast(checker, ast))
                    .collect::<Result<Box<_>, _>>()?;

                let output = checker.solver.add_info(TypeInfo::Unknown(None));
                let got = checker.solver.add_info(TypeInfo::Func(args.iter().map(|&expr| ast[expr].ty).collect(), output));

                checker.solver.unify(ast[func].ty, got);

                Ok(ast.add_expr(Expr::Call { func, args }, output, span))
            }
            Self::Node(mut node_expr) => {
                let ty = node_expr.name.into_typed_ast(checker, ast)?;

                if let TypeInfo::Struct(struct_ty, args) = checker.solver.get_info(ty).clone() {
                    let fields = checker
                        .types
                        .get_type(struct_ty)
                        .cloned()
                        .and_then(|ty| {
                            ty.variant.as_struct().and_then(|structure| {
                                structure
                                    .properties
                                    .iter()
                                    .map(|(name, ty)| {
                                        let prop = node_expr.properties.iter().position(|prop| &prop.value.name.value.0 == name)?;
                                        let prop = node_expr.properties.remove(prop);

                                        prop.value.value.into_typed_ast(checker, ast).ok().map(|value| {
                                            if let &TypeVariant::Generic(idx) = &ty.variant {
                                                checker.solver.unify(*args.get(idx).unwrap(), ast[value].ty);
                                            }

                                            (prop.value.name.value.0, value)
                                        })
                                    })
                                    .collect::<Option<Box<_>>>()
                            })
                        })
                        .unwrap();

                    Ok(ast.add_expr(Expr::Construct { ty, fields }, ty, span))
                } else {
                    unimplemented!()
                }
            }
            Self::Index(index_expr) => {
                let target = index_expr.target.into_typed_ast(checker, ast)?;
                let (expr, ty) = match index_expr.index.value {
                    IndexTarget::Named(property_name) => match checker.solver.get_info(ast[target].ty) {
                        TypeInfo::Struct(ty, args) => checker
                            .types
                            .get_type(*ty)
                            .and_then(|ty| ty.variant.as_struct())
                            .map(|structure| {
                                let mut items = structure.properties.iter();

                                loop {
                                    if let Some(item) = items.next() {
                                        if item.0 == property_name.0 {
                                            break (
                                                Expr::Access {
                                                    target,
                                                    index: property_name.0.clone(),
                                                },
                                                match &item.1.variant {
                                                    TypeVariant::Primitive(primitive_type) => match primitive_type {
                                                        mollie_typing::PrimitiveType::Any => todo!(),
                                                        mollie_typing::PrimitiveType::ISize => checker.core_types.int_size,
                                                        mollie_typing::PrimitiveType::I64 => checker.core_types.int64,
                                                        mollie_typing::PrimitiveType::I32 => checker.core_types.int32,
                                                        mollie_typing::PrimitiveType::I16 => checker.core_types.int16,
                                                        mollie_typing::PrimitiveType::I8 => checker.core_types.int8,
                                                        mollie_typing::PrimitiveType::USize => checker.core_types.uint_size,
                                                        mollie_typing::PrimitiveType::U64 => checker.core_types.uint64,
                                                        mollie_typing::PrimitiveType::U32 => checker.core_types.uint32,
                                                        mollie_typing::PrimitiveType::U16 => checker.core_types.uint16,
                                                        mollie_typing::PrimitiveType::U8 => checker.core_types.uint8,
                                                        mollie_typing::PrimitiveType::Float => checker.core_types.float,
                                                        mollie_typing::PrimitiveType::Boolean => checker.core_types.boolean,
                                                        mollie_typing::PrimitiveType::String => checker.core_types.string,
                                                        mollie_typing::PrimitiveType::Component => checker.core_types.component,
                                                        mollie_typing::PrimitiveType::Void => checker.core_types.void,
                                                        mollie_typing::PrimitiveType::Null => todo!(),
                                                    },
                                                    TypeVariant::Complex(complex_type) => todo!(),
                                                    TypeVariant::Trait(_) => todo!(),
                                                    &TypeVariant::Generic(generic) => *args.get(generic).unwrap(),
                                                    TypeVariant::Ref { ty, mutable } => todo!(),
                                                    TypeVariant::This => todo!(),
                                                    TypeVariant::Unknown => todo!(),
                                                },
                                            );
                                        }
                                    } else {
                                        unimplemented!();
                                    }
                                }
                            })
                            .unwrap(),
                        ty => unimplemented!("{ty:?}"),
                    },
                    IndexTarget::Expression(expr) => {
                        let index = expr.into_typed_ast(checker, ast, index_expr.index.span)?;

                        checker.solver.unify(checker.core_types.uint_size, ast[index].ty);

                        match checker.solver.get_info(ast[target].ty) {
                            TypeInfo::Array(element, _) => (Expr::Index { target, index }, *element),
                            _ => unimplemented!(),
                        }
                    }
                };

                Ok(ast.add_expr(expr, ty, span))
            }
            Self::Binary(binary_expr) => {
                let lhs = binary_expr.lhs.into_typed_ast(checker, ast)?;
                let rhs = binary_expr.rhs.into_typed_ast(checker, ast)?;

                checker.solver.unify(ast[lhs].ty, ast[rhs].ty);

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
                let target = type_index_expr.target.into_typed_ast(checker, ast)?;
                let ty = checker.solver.get_as_type(target, &checker.types).unwrap();

                let vfunc = checker.types.find_vtable_func(&ty.variant, &type_index_expr.index.value.0).unwrap();
                let vfunc_ty = checker.types.get_vfunc_type(vfunc).unwrap();

                if let Some(func) = vfunc_ty.variant.as_function() {
                    let args = func
                        .args
                        .iter()
                        .map(|arg| checker.solver.add_info(TypeInfo::Type(checker.types.ref_of_type(arg).unwrap())))
                        .collect();

                    let output = checker
                        .solver
                        .add_info(TypeInfo::Type(checker.types.ref_of_type(func.returns.as_ref()).unwrap()));
                    let ty = checker.solver.add_info(TypeInfo::Func(args, output));

                    Ok(ast.add_expr(Expr::TypeIndex { target, func: vfunc }, ty, span))
                } else {
                    unimplemented!()
                }
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

                    checker.solver.unify(ty, ast[block].ty);

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

                checker.solver.unify(condition_expected, ast[condition].ty);

                let ty = checker.core_types.void;
                let block = while_expr.block.into_typed_ast(checker, ast)?;

                checker.solver.pop_frame();
                checker.solver.unify(ty, ast[block].ty);

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
                let target = checker.types.get_named_type_ref(enum_path_expr.target.value.0).unwrap();
                let mut current_variant = None;
                let mut resulting_fields = None;

                if let Some(ty) = checker.types.get_type(target).cloned()
                    && let Some(enumeration) = ty.variant.as_enum()
                {
                    for (variant, (name, _)) in enumeration.variants.iter().enumerate() {
                        if name == &enum_path_expr.index.value.name.value.0 {
                            current_variant.replace(variant);

                            break;
                        }
                    }

                    if let Some(current_variant) = current_variant
                        && let Some(fields) = enum_path_expr.properties
                    {
                        let variant = &enumeration.variants[current_variant].1;

                        if let Some(props) = &variant.properties {
                            let mut new_fields = Vec::new();

                            for value in fields.value {
                                let name = value.value.name.value.0;

                                if let Some(prop) = props.iter().find(|prop| prop.0 == name) {
                                    let value = value.value.value.into_typed_ast(checker, ast)?;

                                    let expected = checker.types.ref_of_type(&prop.1).unwrap();
                                    let expected = checker.solver.add_info(TypeInfo::Type(expected));

                                    checker.solver.unify(expected, ast[value].ty);

                                    new_fields.push((name, value));
                                }
                            }

                            resulting_fields.replace(new_fields);
                        }
                    }
                }

                let ty = checker.solver.add_info(TypeInfo::Type(target));

                Ok(ast.add_expr(
                    Expr::ConstructEnum {
                        ty,
                        variant: current_variant.unwrap(),
                        fields: resulting_fields.map(|value| value.into_boxed_slice()),
                    },
                    ty,
                    span,
                ))
            }
            Self::Is(is_expr) => {
                let target = is_expr.target.into_typed_ast(checker, ast)?;
                let pattern = is_expr.pattern.into_typed_ast(checker, ast)?;

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

impl IntoTypedAST<IsPattern> for mollie_parser::IsPattern {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<IsPattern, ()> {
        Ok(match self {
            Self::Literal(literal_expr) => IsPattern::Literal(literal_expr.into_typed_ast(checker, ast, span)?),
            Self::Enum { target, index, values } => {
                let target = checker.types.get_named_type_ref(target.value.0).unwrap();
                let mut current_variant = None;
                let mut resulting_values = None;

                if let Some(ty) = checker.types.get_type(target).cloned()
                    && let Some(enumeration) = ty.variant.as_enum()
                {
                    for (variant, (name, _)) in enumeration.variants.iter().enumerate() {
                        if name == &index.value.name.value.0 {
                            current_variant.replace(variant);

                            break;
                        }
                    }

                    if let Some(current_variant) = current_variant
                        && let Some(values) = values
                    {
                        let variant = &enumeration.variants[current_variant].1;

                        if let Some(props) = &variant.properties {
                            let mut new_values = Vec::new();

                            for value in values.value {
                                if let Some(prop) = props.iter().find(|prop| prop.0 == value.value.name.value.0) {
                                    let pattern = if let Some(value) = value.value.value {
                                        Some(value.into_typed_ast(checker, ast)?)
                                    } else {
                                        None
                                    };

                                    if pattern.is_none() {
                                        let ty = checker.solver.add_info(TypeInfo::Type(checker.types.ref_of_type(&prop.1).unwrap()));

                                        checker.solver.add_var(&prop.0, ty);
                                    }

                                    new_values.push((value.value.name.value.0, pattern));
                                }
                            }

                            resulting_values.replace(new_values);
                        }
                    }
                }

                IsPattern::EnumVariant {
                    target,
                    variant: current_variant.unwrap(),
                    values: resulting_values.map(|v| v.into_boxed_slice()),
                }
            }
            Self::TypeName { ty, name } => {
                let ty = ty.into_typed_ast(checker, ast)?;

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

                    assert!(!checker.solver.contains_unknown(ty), "explicit type can't have non-explicit generics");

                    checker.solver.unify(value_ty, ty);
                }

                checker.solver.add_var(variable_decl.name.value.0, value_ty);

                Ok(None)
            }
            Self::StructDecl(struct_decl) => {
                let mut properties = Vec::new();

                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    checker.types.add_named_type(&name.value.0, TypeVariant::Generic(index));
                }

                // let mut fields = Vec::new();

                for property in struct_decl.properties.value {
                    let name = property.value.name.value.0;
                    let ty = property.value.ty.into_typed_ast(checker, ast)?;
                    // let constant = property.value.default_value.as_ref().and_then(|value|
                    // ConstantValue::to_constant(&value.value));

                    // fields.push((ty.variant.as_ir_type(compiler.jit.module.isa()), constant));

                    if let &TypeInfo::Type(ty) = checker.solver.get_info(ty) {
                        properties.push((name, checker.types.get_type(ty).unwrap().clone()));
                    }
                }

                for name in &struct_decl.name.value.generics {
                    checker.types.remove_named_type(&name.value.0);
                }

                let ty = Type {
                    applied_generics: Vec::new(),
                    variant: TypeVariant::complex(ComplexType::Struct(StructType {
                        structure: Default::default(),
                        generics: struct_decl.name.value.generics.into_iter().map(|g| g.value.0).collect(),
                        properties,
                    })),
                    declared_at: Some(span),
                };

                checker.types.add_named_full_type(struct_decl.name.value.name.value.0, ty);

                Ok(None)
            }
            Self::ComponentDecl(component_decl) => todo!(),
            Self::TraitDecl(trait_decl) => todo!(),
            Self::EnumDecl(enum_decl) => todo!(),
            Self::FuncDecl(func_decl) => todo!(),
            Self::Impl(_) => todo!(),
        }
    }
}

impl IntoTypedAST<TypeInfoRef> for mollie_parser::CustomType {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<TypeInfoRef, ()> {
        let applied_generics = self
            .generics
            .into_iter()
            .map(|generic| generic.into_typed_ast(checker, ast))
            .collect::<Result<Vec<_>, _>>()?;

        let ty = checker.types.get_named_type_ref(self.name.value.0).unwrap();
        let info = checker
            .types
            .get_type(ty)
            .and_then(|ty| ty.variant.as_struct())
            .map_or(TypeInfo::Type(ty), |structure| {
                TypeInfo::Struct(
                    ty,
                    (0..structure.generics.len())
                        .map(|generic| {
                            applied_generics
                                .get(generic)
                                .copied()
                                .unwrap_or_else(|| checker.solver.add_info(TypeInfo::Unknown(None)))
                        })
                        .collect(),
                )
            });

        Ok(checker.solver.add_info(info))
    }
}

impl IntoTypedAST<TypeInfoRef> for mollie_parser::Type {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<TypeInfoRef, ()> {
        Ok(match self {
            Self::Primitive(primitive_type) => match primitive_type {
                mollie_parser::PrimitiveType::IntSize => checker.core_types.int_size,
                mollie_parser::PrimitiveType::Int64 => checker.core_types.int64,
                mollie_parser::PrimitiveType::Int32 => checker.core_types.int32,
                mollie_parser::PrimitiveType::Int16 => checker.core_types.int16,
                mollie_parser::PrimitiveType::Int8 => checker.core_types.int8,
                mollie_parser::PrimitiveType::UIntSize => checker.core_types.uint_size,
                mollie_parser::PrimitiveType::UInt64 => checker.core_types.uint64,
                mollie_parser::PrimitiveType::UInt32 => checker.core_types.uint32,
                mollie_parser::PrimitiveType::UInt16 => checker.core_types.uint16,
                mollie_parser::PrimitiveType::UInt8 => checker.core_types.uint8,
                mollie_parser::PrimitiveType::Float => checker.core_types.float,
                mollie_parser::PrimitiveType::Boolean => checker.core_types.boolean,
                mollie_parser::PrimitiveType::String => checker.core_types.string,
                mollie_parser::PrimitiveType::Component => checker.core_types.component,
                mollie_parser::PrimitiveType::Void => checker.core_types.void,
            },
            Self::Custom(custom_type) => custom_type.into_typed_ast(checker, ast, span)?,
            Self::Array(element, size) => {
                let element = element.into_typed_ast(checker, ast)?;

                checker.solver.add_info(TypeInfo::Array(element, size.map(|size| size.value)))
            }
            Self::OneOf(types) => todo!(),
            Self::Func(args, output) => {
                let args = args.into_iter().map(|arg| arg.into_typed_ast(checker, ast)).collect::<Result<_, _>>()?;
                let output = if let Some(output) = output {
                    output.into_typed_ast(checker, ast)?
                } else {
                    checker.core_types.void
                };

                checker.solver.add_info(TypeInfo::Func(args, output))
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
            &Expr::Binary { operator, lhs, rhs } => self.is_constant_expr(lhs) && self.is_constant_expr(rhs),
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

#[cfg(test)]
mod tests {
    use mollie_parser::Parse;
    use mollie_typing::{
        PrimitiveType, TypeVariant,
        resolver::{CoreTypes, TypeInfo, TypeSolver, TypeStorage},
    };

    use crate::{IntoPositionedTypedAST, TypeChecker, TypedAST, expression::Expr, index::IndexVec, visitor::Visitor};

    struct TestVisitor;

    impl Visitor for TestVisitor {
        fn visit_var(&mut self, ast: &TypedAST, name: &str) {
            println!("accessing to {name}");
        }
    }

    #[test]
    fn test_typed_ast() {
        let mut types = TypeStorage::default();
        let mut solver = TypeSolver::default();

        // struct A {}

        // let a: A<f32> = A {};
        // let b = a;

        let core_types = CoreTypes {
            void: solver.add_info(TypeInfo::Primitive(PrimitiveType::Void)),
            any: solver.add_info(TypeInfo::Primitive(PrimitiveType::Any)),
            boolean: solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean)),
            int8: solver.add_info(TypeInfo::Primitive(PrimitiveType::I8)),
            int16: solver.add_info(TypeInfo::Primitive(PrimitiveType::I16)),
            int32: solver.add_info(TypeInfo::Primitive(PrimitiveType::I32)),
            int64: solver.add_info(TypeInfo::Primitive(PrimitiveType::I64)),
            int_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::ISize)),
            uint8: solver.add_info(TypeInfo::Primitive(PrimitiveType::U8)),
            uint16: solver.add_info(TypeInfo::Primitive(PrimitiveType::U16)),
            uint32: solver.add_info(TypeInfo::Primitive(PrimitiveType::U32)),
            uint64: solver.add_info(TypeInfo::Primitive(PrimitiveType::U64)),
            uint_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::USize)),
            float: solver.add_info(TypeInfo::Primitive(PrimitiveType::Float)),
            component: solver.add_info(TypeInfo::Primitive(PrimitiveType::Component)),
            string: solver.add_info(TypeInfo::Primitive(PrimitiveType::String)),
        };

        types.add_named_type("Hello", TypeVariant::structure([("alo", TypeVariant::usize()), ("da", TypeVariant::uint8())]));
        types.add_named_type("magic", TypeVariant::function([TypeVariant::usize()], TypeVariant::usize()));

        let mut checker = TypeChecker { core_types, types, solver };

        let expr = mollie_parser::Expr::parse_value(
            "{
                struct B<T> { value: T }

                const allow = |hello| { hello };
                const alo: B<int32> = B { value: true };
                const boolean = true;
                const boolean: int64 = boolean;
                const booleans: boolean[] = [boolean, boolean, boolean];
                
                allow(50);

                B { value: B { value: allow } }
            }",
        )
        .unwrap();
        let mut ast = TypedAST {
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
        };

        let expr = expr.into_typed_ast(&mut checker, &mut ast).unwrap();

        TestVisitor.visit_expr(&ast, expr);

        let result = checker
            .solver
            .format_type(checker.solver.get_actual_type(ast[expr].ty, &checker.types).unwrap(), &checker.types);

        if let Expr::Binary { operator, lhs, rhs } = &ast[expr].value {
            let lhs = checker
                .solver
                .format_type(checker.solver.get_actual_type(ast[*lhs].ty, &checker.types).unwrap(), &checker.types);

            let rhs = checker
                .solver
                .format_type(checker.solver.get_actual_type(ast[*rhs].ty, &checker.types).unwrap(), &checker.types);

            println!("{lhs} {operator} {rhs} => {result}");
        } else {
            println!("Typed AST dump: {ast:#?}");
            println!("Solver size: {:#?}", checker.solver.len());
            println!("Solver dump: {:#?}", checker.solver);

            for error in checker.solver.errors.drain(..).collect::<Vec<_>>() {
                match error {
                    mollie_typing::resolver::TypeUnificationError::TypeMismatch(lhs, rhs) => {
                        let lhs = checker.solver.get_actual_type(lhs, &checker.types).map(|ty| checker.solver.format_type(ty, &checker.types));
                        let rhs = checker.solver.get_actual_type(rhs, &checker.types).map(|ty| checker.solver.format_type(ty, &checker.types));

                        println!("type mismatch: {lhs:?} is not {rhs:?}");
                    }
                    mollie_typing::resolver::TypeUnificationError::ArraySizeMismatch(..) => {}
                }
            }
            println!("{result}");
        }
    }
}
