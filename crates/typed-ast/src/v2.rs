#![allow(dead_code)]

use std::{
    collections::HashMap,
    fmt,
    iter::once_with,
    ops::{Index, IndexMut},
};

use itertools::Itertools;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IdxEnumerate, IndexBoxedSlice, IndexVec};
use mollie_parser::LangItem;
use mollie_shared::{MaybePositioned, Operator, Positioned, Span};
use mollie_typing::{AdtKind, AdtRef, AdtVariantRef, FieldRef, IntType, PrimitiveType, TraitRef, UIntType, VFuncRef, VTableRef};

use crate::{Block, BlockRef, ExprRef, FuncRef, IntrinsicKind, LangItemValue, Module, ModuleId, ModuleItem, StmtRef, TraitFuncRef};

#[derive(Debug, Clone)]
pub struct Func {
    pub postfix: bool,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: TypeRef,
    pub kind: VTableFuncKind,
}

#[derive(Debug)]
pub struct TraitFunc {
    pub name: String,
    pub args: Box<[Arg<TypeRef>]>,
    pub returns: TypeRef,
}

#[derive(Debug)]
pub struct Trait {
    pub name: String,
    pub generics: usize,
    pub functions: IndexVec<TraitFuncRef, TraitFunc>,
}

impl Trait {
    pub fn get_func_offset<T: AsRef<str>>(&self, name: T) -> usize {
        let mut offset = 0;
        let name = name.as_ref();

        for func in self.functions.values() {
            if func.name == name {
                break;
            }

            offset += size_of::<usize>();
        }

        offset
    }
}

#[derive(Debug)]
pub struct AdtVariantField {
    pub name: String,
    pub ty: TypeRef,
    pub default_value: Option<ConstantValue>,
}

#[derive(Debug)]
pub struct AdtVariant {
    pub name: Option<String>,
    pub discriminant: usize,
    pub fields: IndexBoxedSlice<FieldRef, AdtVariantField>,
}

#[derive(Debug)]
pub struct Adt {
    pub name: Option<String>,
    pub collectable: bool,
    pub kind: AdtKind,
    pub generics: usize,
    pub variants: IndexBoxedSlice<AdtVariantRef, AdtVariant>,
}

impl Index<(AdtVariantRef, FieldRef)> for Adt {
    type Output = AdtVariantField;

    fn index(&self, (variant, field): (AdtVariantRef, FieldRef)) -> &Self::Output {
        &self.variants[variant].fields[field]
    }
}

impl Index<FieldRef> for Adt {
    type Output = AdtVariantField;

    fn index(&self, field: FieldRef) -> &Self::Output {
        &self.variants[AdtVariantRef::ZERO].fields[field]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SpecialAdtKind {
    AnyOf,
    Specific(AdtKind),
    WithExpectation(AdtKind),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeErrorValue {
    Type,
    Array(Option<usize>),
    Adt(SpecialAdtKind),
    Trait,
    Value,
    Function,
    Module,
    Generic,
    Nothing,
    PrimitiveType(PrimitiveType),
    ExplicitType(TypeRef),
}

#[derive(Debug, Clone)]
pub enum LookupType {
    Variable,
    Type { inside: ModuleId },
    Module { inside: ModuleId },
}

#[derive(Debug, Clone)]
pub enum TypeError {
    Unexpected { expected: TypeErrorValue, found: TypeErrorValue },
    VariantRequired(AdtRef),
    NoField { adt: AdtRef, variant: AdtVariantRef, name: String },
    NoFunction { name: String, postfix: bool },
    NonIndexable { ty: TypeRef, name: String },
    NotFound { name: String, was_looking_for: LookupType },
    NotPostfix { name: String },
    ArgumentCountMismatch { expected: usize, found: usize },
    NonConstantEvaluable,
    // InvalidPostfixFunction { reasons: Vec<Positioned<PostfixRequirement>> },
    Parse(mollie_parser::ParseError),
}

pub trait Descriptor {
    type Type;
    type IndexResult;
}

pub struct Typed<T, D: Descriptor> {
    pub value: T,
    pub span: Span,
    pub ty: D::Type,
}

impl<D: Descriptor, T: Clone> Clone for Typed<T, D>
where
    D::Type: Clone,
{
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            span: self.span.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl<D: Descriptor, T: fmt::Debug> fmt::Debug for Typed<T, D>
where
    D::Type: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Typed")
            .field("value", &self.value)
            .field("span", &self.span)
            .field("ty", &self.ty)
            .finish()
    }
}

pub struct TypedAST<D: Descriptor> {
    pub module: ModuleId,
    pub blocks: IndexVec<BlockRef, Typed<Block, D>>,
    pub statements: IndexVec<StmtRef, Stmt>,
    pub exprs: IndexVec<ExprRef, Typed<Expr<D>, D>>,
    pub used_items: Vec<UsedItem>,
}

impl<D: Descriptor> TypedAST<D> {
    fn use_item(&mut self, vtables: &IndexVec<VTableRef, VTableGenerator>, functions: &IndexVec<FuncRef, Func>, types: &mut TypeStorage, item: UsedItem) {
        match &item {
            UsedItem::VTable(_, vtable, vtable_type_args) => {
                for func in vtables[*vtable].functions.values() {
                    if let VTableFuncKind::Local { ast, .. } = &func.kind {
                        for item in &ast.used_items {
                            let item = match item {
                                UsedItem::VTable(ty, vtable, type_args) => {
                                    UsedItem::VTable(*ty, *vtable, type_args.iter().map(|ty| types.apply_type_args(*ty, vtable_type_args)).collect())
                                }
                                UsedItem::Adt(adt_ref, type_args) => {
                                    UsedItem::Adt(*adt_ref, type_args.iter().map(|ty| types.apply_type_args(*ty, vtable_type_args)).collect())
                                }
                                &UsedItem::Func(func) => UsedItem::Func(func),
                            };

                            self.use_item(vtables, functions, types, item);
                        }
                    }
                }
            }
            &UsedItem::Func(func) => {
                if let VTableFuncKind::Local { ast, .. } = &functions[func].kind {
                    for item in &ast.used_items {
                        let item = match item {
                            UsedItem::VTable(ty, vtable, type_args) => UsedItem::VTable(*ty, *vtable, type_args.clone()),
                            UsedItem::Adt(adt_ref, type_args) => UsedItem::Adt(*adt_ref, type_args.clone()),
                            &UsedItem::Func(func) => UsedItem::Func(func),
                        };

                        self.use_item(vtables, functions, types, item);
                    }
                }
            }
            _ => (),
        }

        if !self.used_items.contains(&item) {
            self.used_items.push(item);
        }
    }
}

impl<D: Descriptor> Clone for TypedAST<D>
where
    D::Type: Clone,
    D::IndexResult: Clone,
{
    fn clone(&self) -> Self {
        Self {
            module: self.module,
            blocks: self.blocks.clone(),
            statements: self.statements.clone(),
            exprs: self.exprs.clone(),
            used_items: self.used_items.clone(),
        }
    }
}

impl<D: Descriptor> Default for TypedAST<D> {
    fn default() -> Self {
        Self {
            module: ModuleId::ZERO,
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
            used_items: Vec::new(),
        }
    }
}

impl<D: Descriptor> fmt::Debug for TypedAST<D>
where
    D::Type: fmt::Debug,
    D::IndexResult: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypedAST")
            .field("module", &self.module)
            .field("blocks", &self.blocks)
            .field("statements", &self.statements)
            .field("exprs", &self.exprs)
            .finish()
    }
}

impl<D: Descriptor> TypedAST<D> {
    pub fn add_block(&mut self, block: Block, ty: D::Type, span: Span) -> BlockRef {
        let result = BlockRef(self.blocks.len());

        self.blocks.push(Typed { value: block, span, ty });

        result
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtRef {
        let result = StmtRef(self.statements.len());

        self.statements.push(stmt);

        result
    }

    pub fn add_expr(&mut self, expr: Expr<D>, ty: D::Type, span: Span) -> ExprRef {
        let result = ExprRef(self.exprs.len());

        self.exprs.push(Typed { value: expr, span, ty });

        result
    }

    // pub fn add_error_expr(&mut self, error: TypeErrorRef, ty: D::Type, span:
    // Span) -> ExprRef {     let result = ExprRef(self.exprs.len());

    //     self.exprs.push(Typed {
    //         value: Expr::Error(error),
    //         span,
    //         ty,
    //     });

    //     result
    // }

    // pub fn use_item(&mut self, item: UsedItem) {
    //     if !self.used_items.contains(&item) {
    //         self.used_items.push(item);
    //     }
    // }

    // /// Function for adding used VTable to Typed AST
    // #[track_caller]
    // pub fn use_vtable_impl<S>(&mut self, checker: &mut TypeChecker<S>, target:
    // TypeInfoRef, vtable: VTableRef) -> Box<[TypeInfoRef]> {     let type_args
    // = match checker.solver.get_info(target) {         TypeInfo::Adt(.., args)
    // => args.clone(),         &TypeInfo::Array(element, _) =>
    // Box::new([element]),         _ => Box::new([]),
    //     };

    //     for item in &checker.vtables[vtable].used_items {
    //         match item {
    //             UsedItem::VTable(ty, vtable, input_type_args) => {
    //                 if checker.current_vtable == Some(*vtable) {
    //                     continue;
    //                 }

    //                 self.use_item(UsedItem::VTable(*ty, *vtable,
    // input_type_args.clone()));             }
    //             UsedItem::Adt(adt_ref, adt_type_args) => {
    //                 let type_args = adt_type_args.iter().map(|&ty|
    // checker.solver.solve_generic_args(ty, &type_args)).collect();

    //                 self.use_item(UsedItem::Adt(*adt_ref, type_args));
    //             }
    //             &UsedItem::Func(func_ref) =>
    // self.use_item(UsedItem::Func(func_ref)),         }
    //     }

    //     if checker.current_vtable != Some(vtable) {
    //         let vtable_type_args: Box<[_]> = checker.vtables[vtable]
    //             .generics
    //             .iter()
    //             .map(|&ty| checker.solver.solve_generic_args(ty, &type_args))
    //             .collect();

    //         self.use_item(UsedItem::VTable(target, vtable, vtable_type_args));
    //     }

    //     type_args
    // }
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

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprRef),
    NewVar { mutable: bool, name: String, value: ExprRef },
}

#[derive(Debug, Clone)]
pub enum LitExpr {
    Boolean(bool),
    Float(f32),
    Int(i64),
    String(String),
}

pub enum Expr<D: Descriptor> {
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
    Func(FuncRef),
    TypeCast(ExprRef, PrimitiveType),
    IsPattern {
        target: ExprRef,
        pattern: IsPattern<D>,
    },
    Error(TypeErrorRef),
}

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

impl<D: Descriptor> Clone for IsPattern<D>
where
    D::Type: Clone,
{
    fn clone(&self) -> Self {
        match self {
            &Self::Literal(arg0) => Self::Literal(arg0),
            Self::EnumVariant {
                adt,
                adt_variant,
                adt_type_args,
                values,
            } => Self::EnumVariant {
                adt: *adt,
                adt_variant: *adt_variant,
                adt_type_args: adt_type_args.clone(),
                values: values.clone(),
            },
            Self::TypeName { ty, name } => Self::TypeName {
                ty: ty.clone(),
                name: name.clone(),
            },
        }
    }
}

impl<D: Descriptor> fmt::Debug for IsPattern<D>
where
    D::Type: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(arg0) => f.debug_tuple("Literal").field(arg0).finish(),
            Self::EnumVariant {
                adt,
                adt_variant,
                adt_type_args,
                values,
            } => f
                .debug_struct("EnumVariant")
                .field("adt", adt)
                .field("adt_variant", adt_variant)
                .field("adt_type_args", adt_type_args)
                .field("values", values)
                .finish(),
            Self::TypeName { ty, name } => f.debug_struct("TypeName").field("ty", ty).field("name", name).finish(),
        }
    }
}

impl<D: Descriptor> Clone for Expr<D>
where
    D::Type: Clone,
    D::IndexResult: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Lit(arg0) => Self::Lit(arg0.clone()),
            Self::Var(arg0) => Self::Var(arg0.clone()),
            Self::Array { element, elements } => Self::Array {
                element: element.clone(),
                elements: elements.clone(),
            },
            &Self::IfElse { condition, block, otherwise } => Self::IfElse { condition, block, otherwise },
            &Self::While { condition, block } => Self::While { condition, block },
            &Self::Block(arg0) => Self::Block(arg0),
            &Self::Binary { operator, lhs, rhs } => Self::Binary { operator, lhs, rhs },
            Self::Closure { args, body } => Self::Closure {
                args: args.clone(),
                body: *body,
            },
            Self::Call { func, args } => Self::Call {
                func: *func,
                args: args.clone(),
            },
            Self::Construct { adt, variant, fields } => Self::Construct {
                adt: *adt,
                variant: *variant,
                fields: fields.clone(),
            },
            Self::AdtIndex { target, field } => Self::AdtIndex {
                target: *target,
                field: field.clone(),
            },
            Self::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => Self::VTableIndex {
                target: *target,
                target_ty: target_ty.clone(),
                vtable: *vtable,
                func: *func,
            },
            Self::IsPattern { target, pattern } => Self::IsPattern {
                target: *target,
                pattern: pattern.clone(),
            },
            &Self::ArrayIndex { target, element } => Self::ArrayIndex { target, element },
            &Self::TypeCast(target, ty) => Self::TypeCast(target, ty),
            &Self::Func(func) => Self::Func(func),
            Self::Error(arg0) => Self::Error(arg0.clone()),
        }
    }
}

impl<D: Descriptor> fmt::Debug for Expr<D>
where
    D::Type: fmt::Debug,
    D::IndexResult: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lit(arg0) => f.debug_tuple("Lit").field(arg0).finish(),
            Self::Var(arg0) => f.debug_tuple("Var").field(arg0).finish(),
            Self::Array { element, elements } => f.debug_struct("Array").field("element", element).field("elements", elements).finish(),
            Self::IfElse { condition, block, otherwise } => f
                .debug_struct("IfElse")
                .field("condition", condition)
                .field("block", block)
                .field("otherwise", otherwise)
                .finish(),
            Self::While { condition, block } => f.debug_struct("While").field("condition", condition).field("block", block).finish(),
            Self::Block(arg0) => f.debug_tuple("Block").field(arg0).finish(),
            Self::Binary { operator, lhs, rhs } => f
                .debug_struct("Binary")
                .field("operator", operator)
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
            Self::Closure { args, body } => f.debug_struct("Closure").field("args", args).field("body", body).finish(),
            Self::Call { func, args } => f.debug_struct("Call").field("func", func).field("args", args).finish(),
            Self::Construct { adt, variant, fields } => f
                .debug_struct("Construct")
                .field("adt", adt)
                .field("variant", variant)
                .field("fields", fields)
                .finish(),
            Self::AdtIndex { target, field } => f.debug_struct("AdtIndex").field("target", target).field("field", field).finish(),
            Self::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => f
                .debug_struct("VTableIndex")
                .field("target", target)
                .field("target_ty", target_ty)
                .field("vtable", vtable)
                .field("func", func)
                .finish(),
            Self::IsPattern { target, pattern } => f.debug_struct("IsPattern").field("target", target).field("pattern", pattern).finish(),
            Self::ArrayIndex { target, element } => f.debug_struct("ArrayIndex").field("target", target).field("element", element).finish(),
            Self::TypeCast(target, ty) => f.debug_tuple("TypeCast").field(target).field(ty).finish(),
            Self::Func(func) => f.debug_tuple("Func").field(func).finish(),
            Self::Error(arg0) => f.debug_tuple("D").field(arg0).finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum VTableFuncKind<S = ()> {
    Local { ast: TypedAST<SolvedPass>, entry: BlockRef },
    External(&'static str),
    Special(S),
}

#[derive(Debug, Clone)]
pub struct VTableFunc {
    pub trait_func: Option<TraitFuncRef>,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: TypeRef,
    pub kind: VTableFuncKind,
}

#[derive(Debug)]
pub struct VTableGenerator {
    pub ty: TypeRef,
    pub origin_trait: Option<TraitRef>,
    pub generics: Box<[TypeRef]>,
    // pub applied_generics: Box<[TypeInfoRef]>,
    // pub used_items: Vec<UsedItem>,
    pub functions: IndexVec<VFuncRef, VTableFunc>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UsedItem {
    VTable(TypeRef, VTableRef, Box<[TypeRef]>),
    Adt(AdtRef, Box<[TypeRef]>),
    Func(FuncRef),
}

#[derive(Debug, Default)]
struct TypeStorage {
    types: IndexVec<TypeRef, Type>,
}

impl TypeStorage {
    fn is_likely_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        match (&self.types[ty], &self.types[other]) {
            (Type::Generic(..), _) | (_, Type::Generic(..)) => true,
            (Type::Primitive(primitive_type), Type::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (&Type::Array(element, size), &Type::Array(other_element, other_size)) => {
                self.is_likely_same(element, other_element)
                    && match (size, other_size) {
                        (None, None | Some(_)) => true,
                        (Some(_), None) => false,
                        (Some(size), Some(other_size)) => size == other_size,
                    }
            }
            (Type::Func(args, returns), Type::Func(other_args, other_returns)) => {
                args.len() == other_args.len()
                    && args
                        .iter()
                        .zip(other_args)
                        .all(|(&field_type, &other_field_type)| self.is_likely_same(field_type, other_field_type))
                    && self.is_likely_same(*returns, *other_returns)
            }
            (Type::Adt(adt, type_args), Type::Adt(other_adt, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args
                        .iter()
                        .zip(other_type_args)
                        .all(|(&field_type, &other_field_type)| self.is_likely_same(field_type, other_field_type))
                    && adt == other_adt
            }
            (Type::Trait(trait_ref, type_args), Type::Trait(other_trait_ref, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args
                        .iter()
                        .zip(other_type_args)
                        .all(|(&arg, &other_arg)| self.is_likely_same(arg, other_arg))
                    && trait_ref == other_trait_ref
            }
            _ => false,
        }
    }

    fn is_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        match (&self.types[ty], &self.types[other]) {
            (Type::Generic(index), Type::Generic(other_index)) => index == other_index,
            (Type::Primitive(primitive_type), Type::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (&Type::Array(element, size), &Type::Array(other_element, other_size)) => self.is_same(element, other_element) && size == other_size,
            (Type::Func(args, returns), Type::Func(other_args, other_returns)) => {
                args.len() == other_args.len()
                    && args.iter().zip(other_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && self.is_same(*returns, *other_returns)
            }
            (Type::Adt(adt, type_args), Type::Adt(other_adt, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && adt == other_adt
            }
            (Type::Trait(trait_ref, type_args), Type::Trait(other_trait_ref, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && trait_ref == other_trait_ref
            }
            _ => false,
        }
    }

    fn get_or_add(&mut self, typo: Type) -> TypeRef {
        for (key, ty) in self.types.iter() {
            if ty == &typo {
                return key;
            }
        }

        self.types.insert(typo)
    }

    fn apply_type_args(&mut self, ty: TypeRef, type_args: &[TypeRef]) -> TypeRef {
        match self.types[ty].clone() {
            Type::Primitive(_) => ty,
            Type::Array(element, size) => {
                let element = self.apply_type_args(element, type_args);

                self.get_or_add(Type::Array(element, size))
            }
            Type::Adt(adt_ref, mut adt_type_args) => {
                for arg in &mut adt_type_args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                self.get_or_add(Type::Adt(adt_ref, adt_type_args))
            }
            Type::Trait(trait_ref, mut adt_type_args) => {
                for arg in &mut adt_type_args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                self.get_or_add(Type::Trait(trait_ref, adt_type_args))
            }
            Type::Func(mut args, returns) => {
                for arg in &mut args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                let returns = self.apply_type_args(returns, type_args);

                self.get_or_add(Type::Func(args, returns))
            }
            Type::Generic(i) => type_args.get(i).copied().unwrap(),
            Type::Error => ty,
        }
    }
}

impl Index<TypeRef> for TypeStorage {
    type Output = Type;

    fn index(&self, index: TypeRef) -> &Self::Output {
        &self.types[index]
    }
}

impl IndexMut<TypeRef> for TypeStorage {
    fn index_mut(&mut self, index: TypeRef) -> &mut Self::Output {
        &mut self.types[index]
    }
}

#[derive(Debug, Default)]
struct TypeContext {
    types: TypeStorage,

    language_items: HashMap<LangItem, LangItemValue>,

    modules: IndexVec<ModuleId, Module>,
    adt_types: IndexVec<AdtRef, Adt>,
    functions: IndexVec<FuncRef, Func>,
    traits: IndexVec<TraitRef, Trait>,
    vtables: IndexVec<VTableRef, VTableGenerator>,

    errors: IndexVec<TypeErrorRef, Positioned<TypeError>>,
}

pub struct TypeDisplay<'a> {
    ty: TypeRef,
    context: &'a TypeContext,
}

impl TypeDisplay<'_> {
    const fn with(&self, ty: TypeRef) -> Self {
        Self { ty, context: self.context }
    }
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.context.types[self.ty] {
            Type::Primitive(primitive_type) => primitive_type.fmt(f),
            &Type::Array(element, size) => match size {
                Some(size) => write!(f, "{}[{size}]", self.with(element)),
                None => write!(f, "{}[]", self.with(element)),
            },
            Type::Adt(adt_ref, type_args) => {
                if let Some(name) = &self.context.adt_types[*adt_ref].name {
                    name.fmt(f)?;
                }

                if type_args.is_empty() {
                    Ok(())
                } else {
                    write!(f, "<{}>", type_args.iter().map(|&ty| self.with(ty)).join(", "))
                }
            }
            Type::Trait(trait_ref, type_args) => {
                self.context.traits[*trait_ref].name.fmt(f)?;

                if type_args.is_empty() {
                    Ok(())
                } else {
                    write!(f, "<{}>", type_args.iter().map(|&ty| self.with(ty)).join(", "))
                }
            }
            Type::Func(arg_types, returns) => write!(f, "fn({}) -> {}", arg_types.iter().map(|&ty| self.with(ty)).join(", "), self.with(*returns)),
            Type::Generic(i) => write!(f, "<generic({i})>"),
            Type::Error => f.write_str("<error>"),
        }
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            types: TypeStorage::default(),

            language_items: HashMap::new(),

            modules: IndexVec::from_iter([Module::new(ModuleId::ZERO, "<anonymous>")]),
            adt_types: IndexVec::new(),
            functions: IndexVec::new(),
            traits: IndexVec::new(),
            vtables: IndexVec::new(),

            errors: IndexVec::new(),
        }
    }

    pub const fn display_of(&self, ty: TypeRef) -> TypeDisplay<'_> {
        TypeDisplay { ty, context: self }
    }

    pub fn error(&mut self, error: TypeError, span: Span) -> TypeErrorRef {
        self.errors.insert(span.wrap(error))
    }

    pub fn get_adt_item(&self, item: LangItem) -> Option<AdtRef> {
        self.language_items
            .get(&item)
            .and_then(|v| if let &LangItemValue::Adt(adt_ref) = v { Some(adt_ref) } else { None })
    }

    pub fn get_adt_variant_item(&self, item: LangItem) -> Option<(AdtRef, AdtVariantRef)> {
        self.language_items.get(&item).and_then(|v| {
            if let &LangItemValue::AdtVariant(adt_ref, adt_variant) = v {
                Some((adt_ref, adt_variant))
            } else {
                None
            }
        })
    }

    pub fn get_trait_item(&self, item: LangItem) -> Option<TraitRef> {
        self.language_items
            .get(&item)
            .and_then(|v| if let &LangItemValue::Trait(trait_ref) = v { Some(trait_ref) } else { None })
    }

    pub fn get_trait_func_item(&self, trait_ref: TraitRef, item: LangItem) -> Option<TraitFuncRef> {
        self.language_items.get(&item).and_then(|v| {
            if let &LangItemValue::TraitFunc(func_trait_ref, trait_func) = v
                && func_trait_ref == trait_ref
            {
                Some(trait_func)
            } else {
                None
            }
        })
    }

    pub fn find_vtable(&self, ty: TypeRef, origin_trait: Option<TraitRef>) -> Option<VTableRef> {
        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_likely_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
    }

    pub fn find_vtable_by_func<T: AsRef<str>>(&self, ty: TypeRef, name: T) -> Option<(VTableRef, VFuncRef)> {
        let name = name.as_ref();

        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_likely_same(ty, vtable.ty)
                && let Some(func) = vtable
                    .functions
                    .iter()
                    .find_map(|(func_ref, func)| if func.name == name { Some(func_ref) } else { None })
            {
                return Some((vtable_ref, func));
            }
        }

        None
    }

    pub fn get_vtable(&self, ty: TypeRef, origin_trait: Option<TraitRef>) -> Option<VTableRef> {
        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
    }

    fn solver(&mut self) -> TypeSolver<'_> {
        TypeSolver {
            context: self,
            type_infos: IndexVec::new(),
            available_generics: HashMap::new(),
            frames: vec![TypeFrame::default()],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Primitive(PrimitiveType),
    Array(TypeRef, Option<usize>),
    Adt(AdtRef, Box<[TypeRef]>),
    Trait(TraitRef, Box<[TypeRef]>),
    Func(Box<[TypeRef]>, TypeRef),
    Generic(usize),
    Error,
}

#[derive(Debug, Clone)]
enum TypeInfo {
    Primitive(PrimitiveType),
    Array(TypeInfoRef, Option<usize>),
    Func(Box<[TypeInfoRef]>, TypeInfoRef),
    Adt(AdtRef, Box<[TypeInfoRef]>),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Unknown(Option<TypeInfoRef>),
    Generic(usize),
    Ref(TypeInfoRef),
    Error,
}

mollie_index::new_idx_type!(TypeInfoRef);
mollie_index::new_idx_type!(TypeRef);
mollie_index::new_idx_type!(TypeErrorRef);

#[derive(Debug)]
struct TypeSolver<'a> {
    context: &'a mut TypeContext,
    available_generics: HashMap<String, (TypeInfoRef, TypeRef)>,
    type_infos: IndexVec<TypeInfoRef, MaybePositioned<TypeInfo>>,
    frames: Vec<TypeFrame>,
}

#[derive(Debug, Default)]
struct TypeFrame(HashMap<String, TypeInfoRef>);

impl TypeSolver<'_> {
    fn fork(&mut self) -> TypeSolver<'_> {
        TypeSolver {
            context: self.context,
            type_infos: IndexVec::new(),
            available_generics: HashMap::new(),
            frames: vec![TypeFrame::default()],
        }
    }

    fn finalize(&mut self) {
        for type_info in self.type_infos.values_mut() {
            if let TypeInfo::Unknown(Some(fallback)) = type_info.value {
                type_info.value = TypeInfo::Ref(fallback);
            }
        }
    }

    fn push_frame(&mut self) {
        self.frames.push(TypeFrame::default());
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn set_var<T: Into<String>>(&mut self, name: T, ty: TypeInfoRef) {
        if let Some(frame) = self.frames.last_mut() {
            frame.0.insert(name.into(), ty);
        }
    }

    fn get_var(&self, name: impl AsRef<str>) -> Option<TypeInfoRef> {
        let name = name.as_ref();

        for frame in self.frames.iter().rev() {
            if let Some(&var) = frame.0.get(name) {
                return Some(var);
            }
        }

        None
    }

    fn add_info(&mut self, info: TypeInfo, span: Option<Span>) -> TypeInfoRef {
        self.type_infos.insert(MaybePositioned::new(info, span))
    }

    fn add_unknown(&mut self, fallback: Option<TypeInfo>, span: Option<Span>) -> TypeInfoRef {
        let fallback = fallback.map(|fallback| self.add_info(fallback, span));

        self.type_infos.insert(MaybePositioned::new(TypeInfo::Unknown(fallback), span))
    }

    fn get_info(&self, info_ref: TypeInfoRef) -> TypeInfoRef {
        if let &TypeInfo::Ref(info_ref) = &self.type_infos[info_ref].value {
            self.get_info(info_ref)
        } else {
            info_ref
        }
    }

    fn unify(&mut self, a: TypeInfoRef, b: TypeInfoRef) {
        let a = self.get_info(a);
        let b = self.get_info(b);

        if a == b {
            println!("warning: cyclic unify");

            return;
        }

        match (self.type_infos[a].value.clone(), self.type_infos[b].value.clone()) {
            (TypeInfo::Unknown(None), _) | (TypeInfo::Unknown(Some(_)), TypeInfo::Unknown(Some(_))) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Unknown(None)) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Unknown(Some(_)), _) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Unknown(Some(_))) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),
            (TypeInfo::Generic(_), _) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Generic(_)) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Array(a_element, a_size), TypeInfo::Array(b_element, b_size)) => {
                self.unify(a_element, b_element);

                match (a_size, b_size) {
                    (None, None) => (),
                    (None, Some(_)) => self.type_infos[b].value = TypeInfo::Array(b_element, None),
                    (Some(_), None) => self.type_infos[a].value = TypeInfo::Array(a_element, None),
                    (Some(a_size), Some(b_size)) => {
                        if a_size != b_size {
                            // if let Some(span) = self.type_infos[got].span {
                            //     self.errors.push(span.
                            // wrap(TypeUnificationError::ArraySizeMismatch(a_size,
                            // b_size))); }
                        }
                    }
                }
            }
            (TypeInfo::Func(a_args, a_returns), TypeInfo::Func(b_args, b_returns)) => {
                for (a, b) in a_args.into_iter().zip(b_args.into_iter()) {
                    self.unify(a, b);
                }

                self.unify(a_returns, b_returns);
            }
            (TypeInfo::Adt(a_adt, a_type_args), TypeInfo::Adt(b_adt, b_type_args)) => {
                for (a, b) in a_type_args.into_iter().zip(b_type_args.into_iter()) {
                    self.unify(a, b);
                }
            }
            (TypeInfo::Trait(trait_ref, t_args), b_info) => {
                let b = self.solve(b);

                if let Some(vtable) = self.context.find_vtable(b, Some(trait_ref)) {
                    let type_args = if let TypeInfo::Adt(_, type_args) = b_info {
                        type_args
                    } else {
                        Box::default()
                    };

                    let generics: Box<[_]> = self.context.vtables[vtable]
                        .generics
                        .iter()
                        .map(|g| Self::type_to_info(&mut self.type_infos, self.context, *g, &type_args))
                        .collect();

                    for (arg, generic) in t_args.into_iter().zip(generics) {
                        self.unify(arg, generic);
                    }
                } else {
                    panic!("unimplemented")
                }
            }
            // (_, Some(TypeInfo::Trait(trait_ref, _))) => {
            //     if let Some(vtable) = self
            //         .storage.find_vtable(&self.get_field_type(got))
            //         .is_none_or(|vtables| !vtables.contains_key(&Some(trait_ref)))
            //     {

            //     }
            // }
            (TypeInfo::Primitive(a), TypeInfo::Primitive(b)) => {
                // assert!(a == b, "Type mismatch between {a:?} and {b:?}");
            }
            (..) => (),
        }
    }

    fn solve(&mut self, info: TypeInfoRef) -> TypeRef {
        match &self.type_infos[info].value {
            &TypeInfo::Primitive(primitive_type) => self.context.types.get_or_add(Type::Primitive(primitive_type)),
            &TypeInfo::Array(element, size) => {
                let element = self.solve(element);

                self.context.types.get_or_add(Type::Array(element, size))
            }
            TypeInfo::Func(args, returns) => {
                let args = args.clone();
                let returns = self.solve(*returns);
                let args = args.into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Func(args, returns))
            }
            &TypeInfo::Unknown(Some(info)) | &TypeInfo::Ref(info) => self.solve(info),
            &TypeInfo::Unknown(None) => panic!("can't infer type"),
            TypeInfo::Adt(adt_ref, type_args) => {
                let adt_ref = *adt_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Adt(adt_ref, type_args))
            }
            TypeInfo::Trait(trait_ref, type_args) => {
                let trait_ref = *trait_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Trait(trait_ref, type_args))
            }
            &TypeInfo::Generic(i) => self.context.types.get_or_add(Type::Generic(i)),
            TypeInfo::Error => self.context.types.get_or_add(Type::Error),
        }
    }

    fn type_to_info(
        infos: &mut IndexVec<TypeInfoRef, MaybePositioned<TypeInfo>>,
        storage: &TypeContext,
        ty: TypeRef,
        type_args: &[TypeInfoRef],
    ) -> TypeInfoRef {
        match storage.types[ty].clone() {
            Type::Primitive(primitive_type) => infos.insert(MaybePositioned::new(TypeInfo::Primitive(primitive_type), None)),
            Type::Array(element, size) => {
                let element = Self::type_to_info(infos, storage, element, type_args);

                infos.insert(MaybePositioned::new(TypeInfo::Array(element, size), None))
            }
            Type::Adt(adt_ref, adt_type_args) => {
                let adt_type_args = adt_type_args
                    .into_iter()
                    .map(|type_arg| Self::type_to_info(infos, storage, type_arg, type_args))
                    .collect();

                infos.insert(MaybePositioned::new(TypeInfo::Adt(adt_ref, adt_type_args), None))
            }
            Type::Trait(trait_ref, trait_type_args) => {
                let trait_type_args = trait_type_args
                    .into_iter()
                    .map(|type_arg| Self::type_to_info(infos, storage, type_arg, type_args))
                    .collect();

                infos.insert(MaybePositioned::new(TypeInfo::Trait(trait_ref, trait_type_args), None))
            }
            Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::type_to_info(infos, storage, arg, type_args)).collect();
                let returns = Self::type_to_info(infos, storage, returns, type_args);

                infos.insert(MaybePositioned::new(TypeInfo::Func(args, returns), None))
            }
            Type::Generic(i) => type_args
                .get(i)
                .copied()
                .unwrap_or_else(|| infos.insert(MaybePositioned::new(TypeInfo::Generic(i), None))),
            Type::Error => infos.insert(MaybePositioned::new(TypeInfo::Error, None)),
        }
    }

    fn instantiate_adt(&mut self, adt: AdtRef, variant: AdtVariantRef, type_args: &[TypeInfoRef]) -> impl Iterator<Item = (FieldRef, TypeInfoRef)> {
        self.context.adt_types[adt].variants[variant]
            .fields
            .iter()
            .map(|(field_ref, field)| (field_ref, Self::type_to_info(&mut self.type_infos, self.context, field.ty, type_args)))
    }
}

struct FirstPass;

impl Descriptor for FirstPass {
    type IndexResult = String;
    type Type = TypeInfoRef;
}

struct SolvedPass;

impl Descriptor for SolvedPass {
    type IndexResult = FieldRef;
    type Type = TypeRef;
}

impl IsPattern<FirstPass> {
    fn from_parsed_expr(pattern: mollie_parser::IsPattern, ast: &mut TypedAST<FirstPass>, solver: &mut TypeSolver<'_>, span: Span) -> Self {
        match pattern {
            mollie_parser::IsPattern::Literal(literal_expr) => Self::Literal(Expr::from_parse_lit(literal_expr, ast, solver, span)),
            mollie_parser::IsPattern::Type { ty, pattern } => {
                let path = TypePathResult::from_parsed_type_path(ty.value, solver, ty.span);

                match path {
                    TypePathResult::Adt(adt, adt_type_args, adt_variant) => match (adt_variant, pattern.value) {
                        (Some(adt_variant), mollie_parser::TypePattern::Values(values)) => {
                            let fields = solver.instantiate_adt(adt, adt_variant, adt_type_args.as_ref()).collect::<Box<[_]>>();

                            let mut new_values = Vec::new();

                            for value in values {
                                if let Some(prop) = fields.iter().find_map(|prop| {
                                    let name = &solver.context.adt_types[adt].variants[adt_variant].fields[prop.0].name;

                                    if name == &value.value.name.value.0 {
                                        Some((name.clone(), prop.0, prop.1))
                                    } else {
                                        None
                                    }
                                }) {
                                    let pattern = if let Some(value) = value.value.value {
                                        Some(Self::from_parsed_expr(value.value, ast, solver, value.span))
                                    } else {
                                        None
                                    };

                                    if pattern.is_none() {
                                        solver.set_var(&prop.0, prop.2);
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
                            let ty = solver.add_info(TypeInfo::Adt(adt, adt_type_args), Some(span));

                            solver.set_var(&name.0, ty);

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

impl Expr<FirstPass> {
    fn from_parse_lit(lit: mollie_parser::LiteralExpr, ast: &mut TypedAST<FirstPass>, solver: &mut TypeSolver<'_>, span: Span) -> ExprRef {
        match lit {
            mollie_parser::LiteralExpr::Number(number, postfix) => match (number.value, postfix.as_ref().map(|v| v.value.as_str())) {
                (mollie_parser::Number::I64(value), Some(postfix)) => {
                    let info = match postfix {
                        "uint_size" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize)),
                        "uint64" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U64)),
                        "uint32" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U32)),
                        "uint16" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U16)),
                        "uint8" => TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U8)),
                        "int_size" => TypeInfo::Primitive(PrimitiveType::Int(IntType::ISize)),
                        "int64" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I64)),
                        "int32" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I32)),
                        "int16" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I16)),
                        "int8" => TypeInfo::Primitive(PrimitiveType::Int(IntType::I8)),
                        _ => TypeInfo::Unknown(Some(solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::I32)), Some(number.span)))),
                    };

                    ast.add_expr(Self::Lit(LitExpr::Int(value)), solver.add_info(info, Some(number.span)), number.span)
                }
                (mollie_parser::Number::I64(value), None) => ast.add_expr(
                    Self::Lit(LitExpr::Int(value)),
                    solver.add_unknown(Some(TypeInfo::Primitive(PrimitiveType::Int(IntType::I32))), Some(number.span)),
                    number.span,
                ),
                (mollie_parser::Number::F32(value), Some(postfix)) => ast.add_expr(
                    Self::Lit(LitExpr::Float(value)),
                    solver.add_info(TypeInfo::Primitive(PrimitiveType::Float), Some(number.span)),
                    number.span,
                ),
                (mollie_parser::Number::F32(value), None) => ast.add_expr(
                    Self::Lit(LitExpr::Float(value)),
                    solver.add_info(TypeInfo::Primitive(PrimitiveType::Float), Some(number.span)),
                    number.span,
                ),
            },
            mollie_parser::LiteralExpr::Boolean(value) => ast.add_expr(
                Self::Lit(LitExpr::Boolean(value)),
                solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), Some(span)),
                span,
            ),
            mollie_parser::LiteralExpr::String(value) => ast.add_expr(
                Self::Lit(LitExpr::String(value)),
                solver.add_info(TypeInfo::Primitive(PrimitiveType::String), Some(span)),
                span,
            ),
        }
    }

    fn from_parsed_expr(expr: mollie_parser::Expr, ast: &mut TypedAST<FirstPass>, solver: &mut TypeSolver<'_>, span: Span) -> ExprRef {
        let expr = match expr {
            mollie_parser::Expr::Literal(literal_expr) => Self::from_parse_lit(literal_expr, ast, solver, span),
            mollie_parser::Expr::FunctionCall(func_call_expr) => {
                let func_span = func_call_expr.function.span;
                let func = Self::from_parsed_expr(func_call_expr.function.value, ast, solver, func_call_expr.function.span);
                let ty = if let TypeInfo::Func(_, returns) = solver.type_infos[ast[func].ty].value {
                    returns
                } else {
                    solver.add_unknown(None, Some(func_span))
                };

                let args: Box<[_]> = func_call_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| Self::from_parsed_expr(arg.value, ast, solver, arg.span))
                    .collect();

                let func_ty = solver.add_info(TypeInfo::Func(args.iter().map(|&arg| ast[arg].ty).collect(), ty), Some(span));

                solver.unify(ast[func].ty, func_ty);

                ast.add_expr(Self::Call { func, args }, ty, span)
            }
            mollie_parser::Expr::Node(mut node_expr) => {
                let name_span = node_expr.name.span;
                let ty = TypePathResult::from_parsed_type_path(node_expr.name.value, solver, node_expr.name.span);

                let (adt, type_args, variant) = match ty {
                    TypePathResult::Adt(adt, type_args, variant) => (adt, type_args, variant),
                    result => {
                        let (ty, found) = match result {
                            TypePathResult::VFunc(.., vtable, vfunc) => {
                                let ty =
                                    TypeSolver::type_to_info(&mut solver.type_infos, solver.context, solver.context.vtables[vtable].functions[vfunc].ty, &[]);

                                (ty, TypeErrorValue::Function)
                            }
                            TypePathResult::Trait(trait_ref, type_args) => {
                                let ty = solver.add_info(TypeInfo::Trait(trait_ref, type_args), None);

                                (ty, TypeErrorValue::Trait)
                            }
                            TypePathResult::Func(func) => {
                                let ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, solver.context.functions[func].ty, &[]);

                                (ty, TypeErrorValue::Function)
                            }
                            TypePathResult::Intrinsic(_) => todo!(),
                            TypePathResult::Generic(ty) => (ty, TypeErrorValue::Generic),
                            TypePathResult::Module(_) => (solver.add_info(TypeInfo::Error, None), TypeErrorValue::Module),
                            TypePathResult::Error(error, span) => (solver.add_info(TypeInfo::Error, None), TypeErrorValue::Nothing),
                            TypePathResult::Adt(..) => unreachable!(),
                        };

                        return ast.add_expr(
                            Self::Error(solver.context.error(
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

                let variant = match (solver.context.adt_types[adt].kind, variant) {
                    (AdtKind::Struct | AdtKind::Component, None) => AdtVariantRef::ZERO,
                    (AdtKind::Struct | AdtKind::Component, Some(_)) => todo!(),
                    (AdtKind::Enum, None) => {
                        let ty = solver.add_info(TypeInfo::Error, None);

                        return ast.add_expr(Self::Error(solver.context.error(TypeError::VariantRequired(adt), name_span)), ty, name_span);
                    }
                    (AdtKind::Enum, Some(variant)) => variant,
                };

                let mut fields = solver
                    .instantiate_adt(adt, variant, &type_args)
                    .map(|(field_ref, field_type)| (field_ref, field_type, ExprRef::INVALID))
                    .collect::<IndexBoxedSlice<FieldRef, _>>();

                let ty = solver.add_info(TypeInfo::Adt(adt, type_args), None);

                for prop in node_expr.properties {
                    let name = prop.value.name.value.0;

                    let field = solver.context.adt_types[adt].variants[variant]
                        .fields
                        .iter()
                        .find_map(|(field_ref, field)| if field.name == name { Some(field_ref) } else { None });

                    if let Some(field) = field {
                        let value = match prop.value.value {
                            Some(value) => Self::from_parsed_expr(value.value, ast, solver, value.span),
                            None => {
                                if let Some(ty) = solver.get_var(&name) {
                                    ast.add_expr(Self::Var(name), ty, prop.value.name.span)
                                } else {
                                    ast.add_expr(
                                        Self::Error(solver.context.error(
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

                        solver.unify(fields[field].1, ast[value].ty);

                        fields[field].2 = value;
                    } else {
                        let ty = solver.add_info(TypeInfo::Unknown(None), None);

                        ast.add_expr(
                            Self::Error(solver.context.error(TypeError::NoField { adt, variant, name }, prop.span)),
                            ty,
                            prop.span,
                        );
                    }
                }

                if !node_expr.children.value.is_empty() {
                    if matches!(solver.context.adt_types[adt].kind, AdtKind::Component) {
                        let field = solver.context.adt_types[adt].variants[variant]
                            .fields
                            .iter()
                            .find_map(|(field_ref, field)| if field.name == "children" { Some(field_ref) } else { None });

                        if let Some(field) = field {
                            let element_count = node_expr.children.value.len();
                            let value = if element_count == 1 {
                                let node = node_expr.children.value.remove(0);

                                Self::from_parsed_expr(mollie_parser::Expr::Node(node.value), ast, solver, node.span)
                            } else {
                                let elements: Box<[ExprRef]> = node_expr
                                    .children
                                    .value
                                    .into_iter()
                                    .map(|child| Self::from_parsed_expr(mollie_parser::Expr::Node(child.value), ast, solver, child.span))
                                    .collect();

                                let element = ast.exprs[elements[0]].ty;
                                let ty = solver.add_info(TypeInfo::Array(element, Some(element_count)), None);

                                ast.add_expr(Self::Array { element, elements }, ty, node_expr.children.span)
                            };

                            solver.unify(fields[field].1, ast[value].ty);

                            fields[field].2 = value;
                        } else {
                            todo!("error: this component type can't have children");
                        }
                    } else {
                        todo!("error: children-syntax is available only for component types");
                    }
                }

                let fields = fields.raw;

                ast.add_expr(Self::Construct { adt, variant, fields }, ty, span)
            }
            mollie_parser::Expr::Index(index_expr) => {
                let target = Self::from_parsed_expr(index_expr.target.value, ast, solver, index_expr.target.span);

                match index_expr.index.value {
                    mollie_parser::IndexTarget::Named(ident) => ast.add_expr(
                        Self::AdtIndex { target, field: ident.0 },
                        solver.add_unknown(None, Some(index_expr.index.span)),
                        span,
                    ),
                    mollie_parser::IndexTarget::Expression(expr) => {
                        let element = Self::from_parsed_expr(*expr, ast, solver, index_expr.index.span);
                        let usize = solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize)), Some(index_expr.index.span));

                        solver.unify(ast[element].ty, usize);

                        ast.add_expr(
                            Self::ArrayIndex { target, element },
                            solver.add_unknown(None, Some(index_expr.index.span)),
                            span,
                        )
                    }
                }
            }
            mollie_parser::Expr::Binary(binary_expr) => {
                let lhs = Self::from_parsed_expr(binary_expr.lhs.value, ast, solver, binary_expr.lhs.span);
                let rhs = Self::from_parsed_expr(binary_expr.rhs.value, ast, solver, binary_expr.rhs.span);

                solver.unify(ast[rhs].ty, ast[lhs].ty);

                let ty = match binary_expr.operator.value {
                    Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan => {
                        solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), Some(binary_expr.operator.span))
                    }
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
                let result = TypePathResult::from_parsed_type_path(type_path_expr, solver, span);

                match result {
                    TypePathResult::VFunc(adt_ref, type_info_refs, vtable, func) => {
                        let target_ty = solver.add_info(TypeInfo::Adt(adt_ref, type_info_refs), None);
                        let ty = solver.context.vtables[vtable].functions[func].ty;
                        let ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]);

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
                        let ty = solver.add_info(TypeInfo::Adt(adt, type_info_refs), None);
                        let variant = match (solver.context.adt_types[adt].kind, variant) {
                            (AdtKind::Struct | AdtKind::Component, None) => AdtVariantRef::ZERO,
                            (AdtKind::Struct | AdtKind::Component, Some(_)) => todo!(),
                            (AdtKind::Enum, None) => {
                                let ty = solver.add_info(TypeInfo::Error, None);

                                return ast.add_expr(Self::Error(solver.context.error(TypeError::VariantRequired(adt), span)), ty, span);
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
                        let ty = solver.add_info(TypeInfo::Trait(trait_ref, type_args), None);

                        ast.add_expr(
                            Self::Error(solver.context.error(
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
                        let ty = solver.context.functions[func_ref].ty;
                        let ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]);

                        ast.add_expr(Self::Func(func_ref), ty, span)
                    }
                    TypePathResult::Intrinsic(intrinsic_kind) => todo!("value: intrinsic({intrinsic_kind:?})"),
                    TypePathResult::Generic(ty) => ast.add_expr(
                        Self::Error(solver.context.error(
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
                        let ty = solver.add_info(TypeInfo::Error, None);

                        ast.add_expr(
                            Self::Error(solver.context.error(
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
                        let ty = solver.add_info(TypeInfo::Error, None);

                        ast.add_expr(Self::Error(error), ty, span)
                    }
                }
            }
            mollie_parser::Expr::Array(array_expr) => {
                let element = solver.add_unknown(None, Some(span));
                let mut elements = Vec::with_capacity(array_expr.elements.capacity());

                for arr_element in array_expr.elements {
                    let arr_element = Self::from_parsed_expr(arr_element.value, ast, solver, arr_element.span);

                    solver.unify(ast[arr_element].ty, element);

                    elements.push(arr_element);
                }

                let elements = elements.into_boxed_slice();
                let size = elements.len();
                let array_ty = solver.add_info(TypeInfo::Array(element, Some(size)), Some(span));

                ast.add_expr(Self::Array { element, elements }, array_ty, span)
            }
            mollie_parser::Expr::IfElse(if_else_expr) => {
                let condition = Self::from_parsed_expr(if_else_expr.condition.value, ast, solver, if_else_expr.condition.span);
                let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), None);

                solver.unify(ast[condition].ty, expected);

                let block = Block::from_parsed_expr(if_else_expr.block.value, ast, solver, if_else_expr.block.span);
                let otherwise = if let Some(otherwise) = if_else_expr.else_block {
                    let expr = Self::from_parsed_expr(otherwise.value, ast, solver, otherwise.span);

                    solver.unify(ast[block].ty, ast[expr].ty);

                    Some(expr)
                } else {
                    let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None);

                    solver.unify(ast[block].ty, expected);

                    None
                };

                let ty = ast[block].ty;

                ast.add_expr(Self::IfElse { condition, block, otherwise }, ty, span)
            }
            mollie_parser::Expr::While(while_expr) => {
                let condition = Self::from_parsed_expr(while_expr.condition.value, ast, solver, while_expr.condition.span);
                let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), None);

                solver.unify(ast[condition].ty, expected);

                let block = Block::from_parsed_expr(while_expr.block.value, ast, solver, while_expr.block.span);
                let ty = ast[block].ty;
                let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None);

                solver.unify(ty, expected);

                ast.add_expr(Self::While { condition, block }, ty, span)
            }
            mollie_parser::Expr::Block(block_expr) => {
                let block = Block::from_parsed_expr(block_expr, ast, solver, span);
                let ty = ast[block].ty;

                ast.add_expr(Self::Block(block), ty, span)
            }
            mollie_parser::Expr::ForIn(for_in) => {
                let target_span = for_in.target.span;
                let target = Self::from_parsed_expr(for_in.target.value, ast, solver, for_in.target.span);
                let while_loop = (|| {
                    let into_iterator = solver.context.get_trait_item(LangItem::IntoIterator)?;
                    let ty = solver.solve(ast[target].ty);
                    let vtable = solver.context.find_vtable(ty, Some(into_iterator))?;

                    let (func_ty, func) = {
                        // let type_args = ast.use_vtable_impl(checker, ast[target].ty, vtable);
                        let func_ref = solver.context.get_trait_func_item(into_iterator, LangItem::IntoIteratorIntoIter)?.as_vfunc();
                        let func = &solver.context.vtables[vtable].functions[func_ref];

                        (
                            // if type_args.is_empty() {
                            TypeSolver::type_to_info(&mut solver.type_infos, solver.context, func.ty, &[]),
                            // } else {
                            //     checker.solver.solve_generic_args(func.ty, &type_args)
                            // },
                            func_ref,
                        )
                    };

                    let func = ast.add_expr(
                        Expr::VTableIndex {
                            target: Some(target),
                            target_ty: ast[target].ty,
                            vtable,
                            func,
                        },
                        func_ty,
                        target_span,
                    );

                    let TypeInfo::Func(_, returns) = solver.type_infos[solver.get_info(func_ty)].value else {
                        return None;
                    };

                    let value = ast.add_expr(Expr::Call { func, args: Box::new([]) }, returns, span);
                    let mut stmts = Vec::new();

                    stmts.push(ast.add_stmt(Stmt::NewVar {
                        mutable: true,
                        name: "!".into(),
                        value,
                    }));

                    let iterator = solver.context.get_trait_item(LangItem::Iterator)?;
                    let ty = solver.solve(returns);
                    let vtable = solver.context.find_vtable(ty, Some(iterator))?;

                    let (func_ty, func) = {
                        // let type_args = ast.use_vtable_impl(checker, returns, vtable);
                        let func_ref = solver.context.get_trait_func_item(iterator, LangItem::IteratorNext)?.as_vfunc();
                        let func = &solver.context.vtables[vtable].functions[func_ref];

                        (
                            // if type_args.is_empty() {
                            TypeSolver::type_to_info(&mut solver.type_infos, solver.context, func.ty, &[]),
                            // } else {
                            //     checker.solver.solve_generic_args(func.ty, &type_args)
                            // },
                            func_ref,
                        )
                    };

                    let target = ast.add_expr(Expr::Var(String::from("!")), returns, span);
                    let func = ast.add_expr(
                        Expr::VTableIndex {
                            target: Some(target),
                            target_ty: ast[target].ty,
                            vtable,
                            func,
                        },
                        func_ty,
                        target_span,
                    );

                    let TypeInfo::Func(_, returns) = solver.type_infos[solver.get_info(func_ty)].value else {
                        return None;
                    };

                    let item = ast.add_expr(Expr::Call { func, args: Box::new([]) }, returns, span);
                    let adt = solver.context.get_adt_item(LangItem::Option)?;
                    let adt_variant = solver.context.get_adt_variant_item(LangItem::OptionSome)?.1;

                    solver.push_frame();

                    if let TypeInfo::Adt(.., type_args) = &solver.type_infos[solver.get_info(returns)].value {
                        let type_args = type_args.clone();
                        let returns = solver.context.adt_types[adt].variants[adt_variant].fields[FieldRef::new(1)].ty;
                        let returns = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, returns, &type_args);

                        solver.set_var(&for_in.name.value.0, returns);
                    }

                    let adt_type_args = if let TypeInfo::Adt(.., generic_args) = &solver.type_infos[solver.get_info(returns)].value {
                        generic_args.clone()
                    } else {
                        Box::default()
                    };

                    let condition = ast.add_expr(
                        Expr::IsPattern {
                            target: item,
                            pattern: IsPattern::EnumVariant {
                                adt,
                                adt_variant,
                                adt_type_args,
                                values: Box::new([(FieldRef::new(1), for_in.name.value.0, None)]),
                            },
                        },
                        solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), None),
                        span,
                    );

                    let block = Block::from_parsed_expr(for_in.block.value, ast, solver, for_in.block.span);
                    let while_loop = ast.add_expr(Expr::While { condition, block }, ast[block].ty, span);

                    stmts.push(ast.add_stmt(Stmt::Expr(while_loop)));
                    solver.pop_frame();

                    let block = ast.add_block(
                        Block {
                            stmts: stmts.into_boxed_slice(),
                            expr: None,
                        },
                        solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None),
                        span,
                    );

                    let ty = ast[block].ty;

                    Some(ast.add_expr(Expr::Block(block), ty, span))
                })();

                while_loop.unwrap_or(target)
            }
            mollie_parser::Expr::Is(is_expr) => {
                let target = Self::from_parsed_expr(is_expr.target.value, ast, solver, is_expr.target.span);
                let pattern = IsPattern::from_parsed_expr(is_expr.pattern.value, ast, solver, is_expr.pattern.span);

                match &pattern {
                    IsPattern::Literal(_) => (),
                    IsPattern::EnumVariant { adt_type_args, .. } => {
                        if let TypeInfo::Adt(_, target_type_args) = &solver.type_infos[ast[target].ty].value {
                            let target_type_args = target_type_args.clone();

                            for (type_arg, target_type_arg) in adt_type_args.iter().copied().zip(target_type_args.into_iter()) {
                                solver.unify(type_arg, target_type_arg);
                            }
                        }
                    }
                    &IsPattern::TypeName { ty, .. } => {
                        if let TypeInfo::Adt(_, adt_type_args) = &solver.type_infos[ty].value {
                            if let TypeInfo::Adt(_, target_type_args) = &solver.type_infos[ast[target].ty].value {
                                let adt_type_args = adt_type_args.clone();
                                let target_type_args = target_type_args.clone();

                                for (type_arg, target_type_arg) in adt_type_args.into_iter().zip(target_type_args.into_iter()) {
                                    solver.unify(type_arg, target_type_arg);
                                }
                            }
                        }
                    }
                }

                ast.add_expr(
                    Self::IsPattern { target, pattern },
                    solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), None),
                    span,
                )
            }
            mollie_parser::Expr::Cast(expr, new_type) => {
                let expr = Self::from_parsed_expr(expr.value, ast, solver, expr.span);
                let ty = match new_type.value {
                    mollie_parser::PrimitiveType::IntSize => PrimitiveType::Int(IntType::ISize),
                    mollie_parser::PrimitiveType::Int64 => PrimitiveType::Int(IntType::I64),
                    mollie_parser::PrimitiveType::Int32 => PrimitiveType::Int(IntType::I32),
                    mollie_parser::PrimitiveType::Int16 => PrimitiveType::Int(IntType::I16),
                    mollie_parser::PrimitiveType::Int8 => PrimitiveType::Int(IntType::I8),
                    mollie_parser::PrimitiveType::UIntSize => PrimitiveType::UInt(UIntType::USize),
                    mollie_parser::PrimitiveType::UInt64 => PrimitiveType::UInt(UIntType::U64),
                    mollie_parser::PrimitiveType::UInt32 => PrimitiveType::UInt(UIntType::U32),
                    mollie_parser::PrimitiveType::UInt16 => PrimitiveType::UInt(UIntType::U16),
                    mollie_parser::PrimitiveType::UInt8 => PrimitiveType::UInt(UIntType::U8),
                    mollie_parser::PrimitiveType::Float => PrimitiveType::Float,
                    mollie_parser::PrimitiveType::Boolean => PrimitiveType::Boolean,
                    mollie_parser::PrimitiveType::String => PrimitiveType::String,
                    mollie_parser::PrimitiveType::Component => PrimitiveType::Component,
                    mollie_parser::PrimitiveType::Void => PrimitiveType::Void,
                };

                ast.add_expr(Self::TypeCast(expr, ty), solver.add_info(TypeInfo::Primitive(ty), Some(new_type.span)), span)
            }
            mollie_parser::Expr::Closure(closure_expr) => {
                solver.push_frame();

                let args: Box<[_]> = closure_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| {
                        let ty = solver.add_unknown(None, Some(arg.span));

                        solver.set_var(&arg.value.0, ty);

                        Arg {
                            name: arg.value.0,
                            kind: ArgType::Regular,
                            ty,
                        }
                    })
                    .collect();

                let body = Block::from_parsed_expr(closure_expr.body.value, ast, solver, closure_expr.body.span);
                let ty = solver.add_info(TypeInfo::Func(args.iter().map(|arg| arg.ty).collect(), ast[body].ty), Some(span));

                solver.pop_frame();

                ast.add_expr(Self::Closure { args, body }, ty, span)
            }
            mollie_parser::Expr::Ident(ident) => {
                if let Some(ty) = solver.get_var(&ident) {
                    ast.add_expr(Self::Var(ident.0), ty, span)
                } else if let Some(item) = solver.context.modules[ModuleId::ZERO].items.get(&ident) {
                    match item {
                        ModuleItem::SubModule(_) => {
                            let ty = solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(solver.context.error(
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
                            let found = TypeErrorValue::Adt(SpecialAdtKind::Specific(solver.context.adt_types[*adt].kind));
                            let ty = solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(solver.context.error(
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
                            let ty = solver.add_info(TypeInfo::Error, None);

                            ast.add_expr(
                                Self::Error(solver.context.error(
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
                            let ty = solver.context.functions[func_ref].ty;
                            let ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]);

                            ast.add_expr(Self::Func(func_ref), ty, span)
                        }
                        ModuleItem::Intrinsic(intrinsic_kind, _) => todo!("value: intrinsic({intrinsic_kind:?})"),
                    }
                } else {
                    let ty = solver.add_info(TypeInfo::Error, None);

                    ast.add_expr(
                        Self::Error(solver.context.error(
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
                if let Some(ty) = solver.get_var("self") {
                    ast.add_expr(Self::Var("self".to_string()), ty, span)
                } else {
                    let ty = solver.add_info(TypeInfo::Error, None);

                    ast.add_expr(
                        Self::Error(solver.context.error(
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
        };

        expr
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ArgType {
    This,
    Regular,
}

#[derive(Debug, Clone)]
pub struct Arg<T> {
    pub name: String,
    pub kind: ArgType,
    pub ty: T,
}

impl Stmt {
    fn from_parsed_stmt(stmt: mollie_parser::Stmt, ast: &mut TypedAST<FirstPass>, solver: &mut TypeSolver, span: Span) -> Option<StmtRef> {
        match stmt {
            mollie_parser::Stmt::Expression(expr) => {
                let expr = Expr::from_parsed_expr(expr, ast, solver, span);

                Some(ast.add_stmt(Self::Expr(expr)))
            }
            mollie_parser::Stmt::VariableDecl(variable_decl) => {
                let value = Expr::from_parsed_expr(variable_decl.value.value, ast, solver, variable_decl.value.span);

                solver.set_var(&variable_decl.name.value.0, ast[value].ty);

                Some(ast.add_stmt(Self::NewVar {
                    mutable: variable_decl.mutable.is_some(),
                    name: variable_decl.name.value.0,
                    value,
                }))
            }
            mollie_parser::Stmt::StructDecl(struct_decl) => {
                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.context.types.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let mut properties = IndexVec::new();

                for property in struct_decl.properties.value {
                    let name = property.value.name.value.0;
                    let ty = Type::from_parsed_type(property.value.ty.value, solver, property.value.ty.span);
                    let default_value = match property.value.default_value {
                        Some(value) => {
                            // let expected_ty = ty.as_type_info(None, &checker.core_types, &mut
                            // checker.solver, &[]); let value =
                            // value.into_typed_ast(checker, ast);

                            // checker.solver.unify(ast[value].ty, expected_ty);

                            // value.into_constant_value(checker, ast, &mut
                            // ConstantContext::default()).map_or_else(     |()| {
                            //         checker.add_error(TypeError::NonConstantEvaluable, ast[value].span);

                            None
                            //     },
                            //     Some,
                            // )
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
                    solver.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(solver.context.adt_types.len());

                solver.context.modules[ast.module]
                    .items
                    .insert(struct_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.context.adt_types.push(Adt {
                    name: Some(struct_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Struct,
                    generics: struct_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::ComponentDecl(component_decl) => {
                for (index, name) in component_decl.name.value.generics.iter().enumerate() {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.context.types.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let mut fields = IndexVec::with_capacity(component_decl.properties.len());

                for property in component_decl.properties {
                    let name = property.value.name.value.0;
                    let ty = Type::from_parsed_type(property.value.ty.value, solver, property.value.ty.span);
                    let default_value = match property.value.default_value {
                        Some(value) => {
                            // let expected_ty = ty.as_type_info(None, &checker.core_types, &mut
                            // checker.solver, &[]); let value =
                            // value.into_typed_ast(checker, ast);

                            // checker.solver.unify(ast[value].ty, expected_ty);

                            // value.into_constant_value(checker, ast, &mut
                            // ConstantContext::default()).map_or_else(     |()| {
                            //         checker.add_error(TypeError::NonConstantEvaluable, ast[value].span);

                            None
                            //     },
                            //     Some,
                            // )
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

                for name in &component_decl.name.value.generics {
                    solver.available_generics.remove(&name.value.0);
                }

                let adt_ref = AdtRef::new(solver.context.adt_types.len());

                solver.context.modules[ast.module]
                    .items
                    .insert(component_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.context.adt_types.push(Adt {
                    name: Some(component_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Component,
                    generics: component_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::TraitDecl(trait_decl) => {
                let trait_ref = solver.context.traits.next_index();

                let this = {
                    let ty_info = solver.add_info(TypeInfo::Generic(0), None);
                    let ty = solver.context.types.get_or_add(Type::Generic(0));

                    solver.available_generics.insert(String::from("<Self>"), (ty_info, ty));

                    (ty_info, ty)
                };

                for (index, name) in trait_decl.name.value.generics.iter().enumerate() {
                    let index = index + 1;
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.context.types.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
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
                        solver.context.language_items.insert(item, LangItemValue::TraitFunc(trait_ref, func_ref));
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
                        let ty = Type::from_parsed_type(arg.value.ty.value, solver, arg.value.ty.span);

                        args.push(Arg {
                            name,
                            kind: ArgType::Regular,
                            ty,
                        });
                    }

                    let args = args.into_boxed_slice();
                    let returns = match function.value.returns {
                        Some(returns) => Type::from_parsed_type(returns.value, solver, returns.span),
                        None => solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void)),
                    };

                    functions.push(TraitFunc { name, args, returns });
                }

                for name in &trait_decl.name.value.generics {
                    solver.available_generics.remove(&name.value.0);
                }

                solver.available_generics.remove("<Self>");

                if let Some(item) = trait_decl.attributes.iter().find_map(|attribute| {
                    attribute.value.value.as_ref().and_then(|v| {
                        if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                            Some(item)
                        } else {
                            None
                        }
                    })
                }) {
                    solver.context.language_items.insert(item, LangItemValue::Trait(trait_ref));
                }

                solver.context.traits.push(Trait {
                    name: trait_decl.name.value.name.value.0.clone(),
                    generics: trait_decl.name.value.generics.len(),
                    functions,
                });

                solver.context.modules[ast.module]
                    .items
                    .insert(trait_decl.name.value.name.value.0, ModuleItem::Trait(trait_ref));

                None
            }
            mollie_parser::Stmt::EnumDecl(enum_decl) => {
                let adt_ref = AdtRef::new(solver.context.adt_types.len());

                if let Some(item) = enum_decl.attributes.iter().find_map(|attribute| {
                    attribute.value.value.as_ref().and_then(|v| {
                        if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                            Some(item)
                        } else {
                            None
                        }
                    })
                }) {
                    solver.context.language_items.insert(item, LangItemValue::Adt(adt_ref));
                }

                for (index, name) in enum_decl.name.value.generics.iter().enumerate() {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.context.types.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let usize = solver.context.types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize)));

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
                            let ty = Type::from_parsed_type(property.value.ty.value, solver, property.value.ty.span);

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
                        solver
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
                    solver.available_generics.remove(&name.value.0);
                }

                solver.context.modules[ast.module]
                    .items
                    .insert(enum_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.context.adt_types.push(Adt {
                    name: Some(enum_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Enum,
                    generics: enum_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::FuncDecl(func_decl) => {
                let mut func_solver = solver.fork();

                let postfix = func_decl
                    .modifiers
                    .iter()
                    .any(|modifier| matches!(modifier.value, mollie_parser::FuncModifier::Postfix));

                let mut arg_names = Vec::with_capacity(func_decl.args.value.capacity());
                let mut args = Vec::with_capacity(func_decl.args.value.capacity());
                let mut arg_spans = Vec::with_capacity(func_decl.args.value.capacity());

                let mut ast = TypedAST {
                    module: ast.module,
                    ..TypedAST::default()
                };

                for arg in func_decl.args.value {
                    let ty = Type::from_parsed_type(arg.value.ty.value, &mut func_solver, arg.value.ty.span);
                    let type_info = TypeSolver::type_to_info(&mut func_solver.type_infos, &func_solver.context, ty, &[]);

                    func_solver.set_var(&arg.value.name.value, type_info);

                    arg_names.push(arg.value.name.value.0);
                    arg_spans.push(arg.span);
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
                    Type::from_parsed_type(returns.value, &mut func_solver, returns.span)
                } else {
                    func_solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                };

                let returns_info = TypeSolver::type_to_info(&mut func_solver.type_infos, &func_solver.context, returns, &[]);
                let body = Block::from_parsed_expr(func_decl.body.value, &mut ast, &mut func_solver, func_decl.body.span);

                func_solver.unify(ast[body].ty, returns_info);

                let (ast, entry) = ast.solve(body, &mut func_solver);

                let ty = solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));
                let func_ref = FuncRef::new(solver.context.functions.len());

                solver.context.modules[ast.module]
                    .items
                    .insert(func_decl.name.value.0.clone(), ModuleItem::Func(func_ref));

                println!("{} -> {:#?}", func_decl.name.value, ast.used_items);

                solver.context.functions.push(Func {
                    postfix,
                    name: func_decl.name.value.0,
                    arg_names,
                    ty,
                    kind: VTableFuncKind::Local { ast, entry },
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
                    .map(|index| solver.context.types.get_or_add(Type::Generic(index)))
                    .collect();

                let (trait_name, origin_trait) = match implementation.trait_name {
                    Some(trait_name) => {
                        if let ModuleItem::Trait(trait_ref) = ModuleItem::from_parsed_type_path(trait_name.value, solver, trait_name.span) {
                            (Some(solver.context.traits[trait_ref].name.clone()), Some(trait_ref))
                        } else {
                            (None, None)
                        }
                    }
                    None => (None, None),
                };

                for (index, (name, span)) in once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                    .flatten()
                    .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                    .enumerate()
                {
                    let ty = solver.context.types.get_or_add(Type::Generic(index));
                    let ty_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]);

                    solver.available_generics.insert(name, (ty_info, ty));
                }

                let ty = Type::from_parsed_type(implementation.target.value, solver, implementation.target.span);

                let vtable = match solver.context.get_vtable(ty, origin_trait) {
                    Some(vtable) => vtable,
                    None => solver.context.vtables.insert(VTableGenerator {
                        ty,
                        generics,
                        origin_trait,
                        functions: IndexVec::new(),
                    }),
                };

                if let (Some(trait_name), Some(trait_ref)) = (&trait_name, origin_trait) {
                    let trait_functions = solver.context.traits[trait_ref]
                        .functions
                        .iter()
                        .map(|(k, func)| (k, func.name.clone()))
                        .collect::<Box<[_]>>();

                    for (func_ref, name) in trait_functions {
                        if let Some(func) = implementation.functions.value.iter().position(|func| func.value.name.value.0 == name) {
                            let function = implementation.functions.value.remove(func);
                            let mut func_solver = solver.fork();
                            let mut ast = TypedAST {
                                module: ast.module,
                                ..TypedAST::default()
                            };

                            for (index, (name, span)) in once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                                .flatten()
                                .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                                .enumerate()
                            {
                                let ty = func_solver.context.types.get_or_add(Type::Generic(index));
                                let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                                func_solver.available_generics.insert(name, (ty_info, ty));
                            }

                            let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                            let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                            if function.value.this.is_some() {
                                let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                                func_solver.set_var("self", ty_info);

                                arg_names.push("self".to_string());
                                args.push(ty);
                            }

                            for arg in function.value.args {
                                let ty = Type::from_parsed_type(arg.value.ty.value, &mut func_solver, arg.value.ty.span);
                                let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                                func_solver.set_var(&arg.value.name.value.0, ty_info);

                                // if let TypeInfo::Adt(adt_ref, _, adt_args) = checker.solver.get_info(ty) {
                                //     ast.use_item(UsedItem::Adt(*adt_ref, adt_args.clone()));
                                // }

                                arg_names.push(arg.value.name.value.0);
                                args.push(ty);
                            }

                            let returns = if let Some(returns) = function.value.returns {
                                Type::from_parsed_type(returns.value, &mut func_solver, returns.span)
                            } else {
                                func_solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                            };

                            let returns_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, returns, &[]);
                            let body = Block::from_parsed_expr(function.value.body.value, &mut ast, &mut func_solver, function.value.body.span);

                            func_solver.unify(ast[body].ty, returns_info);

                            let ty = func_solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                            let (ast, entry) = ast.solve(body, &mut func_solver);

                            solver.context.vtables[vtable].functions.push(VTableFunc {
                                trait_func: Some(func_ref),
                                name: function.value.name.value.0,
                                arg_names,
                                ty,
                                kind: VTableFuncKind::Local { ast, entry },
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

                    let mut func_solver = solver.fork();
                    let mut ast = TypedAST {
                        module: ast.module,
                        ..TypedAST::default()
                    };

                    for (index, (name, span)) in once_with(|| if origin_trait.is_some() { Some((String::from("<Self>"), None)) } else { None })
                        .flatten()
                        .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                        .enumerate()
                    {
                        let ty = func_solver.context.types.get_or_add(Type::Generic(index));
                        let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                        func_solver.available_generics.insert(name, (ty_info, ty));
                    }

                    let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                    let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                        func_solver.set_var("self", ty_info);

                        arg_names.push("self".to_string());
                        args.push(ty);
                    }

                    for arg in function.value.args {
                        let ty = Type::from_parsed_type(arg.value.ty.value, &mut func_solver, arg.value.ty.span);
                        let ty_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, ty, &[]);

                        func_solver.set_var(&arg.value.name.value.0, ty_info);

                        // if let TypeInfo::Adt(adt_ref, _, adt_args) = checker.solver.get_info(ty) {
                        //     ast.use_item(UsedItem::Adt(*adt_ref, adt_args.clone()));
                        // }

                        arg_names.push(arg.value.name.value.0);
                        args.push(ty);
                    }

                    let returns = if let Some(returns) = function.value.returns {
                        Type::from_parsed_type(returns.value, &mut func_solver, returns.span)
                    } else {
                        func_solver.context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
                    };

                    let returns_info = TypeSolver::type_to_info(&mut func_solver.type_infos, func_solver.context, returns, &[]);
                    let body = Block::from_parsed_expr(function.value.body.value, &mut ast, &mut func_solver, function.value.body.span);

                    func_solver.unify(ast[body].ty, returns_info);

                    let ty = func_solver.context.types.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                    let (ast, entry) = ast.solve(body, &mut func_solver);

                    solver.context.vtables[vtable].functions.push(VTableFunc {
                        trait_func: None,
                        name: function.value.name.value.0,
                        arg_names,
                        ty,
                        kind: VTableFuncKind::Local { ast, entry },
                    });
                }

                for generic in implementation.generics {
                    solver.available_generics.remove(&generic.value.0);
                }

                if origin_trait.is_some() {
                    solver.available_generics.remove("<Self>");
                }

                None
            }
            mollie_parser::Stmt::Import(import) => {
                let _path_span = import.path.span;
                let path = ModuleItem::from_parsed_type_path(import.path.value, solver, import.path.span);

                #[allow(clippy::single_match)]
                match path {
                    ModuleItem::SubModule(module) => match import.kind {
                        mollie_parser::ImportKind::Partial(items) => {
                            for item in items.value {
                                let name = item.value.0;

                                if let Some(item) = solver.context.modules[module].items.get(&name).copied() {
                                    solver.context.modules[ModuleId::ZERO].items.insert(name, item);
                                }
                            }
                        }
                        mollie_parser::ImportKind::Named => {
                            let name = solver.context.modules[module].name.clone();

                            solver.context.modules[ModuleId::ZERO].items.insert(name, ModuleItem::SubModule(module));
                        }
                    },
                    _ => todo!("error or value depending on import"),
                }

                None
            }
        }
    }
}

enum TypePathResult {
    VFunc(AdtRef, Box<[TypeInfoRef]>, VTableRef, VFuncRef),
    Adt(AdtRef, Box<[TypeInfoRef]>, Option<AdtVariantRef>),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Func(FuncRef),
    Intrinsic(IntrinsicKind),
    Generic(TypeInfoRef),
    Module(ModuleId),
    Error(TypeErrorRef, Span),
}

impl ModuleItem {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> Self {
        let mut span = None;
        let mut result = Self::SubModule(ModuleId::ZERO);

        for segment in path.segments {
            match result {
                Self::SubModule(current_module) => {
                    if let Some(item) = solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                        result = *item;

                        span.replace(segment.span);
                    } else {
                        todo!("error: there's no item called `{}` in module", segment.value.name.value)
                    }
                }
                result => {
                    let found = match result {
                        ModuleItem::SubModule(_) => TypeErrorValue::Module,
                        ModuleItem::Adt(adt_ref) => TypeErrorValue::Adt(SpecialAdtKind::Specific(solver.context.adt_types[adt_ref].kind)),
                        ModuleItem::Trait(_) => TypeErrorValue::Trait,
                        ModuleItem::Func(_) => TypeErrorValue::Function,
                        ModuleItem::Intrinsic(..) => TypeErrorValue::Function,
                    };

                    match span {
                        Some(span) => {
                            solver.context.error(
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

impl TypePathResult {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> Self {
        let mut result = Self::Module(ModuleId::ZERO);
        let segment_count = path.segments.len();

        for (i, segment) in path.segments.into_iter().enumerate() {
            if let Some((typo, _)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = Self::Generic(typo);

                break;
            }

            match result {
                Self::Adt(adt, type_args, None) => {
                    if let Some(variant) = solver.context.adt_types[adt].variants.iter().find_map(|(variant_ref, variant)| {
                        if variant.name.as_deref() == Some(segment.value.name.value.0.as_str()) {
                            Some(variant_ref)
                        } else {
                            None
                        }
                    }) {
                        result = Self::Adt(adt, type_args, Some(variant));
                    } else {
                        let storage_type_args = type_args.iter().map(|&type_arg| solver.solve(type_arg)).collect();
                        let storage_ty = solver.context.types.get_or_add(Type::Adt(adt, storage_type_args));

                        if let Some((vtable, vfunc)) = solver.context.find_vtable_by_func(storage_ty, &segment.value.name.value.0) {
                            result = Self::VFunc(adt, type_args, vtable, vfunc);
                        } else {
                            result = Self::Adt(adt, type_args, None);
                        }
                    }
                }
                Self::Module(current_module) => {
                    if let Some(item) = solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                        match *item {
                            ModuleItem::SubModule(module_id) => result = Self::Module(module_id),
                            ModuleItem::Adt(adt_ref) => {
                                let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                    type_args
                                        .value
                                        .0
                                        .into_iter()
                                        .map(|arg| TypeInfo::from_parsed_type(arg.value, solver, arg.span))
                                        .collect()
                                });

                                let args = (0..solver.context.adt_types[adt_ref].generics)
                                    .rev()
                                    .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(TypeInfo::Generic(generic), None)))
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
                                        .map(|arg| TypeInfo::from_parsed_type(arg.value, solver, arg.span))
                                        .collect()
                                });

                                let args = (0..solver.context.traits[trait_ref].generics)
                                    .rev()
                                    .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(TypeInfo::Generic(generic), None)))
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
                                solver.context.error(
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
                                solver.context.error(
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

impl TypeInfo {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> TypeInfoRef {
        enum TypePathResult {
            Type(TypeInfoRef),
            Module(ModuleId),
        }

        let mut span = None;
        let mut result = TypePathResult::Module(ModuleId::ZERO);

        for segment in path.segments {
            if let Some((typo, _)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let TypePathResult::Module(current_module) = result {
                if let Some(item) = solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                    span.replace(segment.span);

                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.context.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(Self::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.add_info(Self::Adt(adt_ref, args), None));
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.context.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(Self::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.add_info(Self::Trait(trait_ref, args), None));
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
                                &mut solver.type_infos,
                                solver.context,
                                solver.context.functions[func].ty,
                                &[],
                            ));
                        }
                        ModuleItem::Intrinsic(..) => {
                            // result = Self::Intrinsic(kind);
                        }
                    }
                } else {
                    solver.context.error(
                        TypeError::NotFound {
                            name: segment.value.name.value.0,
                            was_looking_for: LookupType::Type { inside: current_module },
                        },
                        segment.value.name.span,
                    );

                    break;
                }
            }
        }

        match result {
            TypePathResult::Type(type_info_ref) => type_info_ref,
            TypePathResult::Module(_) => {
                match span {
                    Some(span) => {
                        solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Type,
                                found: TypeErrorValue::Module,
                            },
                            span,
                        );
                    }
                    None => todo!(),
                }

                solver.add_info(TypeInfo::Error, None)
            }
        }
    }

    fn from_parsed_type(ty: mollie_parser::Type, solver: &mut TypeSolver, span: Span) -> TypeInfoRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => solver.add_info(
                Self::Primitive(match primitive_type {
                    mollie_parser::PrimitiveType::IntSize => PrimitiveType::Int(IntType::ISize),
                    mollie_parser::PrimitiveType::Int64 => PrimitiveType::Int(IntType::I64),
                    mollie_parser::PrimitiveType::Int32 => PrimitiveType::Int(IntType::I32),
                    mollie_parser::PrimitiveType::Int16 => PrimitiveType::Int(IntType::I16),
                    mollie_parser::PrimitiveType::Int8 => PrimitiveType::Int(IntType::I8),
                    mollie_parser::PrimitiveType::UIntSize => PrimitiveType::UInt(UIntType::USize),
                    mollie_parser::PrimitiveType::UInt64 => PrimitiveType::UInt(UIntType::U64),
                    mollie_parser::PrimitiveType::UInt32 => PrimitiveType::UInt(UIntType::U32),
                    mollie_parser::PrimitiveType::UInt16 => PrimitiveType::UInt(UIntType::U16),
                    mollie_parser::PrimitiveType::UInt8 => PrimitiveType::UInt(UIntType::U8),
                    mollie_parser::PrimitiveType::Float => PrimitiveType::Float,
                    mollie_parser::PrimitiveType::Boolean => PrimitiveType::Boolean,
                    mollie_parser::PrimitiveType::String => PrimitiveType::String,
                    mollie_parser::PrimitiveType::Component => PrimitiveType::Component,
                    mollie_parser::PrimitiveType::Void => PrimitiveType::Void,
                }),
                Some(span),
            ),
            mollie_parser::Type::Array(element, size) => {
                let element = Self::from_parsed_type(element.value, solver, element.span);

                solver.add_info(Self::Array(element, size.map(|size| size.value)), Some(span))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed_type(arg.value, solver, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed_type(ty.value, solver, ty.span),
                    None => solver.add_info(Self::Primitive(PrimitiveType::Void), Some(span)),
                };

                solver.add_info(Self::Func(args, returns), Some(span))
            }
            mollie_parser::Type::Path(type_path_expr) => Self::from_parsed_type_path(type_path_expr, solver, span),
        }
    }
}

impl Type {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> TypeRef {
        enum TypePathResult {
            Type(TypeRef),
            Module(ModuleId),
        }

        let mut span = None;
        let mut result = TypePathResult::Module(ModuleId::ZERO);

        for segment in path.segments {
            if let Some((_, typo)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let &TypePathResult::Module(current_module) = &result {
                if let Some(item) = solver.context.modules[current_module].items.get(&segment.value.name.value.0) {
                    span.replace(segment.span);

                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.context.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.context.types.get_or_add(Self::Generic(generic))))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.context.types.get_or_add(Self::Adt(adt_ref, args)));
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Self::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.context.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.context.types.get_or_add(Self::Generic(generic))))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.context.types.get_or_add(Self::Trait(trait_ref, args)));
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

                            result = TypePathResult::Type(solver.context.functions[func_ref].ty);
                        }
                        ModuleItem::Intrinsic(..) => {
                            // result = TypePath::Intrinsic(kind, ty);
                        }
                    }
                } else {
                    solver.context.error(
                        TypeError::NotFound {
                            name: segment.value.name.value.0,
                            was_looking_for: LookupType::Type { inside: current_module },
                        },
                        segment.value.name.span,
                    );

                    break;
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
                        solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Type,
                                found: TypeErrorValue::Module,
                            },
                            span,
                        );
                    }
                    None => todo!(),
                }

                solver.context.types.get_or_add(Type::Error)
            }
        }
    }

    fn from_parsed_type(ty: mollie_parser::Type, solver: &mut TypeSolver, span: Span) -> TypeRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => solver.context.types.get_or_add(Self::Primitive(match primitive_type {
                mollie_parser::PrimitiveType::IntSize => PrimitiveType::Int(IntType::ISize),
                mollie_parser::PrimitiveType::Int64 => PrimitiveType::Int(IntType::I64),
                mollie_parser::PrimitiveType::Int32 => PrimitiveType::Int(IntType::I32),
                mollie_parser::PrimitiveType::Int16 => PrimitiveType::Int(IntType::I16),
                mollie_parser::PrimitiveType::Int8 => PrimitiveType::Int(IntType::I8),
                mollie_parser::PrimitiveType::UIntSize => PrimitiveType::UInt(UIntType::USize),
                mollie_parser::PrimitiveType::UInt64 => PrimitiveType::UInt(UIntType::U64),
                mollie_parser::PrimitiveType::UInt32 => PrimitiveType::UInt(UIntType::U32),
                mollie_parser::PrimitiveType::UInt16 => PrimitiveType::UInt(UIntType::U16),
                mollie_parser::PrimitiveType::UInt8 => PrimitiveType::UInt(UIntType::U8),
                mollie_parser::PrimitiveType::Float => PrimitiveType::Float,
                mollie_parser::PrimitiveType::Boolean => PrimitiveType::Boolean,
                mollie_parser::PrimitiveType::String => PrimitiveType::String,
                mollie_parser::PrimitiveType::Component => PrimitiveType::Component,
                mollie_parser::PrimitiveType::Void => PrimitiveType::Void,
            })),
            mollie_parser::Type::Array(element, size) => {
                let element = Self::from_parsed_type(element.value, solver, element.span);

                solver.context.types.get_or_add(Self::Array(element, size.map(|size| size.value)))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed_type(arg.value, solver, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed_type(ty.value, solver, ty.span),
                    None => solver.context.types.get_or_add(Self::Primitive(PrimitiveType::Void)),
                };

                solver.context.types.get_or_add(Self::Func(args, returns))
            }
            mollie_parser::Type::Path(type_path_expr) => Self::from_parsed_type_path(type_path_expr, solver, span),
        }
    }
}

impl Block {
    fn from_parsed_expr(expr: mollie_parser::BlockExpr, ast: &mut TypedAST<FirstPass>, solver: &mut TypeSolver, span: Span) -> BlockRef {
        let mut stmts = Vec::new();

        solver.push_frame();

        for stmt in expr.stmts {
            if let Some(stmt) = Stmt::from_parsed_stmt(stmt.value, ast, solver, stmt.span) {
                stmts.push(stmt);
            }
        }

        let stmts = stmts.into_boxed_slice();

        let (expr, ty) = match expr.final_stmt.map(|v| *v) {
            Some(Positioned {
                value: mollie_parser::Stmt::Expression(expr),
                span,
            }) => {
                let expr = Expr::from_parsed_expr(expr, ast, solver, span);

                (Some(expr), ast[expr].ty)
            }
            _ => (None, solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None)),
        };

        solver.pop_frame();

        ast.add_block(Self { stmts, expr }, ty, span)
    }
}

impl TypedAST<FirstPass> {
    fn solve_expr(&self, ast: &mut TypedAST<SolvedPass>, expr: ExprRef, solver: &mut TypeSolver) -> ExprRef {
        let value = match self[expr].value.clone() {
            Expr::Lit(lit_expr) => Expr::Lit(lit_expr),
            Expr::Var(var) => Expr::Var(var),
            Expr::Binary { operator, lhs, rhs } => {
                let lhs = self.solve_expr(ast, lhs, solver);
                let rhs = self.solve_expr(ast, rhs, solver);

                Expr::Binary { operator, lhs, rhs }
            }
            Expr::Construct { adt, variant, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|(field_ref, field_type, field_value)| {
                        let expr = if field_value == ExprRef::INVALID && matches!(solver.context.adt_types[adt].kind, AdtKind::Enum) {
                            field_value
                        } else {
                            self.solve_expr(ast, field_value, solver)
                        };

                        let field_type = solver.get_info(field_type);

                        (field_ref, solver.solve(field_type), expr)
                    })
                    .collect();

                Expr::Construct { adt, variant, fields }
            }
            Expr::AdtIndex { target, field } => {
                let target = self.solve_expr(ast, target, solver);

                if let Some((vtable, func)) = solver.context.find_vtable_by_func(ast[target].ty, &field) {
                    let ty = solver.context.vtables[vtable].functions[func].ty;
                    let type_args: Box<[_]> = solver.context.vtables[vtable]
                        .generics
                        .iter()
                        .copied()
                        .map(|ty| TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]))
                        .collect();

                    let solved_origin_ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ast[target].ty, &[]);
                    let origin_ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, solver.context.vtables[vtable].ty, &type_args);

                    if solver.context.vtables[vtable].origin_trait.is_some() {
                        solver.unify(type_args[0], origin_ty);
                    }

                    solver.unify(origin_ty, solved_origin_ty);

                    let info = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &type_args);

                    solver.unify(self[expr].ty, info);

                    let type_args = type_args.into_iter().map(|ty| solver.solve(ty)).collect();

                    ast.use_item(
                        &solver.context.vtables,
                        &solver.context.functions,
                        &mut solver.context.types,
                        UsedItem::VTable(ast[target].ty, vtable, type_args),
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
                }

                if let Type::Adt(adt_ref, args) = &solver.context.types[ast[target].ty] {
                    let adt = &solver.context.adt_types[*adt_ref];

                    if matches!(adt.kind, AdtKind::Enum) {
                        let ty = solver.context.types.get_or_add(Type::Error);

                        return ast.add_expr(
                            Expr::Error(solver.context.error(
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
                        let ty = solver.context.types.get_or_add(Type::Error);

                        return ast.add_expr(
                            Expr::Error(solver.context.error(
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
                    let ty = solver.context.types.apply_type_args(ty, &args);

                    let info = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, ty, &[]);

                    solver.unify(self[expr].ty, info);

                    return ast.add_expr(Expr::AdtIndex { target, field }, ty, self[expr].span);
                } else {
                    let ty = solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::Adt(SpecialAdtKind::WithExpectation(AdtKind::Enum)),
                                found: TypeErrorValue::ExplicitType(ast[target].ty),
                            },
                            ast[target].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                };
            }
            Expr::ArrayIndex { target, element } => {
                let target = self.solve_expr(ast, target, solver);
                let Type::Array(element_ty, _) = solver.context.types[ast[target].ty] else {
                    let ty = solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(solver.context.error(
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

                let element = self.solve_expr(ast, element, solver);
                let info = TypeSolver::type_to_info(&mut solver.type_infos, solver.context, element_ty, &[]);

                solver.unify(self[expr].ty, info);

                return ast.add_expr(Expr::ArrayIndex { target, element }, element_ty, self[expr].span);
            }
            Expr::Closure { args, body } => {
                let args = args
                    .into_iter()
                    .map(|arg| Arg {
                        name: arg.name,
                        kind: arg.kind,
                        ty: solver.solve(arg.ty),
                    })
                    .collect();

                let body = self.solve_block(ast, body, solver);

                Expr::Closure { args, body }
            }
            Expr::Call { func, args } => {
                let func = self.solve_expr(ast, func, solver);
                let args: Box<[_]> = args.into_iter().map(|arg| self.solve_expr(ast, arg, solver)).collect();

                if let Type::Func(arg_types, _) = &solver.context.types[ast[func].ty] {
                    let arg_types = arg_types.clone();
                    let expected = arg_types.len() - usize::from(matches!(ast[func].value, Expr::VTableIndex { target: Some(_), .. }));
                    let found = args.len();

                    if found != expected {
                        solver.context.error(TypeError::ArgumentCountMismatch { expected, found }, self[expr].span);
                    }

                    for (arg, arg_type) in args.iter().copied().zip(arg_types) {
                        if ast[arg].ty != arg_type {
                            solver.context.error(
                                TypeError::Unexpected {
                                    expected: TypeErrorValue::ExplicitType(arg_type),
                                    found: TypeErrorValue::ExplicitType(ast[arg].ty),
                                },
                                ast[arg].span,
                            );
                        }
                    }
                } else {
                    let ty = solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(solver.context.error(
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

                Expr::Call { func, args }
            }
            Expr::Array { element, elements } => {
                let element = solver.solve(element);
                let elements = elements.into_iter().map(|element| self.solve_expr(ast, element, solver)).collect();

                Expr::Array { element, elements }
            }
            Expr::IfElse { condition, block, otherwise } => {
                let condition = self.solve_expr(ast, condition, solver);

                if solver.context.types[ast[condition].ty] != Type::Primitive(PrimitiveType::Boolean) {
                    let ty = solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::PrimitiveType(PrimitiveType::Boolean),
                                found: TypeErrorValue::ExplicitType(ast[condition].ty),
                            },
                            ast[condition].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                }

                let block = self.solve_block(ast, block, solver);
                let otherwise = otherwise.map(|otherwise| self.solve_expr(ast, otherwise, solver));

                Expr::IfElse { condition, block, otherwise }
            }
            Expr::While { condition, block } => {
                let condition = self.solve_expr(ast, condition, solver);

                if solver.context.types[ast[condition].ty] != Type::Primitive(PrimitiveType::Boolean) {
                    let ty = solver.context.types.get_or_add(Type::Error);

                    return ast.add_expr(
                        Expr::Error(solver.context.error(
                            TypeError::Unexpected {
                                expected: TypeErrorValue::PrimitiveType(PrimitiveType::Boolean),
                                found: TypeErrorValue::ExplicitType(ast[condition].ty),
                            },
                            ast[condition].span,
                        )),
                        ty,
                        self[expr].span,
                    );
                }

                let block = self.solve_block(ast, block, solver);

                Expr::While { condition, block }
            }
            Expr::Block(block) => {
                let block = self.solve_block(ast, block, solver);

                Expr::Block(block)
            }
            Expr::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => {
                let target = target.map(|target| self.solve_expr(ast, target, solver));
                let target_ty = solver.solve(target_ty);

                Expr::VTableIndex {
                    target,
                    target_ty,
                    vtable,
                    func,
                }
            }
            Expr::TypeCast(expr, ty) => {
                let expr = self.solve_expr(ast, expr, solver);

                Expr::TypeCast(expr, ty)
            }
            Expr::IsPattern { target, pattern } => {
                fn solve_pattern(
                    first_pass: &TypedAST<FirstPass>,
                    ast: &mut TypedAST<SolvedPass>,
                    pattern: IsPattern<FirstPass>,
                    solver: &mut TypeSolver,
                ) -> IsPattern<SolvedPass> {
                    match pattern {
                        IsPattern::Literal(expr) => {
                            let expr = first_pass.solve_expr(ast, expr, solver);

                            IsPattern::Literal(expr)
                        }
                        IsPattern::EnumVariant {
                            adt,
                            adt_variant,
                            adt_type_args,
                            values,
                        } => {
                            let adt_type_args = adt_type_args.into_iter().map(|type_arg| solver.solve(type_arg)).collect();
                            let values = values
                                .into_iter()
                                .map(|(field, name, pattern)| (field, name, pattern.map(|pattern| solve_pattern(first_pass, ast, pattern, solver))))
                                .collect();

                            IsPattern::EnumVariant {
                                adt,
                                adt_variant,
                                adt_type_args,
                                values,
                            }
                        }
                        IsPattern::TypeName { ty, name } => {
                            let ty = solver.solve(ty);

                            IsPattern::TypeName { ty, name }
                        }
                    }
                }

                let target = self.solve_expr(ast, target, solver);
                let pattern = solve_pattern(self, ast, pattern, solver);

                Expr::IsPattern { target, pattern }
            }
            Expr::Func(func) => {
                ast.use_item(
                    &solver.context.vtables,
                    &solver.context.functions,
                    &mut solver.context.types,
                    UsedItem::Func(func),
                );

                return ast.add_expr(Expr::Func(func), solver.context.functions[func].ty, self[expr].span);
            }
            Expr::Error(error) => Expr::Error(error),
        };

        let ty = solver.solve(self[expr].ty);

        if let (Expr::Construct { .. }, Type::Adt(adt, type_args)) = (&value, &solver.context.types[ty]) {
            let item = UsedItem::Adt(*adt, type_args.clone());

            ast.use_item(&solver.context.vtables, &solver.context.functions, &mut solver.context.types, item);
        }

        ast.add_expr(value, ty, self[expr].span)
    }

    fn solve_block(&self, ast: &mut TypedAST<SolvedPass>, block: BlockRef, solver: &mut TypeSolver) -> BlockRef {
        let input_block = &self.blocks[block];
        let output_block = Block {
            stmts: input_block
                .value
                .stmts
                .iter()
                .map(|&stmt| {
                    let stmt = match &self[stmt] {
                        &Stmt::Expr(expr) => Stmt::Expr(self.solve_expr(ast, expr, solver)),
                        Stmt::NewVar { mutable, name, value } => Stmt::NewVar {
                            mutable: *mutable,
                            name: name.clone(),
                            value: self.solve_expr(ast, *value, solver),
                        },
                    };

                    ast.add_stmt(stmt)
                })
                .collect(),
            expr: input_block.value.expr.map(|expr| self.solve_expr(ast, expr, solver)),
        };

        ast.add_block(output_block, solver.solve(input_block.ty), input_block.span)
    }

    fn solve(self, root: BlockRef, solver: &mut TypeSolver) -> (TypedAST<SolvedPass>, BlockRef) {
        solver.finalize();

        let mut ast = TypedAST {
            module: self.module,
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
            used_items: Vec::new(),
        };

        let output_block = self.solve_block(&mut ast, root, solver);

        (ast, output_block)
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use itertools::Itertools;
    use mollie_index::Idx;
    use mollie_parser::Parse;
    use mollie_typing::{AdtKind, IntType, PrimitiveType, UIntType};

    use super::Stmt;
    use crate::{
        Block, BlockRef, ExprRef, ModuleId,
        v2::{
            Expr, IsPattern, LitExpr, LookupType, SolvedPass, SpecialAdtKind, Type, TypeContext, TypeError, TypeErrorValue, TypeInfo, TypeRef, TypedAST,
            UsedItem,
        },
    };

    struct TypeFmt<'a> {
        storage: &'a TypeContext,
        ty: TypeRef,
    }

    impl fmt::Display for TypeFmt<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.storage.types[self.ty] {
                Type::Primitive(primitive_type) => primitive_type.fmt(f),
                &Type::Array(ty, size) => match size {
                    Some(size) => write!(f, "{}[{size}]", TypeFmt { storage: self.storage, ty }),
                    None => write!(f, "{}[]", TypeFmt { storage: self.storage, ty }),
                },
                Type::Adt(adt_ref, type_args) => write!(
                    f,
                    "{}<{}>",
                    self.storage.adt_types[*adt_ref].name.as_deref().unwrap_or_default(),
                    type_args.iter().map(|&ty| TypeFmt { storage: self.storage, ty }).join(", "),
                ),
                Type::Trait(trait_ref, type_args) => write!(
                    f,
                    "{}<{}>",
                    self.storage.traits[*trait_ref].name,
                    type_args.iter().map(|&ty| TypeFmt { storage: self.storage, ty }).join(", "),
                ),
                Type::Generic(generic) => write!(f, "<generic({generic})>"),
                Type::Func(args, returns) => {
                    write!(
                        f,
                        "({}) -> {}",
                        args.iter().map(|&ty| TypeFmt { storage: self.storage, ty }).join(", "),
                        TypeFmt {
                            storage: self.storage,
                            ty: *returns
                        }
                    )
                }
                Type::Error => f.write_str("<error>"),
            }
        }
    }

    struct TypeBlockFmt<'a> {
        ast: &'a TypedAST<SolvedPass>,
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
                            TypeFmt {
                                storage: self.storage,
                                ty: self.ast[*value].ty
                            },
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
        ast: &'a TypedAST<SolvedPass>,
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
                        format!(
                            "::<{}>",
                            adt_type_args.iter().copied().map(|ty| TypeFmt { storage: self.storage, ty }).join(", ")
                        )
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
                IsPattern::TypeName { ty, name } => write!(f, "{} {name}", TypeFmt {
                    storage: self.storage,
                    ty: *ty
                }),
            }
        }
    }

    struct TypeExprFmt<'a> {
        ast: &'a TypedAST<SolvedPass>,
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
            match &self.ast[self.expr].value {
                Expr::Var(name) => f.write_str(name),
                Expr::Lit(value) => match value {
                    LitExpr::Boolean(value) => write!(f, "{value}"),
                    LitExpr::Float(value) => write!(f, "{value}"),
                    LitExpr::Int(value) => write!(f, "{value}{}", TypeFmt {
                        storage: self.storage,
                        ty: self.ast[self.expr].ty
                    }),
                    LitExpr::String(value) => write!(f, "{value:?}"),
                },
                Expr::Array { elements, .. } => {
                    write!(f, "[{}]", elements.iter().map(|&element| self.fork(element)).join(", "))
                }
                &Expr::Binary { operator, lhs, rhs } => {
                    write!(f, "{} {} {}", self.fork(lhs), operator.value, self.fork(rhs))
                }
                Expr::Closure { args, body } => {
                    write!(
                        f,
                        "|{}| {{ {} }}",
                        args.iter()
                            .map(|arg| format!("{}: {}", arg.name, TypeFmt {
                                storage: self.storage,
                                ty: arg.ty
                            }))
                            .join(", "),
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
                        "({} as {}).{}",
                        self.fork(target),
                        self.storage.traits[origin_trait].name,
                        self.storage.vtables[vtable].functions[func].name
                    ),
                    (Some(target), None) => write!(f, "{}.{}", self.fork(target), self.storage.vtables[vtable].functions[func].name),
                    (None, Some(origin_trait)) => write!(
                        f,
                        "({} as {}).{}",
                        TypeFmt {
                            storage: self.storage,
                            ty: target_ty
                        },
                        self.storage.traits[origin_trait].name,
                        self.storage.vtables[vtable].functions[func].name
                    ),
                    (None, None) => write!(
                        f,
                        "{}.{}",
                        TypeFmt {
                            storage: self.storage,
                            ty: target_ty
                        },
                        self.storage.vtables[vtable].functions[func].name
                    ),
                },
                &Expr::ArrayIndex { target, element } => {
                    write!(f, "{}[{}]", self.fork(target), self.fork(element),)
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
                &Expr::Func(func_ref) => self.storage.functions[func_ref].name.fmt(f),
                Expr::Error(_) => f.write_str("<error>"),
            }
        }
    }

    #[test]
    fn test_literal() {
        let source = "{
            let hello = 12;
            let hello2 = [1, 2, 4int16];
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
                fn hello(self) -> A<T>;
            }

            struct C<T> {
                hi: Hello<T>
            }

            impl<T> Hello<T> for A<T> {
                fn hello(self) -> A<T> {
                    let hew = A { value: true };

                    A { value: self.value }
                }
            }

            fn println(input: int64) -> int64 {
                let dengi = Option::Some { value: A { value: 5int16 } };

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

            hello = 54int64;

            calc_smth(|b| { b * 2 });

            hello = hello + 4 + volua(1, 2);

            let damn = B { value: A { value: 50 } };
            let volua2 = |a| { a.value.value };

            damn = B { value: A { value: 50uint_size } };
            damn.val;
            damn();
            volua2(damn);
            volua2();
            volua2(hello);

            let mm = C { hi: damn.value.hello() };

            mm = mm;
            mm
        }";
        let parsed = mollie_parser::BlockExpr::parse_value(source).unwrap();

        // {
        //     let mut hello = 12;
        //     let volua = |a, b| { a + b };

        //     hello = 54i64;

        //     hello + 4 + volua(1, 2)
        // }

        let mut ast = TypedAST::default();
        let mut storage = TypeContext::new();
        let mut solver = storage.solver();
        let isize = solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::ISize)), None);
        let input_func = solver.add_info(TypeInfo::Func(Box::new([isize]), isize), None);
        let func = solver.add_info(TypeInfo::Func(Box::new([input_func]), isize), None);

        solver.push_frame();
        solver.set_var("calc_smth", func);

        let result = Block::from_parsed_expr(parsed.value, &mut ast, &mut solver, parsed.span);

        let (solved, block) = ast.solve(result, &mut solver);

        println!("Solved Typed AST dump (fmt):\n{}", TypeBlockFmt {
            ast: &solved,
            storage: &storage,
            block
        });

        for item in solved.used_items {
            match item {
                UsedItem::VTable(ty, vtable, type_args) => println!(
                    "used vtable item: ({})<{}> for {}",
                    storage.vtables[vtable]
                        .origin_trait
                        .map(|origin_trait| storage.traits[origin_trait].name.clone())
                        .unwrap_or_else(|| storage.display_of(ty).to_string()),
                    type_args.into_iter().map(|ty| storage.display_of(ty)).join(", "),
                    storage.display_of(ty)
                ),
                UsedItem::Adt(adt_ref, type_args) => println!(
                    "used adt item: {}<{}>",
                    storage.adt_types[adt_ref].name.as_deref().unwrap_or_default(),
                    type_args.into_iter().map(|ty| storage.display_of(ty)).join(", ")
                ),
                UsedItem::Func(func) => println!("used func item: {}", storage.functions[func].name),
            }
        }

        for error in std::mem::take(&mut storage.errors).into_values() {
            let mut report = ariadne::Report::build(ariadne::ReportKind::Error, ("ui.mol", error.span.start..error.span.end))
                .with_config(ariadne::Config::new().with_compact(true));
            let label = ariadne::Label::new(("ui.mol", error.span.start..error.span.end)).with_color(ariadne::Color::Cyan);

            match error.value {
                TypeError::Unexpected { expected, found } => {
                    let expected = match &expected {
                        TypeErrorValue::Nothing => None,
                        value => Some(fmt::from_fn(|f| match value {
                            TypeErrorValue::Type => f.write_str("`type`"),
                            TypeErrorValue::Adt(adt_kind) => f.write_str(match adt_kind {
                                SpecialAdtKind::Specific(AdtKind::Struct) => "`struct`",
                                SpecialAdtKind::Specific(AdtKind::Component) => "`component`",
                                SpecialAdtKind::Specific(AdtKind::Enum) => "`enum`",
                                SpecialAdtKind::WithExpectation(AdtKind::Struct) => "`enum` or `component`",
                                SpecialAdtKind::WithExpectation(AdtKind::Component) => "`struct` or `enum`",
                                SpecialAdtKind::WithExpectation(AdtKind::Enum) => "`struct` or `component`",
                                SpecialAdtKind::AnyOf => "`struct`, `component` or `enum`",
                            }),
                            TypeErrorValue::Trait => f.write_str("`trait`"),
                            TypeErrorValue::Value => f.write_str("`value`"),
                            TypeErrorValue::Function => f.write_str("`function`"),
                            TypeErrorValue::Array(_) => f.write_str("`array`"),
                            TypeErrorValue::Module => f.write_str("`module`"),
                            TypeErrorValue::Generic => f.write_str("`generic`"),
                            TypeErrorValue::PrimitiveType(ty) => write!(f, "`{ty}`"),
                            TypeErrorValue::Nothing => unreachable!(),
                            &TypeErrorValue::ExplicitType(type_ref) => write!(f, "`{}`", storage.display_of(type_ref)),
                        })),
                    };

                    let found = match &found {
                        TypeErrorValue::Nothing => None,
                        value => Some(fmt::from_fn(|f| match value {
                            TypeErrorValue::Type => f.write_str("`type`"),
                            TypeErrorValue::Adt(adt_kind) => f.write_str(match adt_kind {
                                SpecialAdtKind::Specific(AdtKind::Struct) => "`struct`",
                                SpecialAdtKind::Specific(AdtKind::Component) => "`component`",
                                SpecialAdtKind::Specific(AdtKind::Enum) => "`enum`",
                                SpecialAdtKind::WithExpectation(AdtKind::Struct) => "`enum` or `component`",
                                SpecialAdtKind::WithExpectation(AdtKind::Component) => "`struct` or `enum`",
                                SpecialAdtKind::WithExpectation(AdtKind::Enum) => "`struct` or `component`",
                                SpecialAdtKind::AnyOf => "`struct`, `component` or `enum`",
                            }),
                            TypeErrorValue::Trait => f.write_str("`trait`"),
                            TypeErrorValue::Value => f.write_str("`value`"),
                            TypeErrorValue::Function => f.write_str("`function`"),
                            TypeErrorValue::Array(_) => f.write_str("`array`"),
                            TypeErrorValue::Module => f.write_str("`module`"),
                            TypeErrorValue::Generic => f.write_str("`generic`"),
                            TypeErrorValue::PrimitiveType(ty) => write!(f, "`{ty}`"),
                            TypeErrorValue::Nothing => unreachable!(),
                            &TypeErrorValue::ExplicitType(type_ref) => write!(f, "`{}`", storage.display_of(type_ref)),
                        })),
                    };

                    if let Some(expected) = expected {
                        report.set_message(format!("expected {expected}"));
                    }

                    report.add_label(if let Some(found) = found {
                        label.with_message(format!("found {found}"))
                    } else {
                        label
                    });
                }
                TypeError::NoField { adt, variant, name } => {
                    report.set_message("no field");
                    report.add_label(label.with_message(format!(
                        "`{}{}` doesn't have field called `{name}`",
                        storage.adt_types[adt].name.as_deref().unwrap_or_default(),
                        if matches!(storage.adt_types[adt].kind, AdtKind::Enum) {
                            format!("::{}", storage.adt_types[adt].variants[variant].name.as_deref().unwrap_or_default())
                        } else {
                            String::new()
                        }
                    )));
                }
                TypeError::VariantRequired(adt) => {
                    report.set_message("can't construct");
                    report.add_label(label.with_message(format!(
                        "`{}` requires to specify variant explicitly",
                        storage.adt_types[adt].name.as_deref().unwrap_or_default(),
                    )));
                }
                TypeError::NonIndexable { ty, name } => {
                    report.set_message("non-indexable value");
                    report.add_label(label.with_message(format!("`{}` can't have fields and be indexed by `{name}`", storage.display_of(ty))));
                }
                // TypeError::InvalidPostfixFunction { reasons } => {
                //     report.set_message("invalid postfix function definition");

                //     for reason in reasons {
                //         let label = Label::new(("ui.mol", reason.span.start..reason.span.end)).with_color(ariadne::Color::Cyan);

                //         report.add_label(match reason.value {
                //             mollie_typed_ast::PostfixRequirement::NoGenerics => label.with_message("generics are not allowed in postfix context"),
                //             mollie_typed_ast::PostfixRequirement::OneArgument => label.with_message("one argument is expected"),
                //             mollie_typed_ast::PostfixRequirement::OnlyOneArgument => label.with_message("multiple arguments are not allowed"),
                //             mollie_typed_ast::PostfixRequirement::ArgumentType => {
                //                 label.with_message("argument type must be either an (unsigned) integer or a float")
                //             }
                //         });
                //     }
                // }
                TypeError::NotFound { name, was_looking_for } => {
                    let message = match was_looking_for {
                        LookupType::Variable => format!("there's no variable called `{name}`"),
                        LookupType::Type { inside } => {
                            if inside == ModuleId::ZERO {
                                format!("there's no type called `{name}`")
                            } else {
                                format!("there's no type called `{name}` in `{}`", storage.modules[inside].name)
                            }
                        }
                        LookupType::Module { inside } => {
                            if inside == ModuleId::ZERO {
                                format!("there's no module called `{name}`")
                            } else {
                                format!("there's no module called `{name}` in `{}`", storage.modules[inside].name)
                            }
                        }
                    };

                    report.set_message(message);
                    report.add_label(label.with_message("tried to access here"));
                }
                TypeError::NoFunction { name, postfix } => {
                    if postfix {
                        report.set_message(format!("there's no postfix function called `{name}`"));
                        report.add_label(label.with_message("tried to use here"));
                    } else {
                        report.set_message(format!("there's no function called `{name}`"));
                        report.add_label(label.with_message("tried to call here"));
                    }
                }
                TypeError::NotPostfix { name } => {
                    report.set_message(format!("`{name}` can't be used in postfix context"));
                    report.add_label(label.with_message("tried to use here"));
                }
                TypeError::NonConstantEvaluable => {
                    report.set_message("expression can't be evaluated at compile-time");
                    report.add_label(label.with_message("this expression"));
                }
                TypeError::ArgumentCountMismatch { expected, found } => {
                    report.set_message(if found > expected {
                        "received more arguments than was expected"
                    } else {
                        "received less arguments than was expected"
                    });

                    report.add_label(label.with_message(format!("expected {expected} arguments here, found {found}")));
                }
                TypeError::Parse(parse_error) => {
                    report.set_message(parse_error);
                }
            }

            report.finish().print(("ui.mol", ariadne::Source::from(source))).unwrap();
        }
    }
}
