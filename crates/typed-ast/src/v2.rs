use std::{collections::HashMap, fmt::Debug, mem::MaybeUninit, ops::Index};

use itertools::Itertools;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IdxEnumerate, IndexBoxedSlice, IndexVec};
use mollie_parser::{LangItem, NodeExpr};
use mollie_shared::{MaybePositioned, Operator, Positioned, Span};
use mollie_typing::{AdtKind, AdtRef, AdtVariantRef, FieldRef, IntType, PrimitiveType, TraitRef, UIntType, VFuncRef, VTableRef};

use crate::{Block, BlockRef, ExprRef, Func, FuncRef, IntrinsicKind, LangItemValue, Module, ModuleId, ModuleItem, StmtRef, TraitFuncRef};

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

pub trait Descriptor {
    type Type;
    type IndexResult;
}

pub struct Typed<T, D: Descriptor> {
    pub value: T,
    pub span: Span,
    pub ty: D::Type,
}

impl<D: Descriptor, T: Debug> Debug for Typed<T, D>
where
    D::Type: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    // pub used_items: Vec<UsedItem>,
}

impl<D: Descriptor> Default for TypedAST<D> {
    fn default() -> Self {
        Self {
            module: ModuleId::ZERO,
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
        }
    }
}

impl<D: Descriptor> Debug for TypedAST<D>
where
    D::Type: Debug,
    D::IndexResult: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

#[derive(Debug)]
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
    D(D::Type),
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
            Self::IfElse { condition, block, otherwise } => Self::IfElse {
                condition: condition.clone(),
                block: block.clone(),
                otherwise: otherwise.clone(),
            },
            Self::While { condition, block } => Self::While {
                condition: condition.clone(),
                block: block.clone(),
            },
            Self::Block(arg0) => Self::Block(arg0.clone()),
            Self::Binary { operator, lhs, rhs } => Self::Binary {
                operator: operator.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            },
            Self::Closure { args, body } => Self::Closure {
                args: args.clone(),
                body: body.clone(),
            },
            Self::Call { func, args } => Self::Call {
                func: func.clone(),
                args: args.clone(),
            },
            Self::Construct { adt, variant, fields } => Self::Construct {
                adt: adt.clone(),
                variant: variant.clone(),
                fields: fields.clone(),
            },
            Self::AdtIndex { target, field } => Self::AdtIndex {
                target: target.clone(),
                field: field.clone(),
            },
            Self::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => Self::VTableIndex {
                target: target.clone(),
                target_ty: target_ty.clone(),
                vtable: vtable.clone(),
                func: func.clone(),
            },
            Self::ArrayIndex { target, element } => Self::ArrayIndex {
                target: target.clone(),
                element: element.clone(),
            },
            Self::D(arg0) => Self::D(arg0.clone()),
        }
    }
}

impl<D: Descriptor> Debug for Expr<D>
where
    D::Type: Debug,
    D::IndexResult: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            Self::ArrayIndex { target, element } => f.debug_struct("ArrayIndex").field("target", target).field("element", element).finish(),
            Self::D(arg0) => f.debug_tuple("D").field(arg0).finish(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VTableFuncKind<S = ()> {
    Local(BlockRef),
    External(&'static str),
    Special(S),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Default)]
struct TypeContext {
    types: IndexVec<TypeRef, Type>,

    language_items: HashMap<LangItem, LangItemValue>,

    modules: IndexVec<ModuleId, Module>,
    adt_types: IndexVec<AdtRef, Adt>,
    local_functions: IndexVec<FuncRef, Func>,
    traits: IndexVec<TraitRef, Trait>,
    vtables: IndexVec<VTableRef, VTableGenerator>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            types: IndexVec::new(),
            language_items: HashMap::new(),
            modules: IndexVec::from_iter([Module::new(ModuleId::ZERO, "<anonymous>")]),
            adt_types: IndexVec::new(),
            local_functions: IndexVec::new(),
            traits: IndexVec::new(),
            vtables: IndexVec::new(),
        }
    }

    fn is_likely_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        println!("check for ~equality: {:?} & {:?}", self.types[ty], self.types[other]);

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

    pub fn find_vtable(&self, ty: TypeRef, origin_trait: Option<TraitRef>) -> Option<VTableRef> {
        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.is_likely_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
    }

    pub fn find_vtable_by_func<T: AsRef<str>>(&self, ty: TypeRef, name: T) -> Option<(VTableRef, VFuncRef)> {
        let name = name.as_ref();

        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.is_likely_same(ty, vtable.ty)
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
            if self.is_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
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
        }
    }

    fn solver(&mut self) -> TypeSolver<'_> {
        TypeSolver {
            storage: self,
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
}

mollie_index::new_idx_type!(TypeInfoRef);
mollie_index::new_idx_type!(TypeRef);

#[derive(Debug)]
struct TypeSolver<'a> {
    storage: &'a mut TypeContext,
    available_generics: HashMap<String, (TypeInfoRef, TypeRef)>,
    type_infos: IndexVec<TypeInfoRef, MaybePositioned<TypeInfo>>,
    frames: Vec<TypeFrame>,
}

#[derive(Debug, Default)]
struct TypeFrame(HashMap<String, TypeInfoRef>);

impl TypeSolver<'_> {
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

        println!("unify [start]: {:?} ({a:?}) & {:?} ({b:?})", self.type_infos[a].value, self.type_infos[b].value);

        match (self.type_infos[a].value.clone(), self.type_infos[b].value.clone()) {
            (TypeInfo::Unknown(None), _) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Unknown(None)) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Unknown(Some(_)), TypeInfo::Unknown(Some(_))) => self.type_infos[a].value = TypeInfo::Ref(b),
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
                if a_adt != b_adt {
                    panic!("Type mismatch between {a:?} and {b:?}")
                }

                for (a, b) in a_type_args.into_iter().zip(b_type_args.into_iter()) {
                    self.unify(a, b);
                }
            }
            (TypeInfo::Trait(trait_ref, t_args), b_info) => {
                let b = self.solve(b);

                if let Some(vtable) = self.storage.find_vtable(b, Some(trait_ref)) {
                    let type_args = if let TypeInfo::Adt(_, type_args) = b_info {
                        type_args
                    } else {
                        Box::default()
                    };

                    let generics: Box<[_]> = self.storage.vtables[vtable]
                        .generics
                        .iter()
                        .map(|g| Self::type_to_info(&mut self.type_infos, &self.storage, *g, &type_args))
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
                if a != b {
                    panic!("Type mismatch between {a:?} and {b:?}")
                }
            }
            (a, b) => panic!("Type mismatch between {a:?} and {b:?}"),
        }

        println!("unify [end]: {:?} ({a:?}) & {:?} ({b:?})", self.type_infos[a].value, self.type_infos[b].value);
    }

    fn solve(&mut self, info: TypeInfoRef) -> TypeRef {
        println!("Solving {info:?} => {:?}", self.type_infos[info].value);

        match &self.type_infos[info].value {
            &TypeInfo::Primitive(primitive_type) => self.storage.get_or_add(Type::Primitive(primitive_type)),
            &TypeInfo::Array(element, size) => {
                let element = self.solve(element);

                self.storage.get_or_add(Type::Array(element, size))
            }
            TypeInfo::Func(args, returns) => {
                let args = args.clone();
                let returns = self.solve(*returns);
                let args = args.into_iter().map(|arg| self.solve(arg)).collect();

                self.storage.get_or_add(Type::Func(args, returns))
            }
            &TypeInfo::Unknown(Some(info)) => self.solve(info),
            &TypeInfo::Unknown(None) => panic!("can't infer type"),
            &TypeInfo::Ref(info) => self.solve(info),
            TypeInfo::Adt(adt_ref, type_args) => {
                let adt_ref = *adt_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.storage.get_or_add(Type::Adt(adt_ref, type_args))
            }
            TypeInfo::Trait(trait_ref, type_args) => {
                let trait_ref = *trait_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.storage.get_or_add(Type::Trait(trait_ref, type_args))
            }
            TypeInfo::Generic(_) => todo!(),
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
            Type::Generic(i) => match type_args.get(i) {
                Some(ty) => *ty,
                None => infos.insert(MaybePositioned::new(TypeInfo::Generic(i), None)),
            },
        }
    }

    fn instantiate_adt(&mut self, adt: AdtRef, variant: AdtVariantRef, type_args: &[TypeInfoRef]) -> impl Iterator<Item = (FieldRef, TypeInfoRef)> {
        self.storage.adt_types[adt].variants[variant]
            .fields
            .iter()
            .map(|(field_ref, field)| (field_ref, Self::type_to_info(&mut self.type_infos, &self.storage, field.ty, type_args)))
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
                (mollie_parser::Number::F32(value), Some(_)) => ast.add_expr(
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
                let func = Expr::from_parsed_expr(func_call_expr.function.value, ast, solver, func_call_expr.function.span);
                let ty = if let TypeInfo::Func(_, returns) = solver.type_infos[ast[func].ty].value {
                    returns
                } else {
                    solver.add_unknown(None, Some(func_span))
                };

                let args: Box<[_]> = func_call_expr
                    .args
                    .value
                    .into_iter()
                    .map(|arg| Expr::from_parsed_expr(arg.value, ast, solver, arg.span))
                    .collect();

                let func_ty = solver.add_info(TypeInfo::Func(args.iter().map(|&arg| ast[arg].ty).collect(), ty), Some(span));

                solver.unify(ast[func].ty, func_ty);

                ast.add_expr(Expr::Call { func, args }, ty, span)
            }
            mollie_parser::Expr::Node(mut node_expr) => {
                let ty = TypePathResult::from_parsed_type_path(node_expr.name.value, solver, node_expr.name.span);

                if let TypePathResult::Adt(adt, type_args, variant) = ty {
                    let variant = variant.unwrap_or(AdtVariantRef::ZERO);
                    let mut fields = solver
                        .instantiate_adt(adt, variant, &type_args)
                        .map(|(field_ref, field_type)| (field_ref, field_type, ExprRef::INVALID))
                        .collect::<IndexBoxedSlice<FieldRef, _>>();

                    for prop in node_expr.properties {
                        let name = prop.value.name.value.0;

                        let field = solver.storage.adt_types[adt].variants[variant]
                            .fields
                            .iter()
                            .find_map(|(field_ref, field)| if field.name == name { Some(field_ref) } else { None });

                        if let Some(field) = field {
                            let value = match prop.value.value {
                                Some(value) => Self::from_parsed_expr(value.value, ast, solver, value.span),
                                None => {
                                    if let Some(ty) = solver.get_var(&name) {
                                        ast.add_expr(Expr::Var(name), ty, prop.value.name.span)
                                    } else {
                                        panic!("uh")
                                    }
                                }
                            };

                            solver.unify(fields[field].1, ast[value].ty);

                            fields[field].2 = value;
                        }
                    }

                    if !node_expr.children.value.is_empty() {
                        let field = solver.storage.adt_types[adt].variants[variant]
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

                                ast.add_expr(Expr::Array { element, elements }, ty, node_expr.children.span)
                            };

                            solver.unify(fields[field].1, ast[value].ty);

                            fields[field].2 = value;
                        }
                    }

                    let fields = fields.raw;
                    let ty = solver.add_info(TypeInfo::Adt(adt, type_args), None);

                    ast.add_expr(Expr::Construct { adt, variant, fields }, ty, span)
                } else {
                    panic!("expected adt")
                }
            }
            mollie_parser::Expr::Index(index_expr) => {
                let target = Self::from_parsed_expr(index_expr.target.value, ast, solver, index_expr.target.span);

                match index_expr.index.value {
                    mollie_parser::IndexTarget::Named(ident) => ast.add_expr(
                        Expr::AdtIndex { target, field: ident.0 },
                        solver.add_unknown(None, Some(index_expr.index.span)),
                        span,
                    ),
                    mollie_parser::IndexTarget::Expression(expr) => {
                        let element = Self::from_parsed_expr(*expr, ast, solver, index_expr.index.span);
                        let usize = solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize)), Some(index_expr.index.span));

                        solver.unify(ast[element].ty, usize);

                        ast.add_expr(
                            Expr::ArrayIndex { target, element },
                            solver.add_unknown(None, Some(index_expr.index.span)),
                            span,
                        )
                    }
                }
            }
            mollie_parser::Expr::Binary(binary_expr) => {
                let lhs = Expr::from_parsed_expr(binary_expr.lhs.value, ast, solver, binary_expr.lhs.span);
                let rhs = Expr::from_parsed_expr(binary_expr.rhs.value, ast, solver, binary_expr.rhs.span);

                solver.unify(ast[rhs].ty, ast[lhs].ty);

                let ty = match binary_expr.operator.value {
                    Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan => {
                        solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), Some(binary_expr.operator.span))
                    }
                    _ => ast[lhs].ty,
                };

                ast.add_expr(
                    Expr::Binary {
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
                        let ty = solver.storage.vtables[vtable].functions[func].ty;
                        let ty = TypeSolver::type_to_info(&mut solver.type_infos, &solver.storage, ty, &[]);

                        ast.add_expr(
                            Expr::VTableIndex {
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
                        let variant = variant.unwrap_or(AdtVariantRef::ZERO);

                        ast.add_expr(
                            Expr::Construct {
                                adt,
                                variant,
                                fields: Box::new([]),
                            },
                            ty,
                            span,
                        )
                    }
                    TypePathResult::Trait(trait_ref, type_info_refs) => todo!(),
                    TypePathResult::Func(func_ref) => todo!(),
                    TypePathResult::Intrinsic(intrinsic_kind) => todo!(),
                    TypePathResult::Generic(type_info_ref) => todo!(),
                    TypePathResult::Module(module_id) => todo!(),
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

                ast.add_expr(Expr::Array { element, elements }, array_ty, span)
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

                ast.add_expr(Expr::IfElse { condition, block, otherwise }, ty, span)
            }
            mollie_parser::Expr::While(while_expr) => {
                let condition = Self::from_parsed_expr(while_expr.condition.value, ast, solver, while_expr.condition.span);
                let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean), None);

                solver.unify(ast[condition].ty, expected);

                let block = Block::from_parsed_expr(while_expr.block.value, ast, solver, while_expr.block.span);
                let ty = ast[block].ty;
                let expected = solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None);

                solver.unify(ty, expected);

                ast.add_expr(Expr::While { condition, block }, ty, span)
            }
            mollie_parser::Expr::Block(block_expr) => {
                let block = Block::from_parsed_expr(block_expr, ast, solver, span);
                let ty = ast[block].ty;

                ast.add_expr(Expr::Block(block), ty, span)
            }
            mollie_parser::Expr::ForIn(for_in_expr) => todo!(),
            mollie_parser::Expr::Is(is_expr) => todo!(),
            mollie_parser::Expr::Cast(positioned, positioned1) => todo!(),
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

                ast.add_expr(Expr::Closure { args, body }, ty, span)
            }
            mollie_parser::Expr::Ident(ident) => {
                if let Some(ty) = solver.get_var(&ident.0) {
                    ast.add_expr(Expr::Var(ident.0), ty, span)
                } else {
                    println!("Frames dump: {:#?}", solver.frames);

                    unimplemented!("whoops => {}", ident.0)
                }
            }
            mollie_parser::Expr::This => {
                if let Some(ty) = solver.get_var("self") {
                    ast.add_expr(Expr::Var("self".to_string()), ty, span)
                } else {
                    println!("Frames dump: {:#?}", solver.frames);

                    unimplemented!("whoops => self")
                }
            }
            mollie_parser::Expr::Nothing => todo!(),
        };

        println!("From parsed expr => {:?}", ast[expr].value);

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

                Some(ast.add_stmt(Stmt::Expr(expr)))
            }
            mollie_parser::Stmt::VariableDecl(variable_decl) => {
                let value = Expr::from_parsed_expr(variable_decl.value.value, ast, solver, variable_decl.value.span);

                solver.set_var(&variable_decl.name.value.0, ast[value].ty);

                Some(ast.add_stmt(Stmt::NewVar {
                    mutable: variable_decl.mutable.is_some(),
                    name: variable_decl.name.value.0,
                    value,
                }))
            }
            mollie_parser::Stmt::StructDecl(struct_decl) => {
                for (index, name) in struct_decl.name.value.generics.iter().enumerate() {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.storage.get_or_add(Type::Generic(index));

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

                let adt_ref = AdtRef::new(solver.storage.adt_types.len());

                solver.storage.modules[ast.module]
                    .items
                    .insert(struct_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.storage.adt_types.push(Adt {
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
                    let ty = solver.storage.get_or_add(Type::Generic(index));

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

                let adt_ref = AdtRef::new(solver.storage.adt_types.len());

                solver.storage.modules[ast.module]
                    .items
                    .insert(component_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.storage.adt_types.push(Adt {
                    name: Some(component_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Component,
                    generics: component_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::TraitDecl(trait_decl) => {
                let trait_ref = solver.storage.traits.next_index();

                let this = {
                    let ty_info = solver.add_info(TypeInfo::Generic(0), None);
                    let ty = solver.storage.get_or_add(Type::Generic(0));

                    solver.available_generics.insert(String::from("<Self>"), (ty_info, ty));

                    (ty_info, ty)
                };

                for (index, name) in trait_decl.name.value.generics.iter().enumerate() {
                    let index = index + 1;
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.storage.get_or_add(Type::Generic(index));

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
                        solver.storage.language_items.insert(item, LangItemValue::TraitFunc(trait_ref, func_ref));
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
                        None => solver.storage.get_or_add(Type::Primitive(PrimitiveType::Void)),
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
                    solver.storage.language_items.insert(item, LangItemValue::Trait(trait_ref));
                }

                solver.storage.traits.push(Trait {
                    name: trait_decl.name.value.name.value.0.clone(),
                    generics: trait_decl.name.value.generics.len(),
                    functions,
                });

                solver.storage.modules[ast.module]
                    .items
                    .insert(trait_decl.name.value.name.value.0, ModuleItem::Trait(trait_ref));

                None
            }
            mollie_parser::Stmt::EnumDecl(enum_decl) => {
                let adt_ref = AdtRef::new(solver.storage.adt_types.len());

                if let Some(item) = enum_decl.attributes.iter().find_map(|attribute| {
                    attribute.value.value.as_ref().and_then(|v| {
                        if let mollie_parser::AttributeValue::LangItem(item) = v.value {
                            Some(item)
                        } else {
                            None
                        }
                    })
                }) {
                    solver.storage.language_items.insert(item, LangItemValue::Adt(adt_ref));
                }

                for (index, name) in enum_decl.name.value.generics.iter().enumerate() {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), Some(name.span));
                    let ty = solver.storage.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name.value.0.clone(), (ty_info, ty));
                }

                let mut variants = IndexVec::new();
                let usize = solver.storage.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize)));

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
                            .storage
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

                solver.storage.modules[ast.module]
                    .items
                    .insert(enum_decl.name.value.name.value.0.clone(), ModuleItem::Adt(adt_ref));

                solver.storage.adt_types.push(Adt {
                    name: Some(enum_decl.name.value.name.value.0),
                    collectable: true,
                    kind: AdtKind::Enum,
                    generics: enum_decl.name.value.generics.len(),
                    variants: variants.into_boxed_slice(),
                });

                None
            }
            mollie_parser::Stmt::FuncDecl(func_decl) => None,
            mollie_parser::Stmt::Impl(mut implementation) => {
                let generics = std::iter::once_with(|| {
                    if implementation.trait_name.is_some() {
                        Some((String::from("<Self>"), None))
                    } else {
                        None
                    }
                })
                .flatten()
                .chain(implementation.generics.iter().map(|name| (name.value.0.clone(), Some(name.span))))
                .enumerate()
                .map(|(index, (name, span))| {
                    let ty_info = solver.add_info(TypeInfo::Generic(index), span);
                    let ty = solver.storage.get_or_add(Type::Generic(index));

                    solver.available_generics.insert(name, (ty_info, ty));

                    ty
                })
                .collect::<Box<[_]>>();

                // let mut applied_generics: Box<[_]> = Box::default();

                let (trait_name, origin_trait) = match implementation.trait_name {
                    Some(trait_name) => {
                        if let ModuleItem::Trait(trait_ref) = ModuleItem::from_parsed_type_path(trait_name.value, solver, trait_name.span) {
                            // applied_generics = generics.iter().map(|&(_, generic)| generic).collect();

                            // if let TypeInfo::Trait(_, args) = checker.solver.get_info(ty) {
                            //     applied_generics.clone_from(args);
                            // }

                            (Some(solver.storage.traits[trait_ref].name.clone()), Some(trait_ref))
                        } else {
                            (None, None)
                        }
                    }
                    None => (None, None),
                };

                let ty = Type::from_parsed_type(implementation.target.value, solver, implementation.target.span);
                let ty_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ty, &[]);

                let vtable = match solver.storage.get_vtable(ty, origin_trait) {
                    Some(vtable) => vtable,
                    None => solver.storage.vtables.insert(VTableGenerator {
                        ty,
                        generics,
                        origin_trait,
                        functions: IndexVec::new(),
                    }),
                };

                if let (Some(trait_name), Some(trait_ref)) = (&trait_name, origin_trait) {
                    let trait_functions = solver.storage.traits[trait_ref]
                        .functions
                        .iter()
                        .map(|(k, func)| (k, func.name.clone()))
                        .collect::<Box<[_]>>();

                    for (func_ref, name) in trait_functions {
                        if let Some(func) = implementation.functions.value.iter().position(|func| func.value.name.value.0 == name) {
                            let function = implementation.functions.value.remove(func);

                            solver.push_frame();

                            let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                            let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                            if function.value.this.is_some() {
                                solver.set_var("self", ty_info);

                                arg_names.push("self".to_string());
                                args.push(ty);
                            }

                            for arg in function.value.args {
                                let ty = Type::from_parsed_type(arg.value.ty.value, solver, arg.value.ty.span);
                                let ty_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ty, &[]);

                                solver.set_var(&arg.value.name.value.0, ty_info);

                                // if let TypeInfo::Adt(adt_ref, _, adt_args) = checker.solver.get_info(ty) {
                                //     ast.use_item(UsedItem::Adt(*adt_ref, adt_args.clone()));
                                // }

                                arg_names.push(arg.value.name.value.0);
                                args.push(ty);
                            }

                            let returns = if let Some(returns) = function.value.returns {
                                Type::from_parsed_type(returns.value, solver, returns.span)
                            } else {
                                solver.storage.get_or_add(Type::Primitive(PrimitiveType::Void))
                            };

                            let returns_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, returns, &[]);
                            let body = Block::from_parsed_expr(function.value.body.value, ast, solver, function.value.body.span);

                            solver.unify(ast[body].ty, returns_info);
                            solver.pop_frame();

                            let ty = solver.storage.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                            solver.storage.vtables[vtable].functions.push(VTableFunc {
                                trait_func: Some(func_ref),
                                name: function.value.name.value.0,
                                arg_names,
                                ty,
                                kind: VTableFuncKind::Local(body),
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

                    solver.push_frame();

                    let mut arg_names = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));
                    let mut args = Vec::with_capacity(function.value.args.capacity() + usize::from(function.value.this.is_some()));

                    if function.value.this.is_some() {
                        solver.set_var("self", ty_info);

                        arg_names.push("self".to_string());
                        args.push(ty);
                    }

                    for arg in function.value.args {
                        let ty = Type::from_parsed_type(arg.value.ty.value, solver, arg.value.ty.span);
                        let ty_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ty, &[]);

                        solver.set_var(&arg.value.name.value.0, ty_info);

                        // if let TypeInfo::Adt(adt_ref, _, adt_args) = checker.solver.get_info(ty) {
                        //     ast.use_item(UsedItem::Adt(*adt_ref, adt_args.clone()));
                        // }

                        arg_names.push(arg.value.name.value.0);
                        args.push(ty);
                    }

                    let returns = if let Some(returns) = function.value.returns {
                        Type::from_parsed_type(returns.value, solver, returns.span)
                    } else {
                        solver.storage.get_or_add(Type::Primitive(PrimitiveType::Void))
                    };

                    let returns_info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, returns, &[]);
                    let body = Block::from_parsed_expr(function.value.body.value, ast, solver, function.value.body.span);

                    solver.unify(ast[body].ty, returns_info);
                    solver.pop_frame();

                    let ty = solver.storage.get_or_add(Type::Func(args.into_boxed_slice(), returns));

                    solver.storage.vtables[vtable].functions.push(VTableFunc {
                        trait_func: None,
                        name: function.value.name.value.0,
                        arg_names,
                        ty,
                        kind: VTableFuncKind::Local(body),
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
                let path_span = import.path.span;
                let path = ModuleItem::from_parsed_type_path(import.path.value, solver, import.path.span);

                match path {
                    ModuleItem::SubModule(module) => match import.kind {
                        mollie_parser::ImportKind::Partial(items) => {
                            for item in items.value {
                                let name = item.value.0;

                                if let Some(item) = solver.storage.modules[module].items.get(&name).copied() {
                                    solver.storage.modules[ModuleId::ZERO].items.insert(name, item);
                                }
                            }
                        }
                        mollie_parser::ImportKind::Named => {
                            let name = solver.storage.modules[module].name.clone();

                            solver.storage.modules[ModuleId::ZERO].items.insert(name, ModuleItem::SubModule(module));
                        }
                    },
                    // TypePath::Adt(..) => {
                    //     checker.add_error(TypeError::ExpectedModule { found: NotModule::Adt }, path_span);
                    // }
                    // TypePath::Trait(..) => {
                    //     checker.add_error(TypeError::ExpectedModule { found: NotModule::Trait }, path_span);
                    // }
                    // TypePath::Func(_) => {
                    //     checker.add_error(TypeError::ExpectedModule { found: NotModule::Function }, path_span);
                    // }
                    // TypePath::Generic(..) => {
                    //     checker.add_error(TypeError::ExpectedModule { found: NotModule::Generic }, path_span);
                    // }
                    // TypePath::Intrinsic(..) => {
                    //     checker.add_error(TypeError::ExpectedModule { found: NotModule::Function }, path_span);
                    // }
                    _ => (),
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
}

impl ModuleItem {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> Self {
        let mut result = Self::SubModule(ModuleId::ZERO);

        for segment in path.segments {
            if let &Self::SubModule(current_module) = &result {
                if let Some(item) = solver.storage.modules[current_module].items.get(&segment.value.name.value.0) {
                    result = *item;
                } else {
                    panic!("mm x2")
                }
            } else {
                panic!("mm x2")
            }
        }

        result
    }
}

impl TypePathResult {
    fn from_parsed_type_path(path: mollie_parser::TypePathExpr, solver: &mut TypeSolver, span: Span) -> Self {
        let mut result = Self::Module(ModuleId::ZERO);

        for segment in path.segments {
            if let Some((typo, _)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = Self::Generic(typo);

                break;
            }

            match result {
                Self::Adt(adt, type_args, None) => {
                    if let Some(variant) = solver.storage.adt_types[adt].variants.iter().find_map(|(variant_ref, variant)| {
                        if variant.name.as_deref() == Some(segment.value.name.value.0.as_str()) {
                            Some(variant_ref)
                        } else {
                            None
                        }
                    }) {
                        result = Self::Adt(adt, type_args, Some(variant));
                    } else {
                        let storage_type_args = type_args.iter().map(|&type_arg| solver.solve(type_arg)).collect();
                        let storage_ty = solver.storage.get_or_add(Type::Adt(adt, storage_type_args));

                        if let Some((vtable, vfunc)) = solver.storage.find_vtable_by_func(storage_ty, &segment.value.name.value.0) {
                            result = Self::VFunc(adt, type_args, vtable, vfunc);
                        } else {
                            result = Self::Adt(adt, type_args, None);
                        }
                    }
                }
                Self::Module(current_module) => {
                    if let Some(item) = solver.storage.modules[current_module].items.get(&segment.value.name.value.0) {
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

                                let args = (0..solver.storage.adt_types[adt_ref].generics)
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

                                let args = (0..solver.storage.traits[trait_ref].generics)
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
                        panic!("mm x2")
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

        let mut result = TypePathResult::Module(ModuleId::ZERO);

        for segment in path.segments {
            if let Some((typo, _)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let TypePathResult::Module(current_module) = result {
                if let Some(item) = solver.storage.modules[current_module].items.get(&segment.value.name.value.0) {
                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| TypeInfo::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.storage.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(TypeInfo::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.add_info(TypeInfo::Adt(adt_ref, args), None));
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

                            let args = (0..solver.storage.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.add_info(TypeInfo::Generic(generic), None)))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.add_info(TypeInfo::Trait(trait_ref, args), None));
                        }
                        ModuleItem::Func(func_ref) => {
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

                            // result = Self::Func(func_ref);
                        }
                        ModuleItem::Intrinsic(kind, _) => {
                            // result = Self::Intrinsic(kind);
                        }
                    }
                } else {
                    panic!("mm x2")
                }
            }
        }

        match result {
            TypePathResult::Type(type_info_ref) => type_info_ref,
            TypePathResult::Module(module_id) => todo!(),
        }
    }

    fn from_parsed_type(ty: mollie_parser::Type, solver: &mut TypeSolver, span: Span) -> TypeInfoRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => solver.add_info(
                TypeInfo::Primitive(match primitive_type {
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

                solver.add_info(TypeInfo::Array(element, size.map(|size| size.value)), Some(span))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed_type(arg.value, solver, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed_type(ty.value, solver, ty.span),
                    None => solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), Some(span)),
                };

                solver.add_info(TypeInfo::Func(args, returns), Some(span))
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

        let mut result = TypePathResult::Module(ModuleId::ZERO);

        for segment in path.segments {
            if let Some((_, typo)) = solver.available_generics.get(&segment.value.name.value.0).copied() {
                result = TypePathResult::Type(typo);

                break;
            } else if let &TypePathResult::Module(current_module) = &result {
                if let Some(item) = solver.storage.modules[current_module].items.get(&segment.value.name.value.0) {
                    match *item {
                        ModuleItem::SubModule(module_id) => result = TypePathResult::Module(module_id),
                        ModuleItem::Adt(adt_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Type::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.storage.adt_types[adt_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.storage.get_or_add(Type::Generic(generic))))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.storage.get_or_add(Type::Adt(adt_ref, args)));
                        }
                        ModuleItem::Trait(trait_ref) => {
                            let mut type_args = segment.value.args.map_or_else(Vec::default, |type_args| {
                                type_args
                                    .value
                                    .0
                                    .into_iter()
                                    .map(|arg| Type::from_parsed_type(arg.value, solver, arg.span))
                                    .collect()
                            });

                            let args = (0..solver.storage.traits[trait_ref].generics)
                                .rev()
                                .map(|generic| type_args.pop().unwrap_or_else(|| solver.storage.get_or_add(Type::Generic(generic))))
                                .rev()
                                .collect();

                            result = TypePathResult::Type(solver.storage.get_or_add(Type::Trait(trait_ref, args)));
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

                            // result = TypePath::Func(func_ref);
                        }
                        ModuleItem::Intrinsic(kind, ty) => {
                            // result = TypePath::Intrinsic(kind, ty);
                        }
                    }
                } else {
                    panic!("mm x2")
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
            TypePathResult::Module(module_id) => panic!("whoops"),
        }
    }

    fn from_parsed_type(ty: mollie_parser::Type, solver: &mut TypeSolver, span: Span) -> TypeRef {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => solver.storage.get_or_add(Type::Primitive(match primitive_type {
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

                solver.storage.get_or_add(Type::Array(element, size.map(|size| size.value)))
            }
            mollie_parser::Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::from_parsed_type(arg.value, solver, arg.span)).collect();
                let returns = match returns {
                    Some(ty) => Self::from_parsed_type(ty.value, solver, ty.span),
                    None => solver.storage.get_or_add(Type::Primitive(PrimitiveType::Void)),
                };

                solver.storage.get_or_add(Type::Func(args, returns))
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

        ast.add_block(Block { stmts, expr }, ty, span)
    }
}

impl TypedAST<FirstPass> {
    fn solve_expr(&self, ast: &mut TypedAST<SolvedPass>, expr: ExprRef, solver: &mut TypeSolver) -> ExprRef {
        println!("Solving expr {expr:?} => {:?}", self[expr].value);

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
                        let expr = self.solve_expr(ast, field_value, solver);
                        let field_type = solver.get_info(field_type);

                        if let TypeInfo::Trait(trait_ref, args) = &solver.type_infos[field_type].value {
                            println!("{field_ref:?}: {:?}", solver.storage.types[ast[expr].ty]);
                        }

                        (field_ref, solver.solve(field_type), expr)
                    })
                    .collect();

                Expr::Construct { adt, variant, fields }
            }
            Expr::AdtIndex { target, field } => {
                let target = self.solve_expr(ast, target, solver);

                println!("Solving adt index expr ty: {:?}", self[expr].value);
                println!("{:#?}", solver.storage.vtables);

                if let Some((vtable, func)) = solver.storage.find_vtable_by_func(ast[target].ty, &field) {
                    let ty = solver.storage.vtables[vtable].functions[func].ty;
                    let type_args: Box<[_]> = solver.storage.vtables[vtable]
                        .generics
                        .iter()
                        .map(|&ty| TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ty, &[]))
                        .collect();

                    let solved_origin_ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ast[target].ty, &[]);

                    println!("input type args: {type_args:?}");

                    let origin_ty = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, solver.storage.vtables[vtable].ty, &type_args);

                    println!("unsolved origin impl type: {:?}", solver.type_infos[origin_ty]);

                    if solver.storage.vtables[vtable].origin_trait.is_some() {
                        solver.unify(type_args[0], origin_ty);
                    }

                    solver.unify(origin_ty, solved_origin_ty);

                    for &type_arg in &type_args {
                        println!("{type_arg:?} => {:?}", solver.type_infos[type_arg]);
                    }

                    let info = TypeSolver::type_to_info(&mut solver.type_infos, solver.storage, ty, &type_args);

                    solver.unify(self[expr].ty, info);

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

                let (field, ty) = if let Type::Adt(adt, args) = &solver.storage.types[ast[target].ty] {
                    let field = solver.storage.adt_types[*adt].variants[AdtVariantRef::ZERO]
                        .fields
                        .iter()
                        .find_map(|(field_ref, variant_field)| if variant_field.name == field { Some(field_ref) } else { None })
                        .unwrap();

                    let ty = solver.storage.adt_types[*adt][field].ty;

                    println!("{ty:?} => {:?} (apply {args:?})", solver.storage.types[ty]);

                    let args = args.clone();
                    let ty = solver.storage.apply_type_args(ty, &args);

                    (field, ty)
                } else {
                    todo!()
                };

                let info = TypeSolver::type_to_info(&mut solver.type_infos, &solver.storage, ty, &[]);

                solver.unify(self[expr].ty, info);

                println!("{:?}.{field:?} => {ty:?} => {:?}", self[target].value, solver.storage.types[ty]);

                return ast.add_expr(Expr::AdtIndex { target, field }, ty, self[expr].span);
            }
            Expr::ArrayIndex { target, element } => {
                let target = self.solve_expr(ast, target, solver);
                let element = self.solve_expr(ast, element, solver);

                println!("Solving array index expr ty: {:?}", self[expr].value);

                let ty = if let Type::Array(element, _) = solver.storage.types[ast[target].ty] {
                    element
                } else {
                    todo!()
                };

                let info = TypeSolver::type_to_info(&mut solver.type_infos, &solver.storage, ty, &[]);

                solver.unify(self[expr].ty, info);

                return ast.add_expr(Expr::ArrayIndex { target, element }, ty, self[expr].span);
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
                let args = args.into_iter().map(|arg| self.solve_expr(ast, arg, solver)).collect();

                Expr::Call { func, args }
            }
            Expr::Array { element, elements } => {
                let element = solver.solve(element);
                let elements = elements.into_iter().map(|element| self.solve_expr(ast, element, solver)).collect();

                Expr::Array { element, elements }
            }
            Expr::IfElse { condition, block, otherwise } => {
                let condition = self.solve_expr(ast, condition, solver);
                let block = self.solve_block(ast, block, solver);
                let otherwise = otherwise.map(|otherwise| self.solve_expr(ast, otherwise, solver));

                Expr::IfElse { condition, block, otherwise }
            }
            Expr::While { condition, block } => {
                let condition = self.solve_expr(ast, condition, solver);
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
            Expr::D(_) => todo!(),
        };

        println!("Solving expr ty: {:?}", self[expr].value);

        let ty = solver.solve(self[expr].ty);

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
    use mollie_typing::{AdtVariantRef, IntType, PrimitiveType};

    use super::Stmt;
    use crate::{
        Block, BlockRef, ExprRef,
        v2::{Expr, LitExpr, SolvedPass, Type, TypeContext, TypeInfo, TypeRef, TypedAST},
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
                Type::Generic(_) => todo!(),
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
                        | (Expr::D(_), Expr::D(_)) => (),
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
                        "{} {{ {} }}",
                        self.storage.adt_types[*adt].name.as_deref().unwrap_or_default(),
                        fields
                            .iter()
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
                        otherwise.map_or_else(String::new, |otherwise| format!("else {}", self.fork(otherwise)))
                    )
                }
                Expr::Call { func, args } => {
                    write!(f, "{}({})", self.fork(*func), args.iter().map(|&arg| self.fork(arg)).join(", "))
                }
                Expr::D(_) => todo!(),
            }
        }
    }

    #[test]
    fn test_literal() {
        let parsed = mollie_parser::BlockExpr::parse_value(
            "{
            let hello = 12;
            let hello2 = [1, 2, 4int16];
            let volua = |a, b| { a + b };

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
                    A { value: self.value }
                }
            }

            let a = if 1 == 4 {
                \"peak\"
            } else {
                \"kaep\"
            };

            hello = 54int64;

            calc_smth(|b| { b * 2 });

            hello = hello + 4 + volua(1, 2);

            let damn = B { value: A { value: 50 } };
            let volua2 = |a| { a.value.value };

            damn = B { value: A { value: 50uint_size } };

            volua2(damn);

            let mm = C { hi: damn.value.hello() };

            mm = mm;

            mm
        }",
        )
        .unwrap();

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

        println!("Storage dump: {storage:#?}");
        println!("Solved Typed AST dump: {solved:#?}");
        println!("Solved Typed AST dump (fmt):\n{}", TypeBlockFmt {
            ast: &solved,
            storage: &storage,
            block
        });
    }
}
