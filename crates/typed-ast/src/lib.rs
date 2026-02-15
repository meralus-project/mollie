mod constant;
mod expression;
mod statement;
pub mod visitor;

use std::{collections::HashMap, fmt, iter, mem, ops::Index};

use indexmap::IndexMap;
use itertools::Itertools;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec, new_idx_type};
use mollie_parser::LangItem;
use mollie_shared::{Positioned, Span};
use mollie_typing::{
    Adt, AdtRef, AdtVariantRef, CoreTypes, FieldType, FuncArg, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver, TypeStorage,
    TypeUnificationError, UIntType, VFuncRef, VTableRef, Variable,
};
use serde::Serialize;

pub use crate::{
    expression::{Block, Expr, FuncSource, IsPattern, LiteralExpr, TypePath, VFunc},
    statement::Stmt,
};

new_idx_type!(BlockRef);
new_idx_type!(StmtRef);
new_idx_type!(ExprRef);
new_idx_type!(FuncRef);
new_idx_type!(TraitFuncRef);
new_idx_type!(ModuleId);
new_idx_type!(TypeErrorRef);

impl TraitFuncRef {
    pub fn as_vfunc(&self) -> VFuncRef {
        VFuncRef::new(self.0)
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum NonConstructable {
    Trait,
    Function,
    Generic,
    Module,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum NotModule {
    Trait,
    Function,
    Generic,
    Adt,
}

#[derive(Debug, Clone, Serialize)]
pub enum NotFunction {
    Type(TypeInfoRef),
    Adt(AdtRef),
    Trait(TraitRef),
    Primitive(PrimitiveType),
}

#[derive(Debug, Clone, Serialize)]
pub enum TypeError {
    ExpectedFunction { found: NotFunction },
    ExpectedConstructable { found: NonConstructable },
    ExpectedArray { found: TypeInfoRef },
    ExpectedModule { found: NotModule },
    NoField { ty: TypeInfoRef, name: String },
    NonIndexable { ty: TypeInfoRef, name: String },
    NoFunction { name: String, postfix: bool },
    NoVariable { name: String },
    TypeNotFound { name: String, module: ModuleId },
    InvalidTypePathSegment { reason: InvalidTypePathSegmentReason, module: ModuleId },
    NotPostfix { name: String },
    ModuleIsNotValue,
    NonConstantEvaluable,
    InvalidPostfixFunction { reasons: Vec<Positioned<PostfixRequirement>> },
    Parse(mollie_parser::ParseError),
}

impl TypeError {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::ExpectedFunction { .. } => "expected-function",
            Self::ExpectedConstructable { .. } => "expected-constructable",
            Self::ExpectedArray { .. } => "expected-array",
            Self::ExpectedModule { .. } => "expected-module",
            Self::NoField { .. } => "no-field",
            Self::NonIndexable { .. } => "non-indexable",
            Self::NoFunction { .. } => "no-function",
            Self::NoVariable { .. } => "no-variable",
            Self::TypeNotFound { .. } => "type-not-found",
            Self::InvalidTypePathSegment { .. } => "invalid-type-path-segment",
            Self::NotPostfix { .. } => "not-postfix",
            Self::ModuleIsNotValue => "module-is-not-value",
            Self::NonConstantEvaluable => "non-constant-evaluable",
            Self::InvalidPostfixFunction { .. } => "invalid-postfix-definition",
            Self::Parse(..) => "parse",
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum PostfixRequirement {
    NoGenerics,
    OneArgument,
    OnlyOneArgument,
    ArgumentType,
}

#[derive(Debug, Clone, Serialize)]
pub enum InvalidTypePathSegmentReason {
    Variable(String),
    Primitive(PrimitiveType),
}

pub struct TypeErrors<'a>(pub &'a [Positioned<TypeError>]);

pub type TypeResult<'a, T> = Result<T, TypeErrors<'a>>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum UsedItem {
    VTable(TypeInfoRef, VTableRef, Box<[TypeInfoRef]>),
    Adt(AdtRef, Box<[TypeInfoRef]>),
    Func(FuncRef),
}

#[derive(Debug, Default, Serialize)]
pub struct TypedAST {
    pub module: ModuleId,
    pub blocks: IndexVec<BlockRef, Typed<Block>>,
    pub statements: IndexVec<StmtRef, Stmt>,
    pub exprs: IndexVec<ExprRef, Typed<Expr>>,

    pub used_items: Vec<UsedItem>,
}

impl TypedAST {
    pub fn add_block(&mut self, block: Block, ty: TypeInfoRef, span: Span) -> BlockRef {
        let result = BlockRef(self.blocks.len());

        self.blocks.push(Typed { value: block, span, ty });

        result
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtRef {
        let result = StmtRef(self.statements.len());

        self.statements.push(stmt);

        result
    }

    pub fn add_expr(&mut self, expr: Expr, ty: TypeInfoRef, span: Span) -> ExprRef {
        let result = ExprRef(self.exprs.len());

        self.exprs.push(Typed { value: expr, span, ty });

        result
    }

    pub fn add_error_expr(&mut self, error: TypeErrorRef, ty: TypeInfoRef, span: Span) -> ExprRef {
        let result = ExprRef(self.exprs.len());

        self.exprs.push(Typed {
            value: Expr::Error(error),
            span,
            ty,
        });

        result
    }

    pub fn use_item(&mut self, item: UsedItem) {
        if !self.used_items.contains(&item) {
            self.used_items.push(item);
        }
    }

    /// Function for adding used VTable to Typed AST
    #[track_caller]
    pub fn use_vtable_impl<S>(&mut self, checker: &mut TypeChecker<S>, target: TypeInfoRef, vtable: VTableRef) -> Box<[TypeInfoRef]> {
        let type_args = match checker.solver.get_info(target) {
            TypeInfo::Adt(.., args) => args.clone(),
            &TypeInfo::Array(element, _) => Box::new([element]),
            _ => Box::new([]),
        };

        for item in &checker.vtables[vtable].used_items {
            match item {
                UsedItem::VTable(ty, vtable, input_type_args) => {
                    if checker.current_vtable == Some(*vtable) {
                        continue;
                    }

                    self.use_item(UsedItem::VTable(*ty, *vtable, input_type_args.clone()));
                }
                UsedItem::Adt(adt_ref, adt_type_args) => {
                    let type_args = adt_type_args.iter().map(|&ty| checker.solver.solve_generic_args(ty, &type_args)).collect();

                    self.use_item(UsedItem::Adt(*adt_ref, type_args));
                }
                &UsedItem::Func(func_ref) => self.use_item(UsedItem::Func(func_ref)),
            }
        }

        if checker.current_vtable != Some(vtable) {    
            let vtable_type_args: Box<[_]> = checker.vtables[vtable]
                .generics
                .iter()
                .map(|&ty| checker.solver.solve_generic_args(ty, &type_args))
                .collect();

            self.use_item(UsedItem::VTable(target, vtable, vtable_type_args));
        }

        type_args
    }
}

impl Index<ExprRef> for TypedAST {
    type Output = Typed<Expr>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.exprs[index]
    }
}

impl Index<BlockRef> for TypedAST {
    type Output = Typed<Block>;

    fn index(&self, index: BlockRef) -> &Self::Output {
        &self.blocks[index]
    }
}

impl Index<StmtRef> for TypedAST {
    type Output = Stmt;

    fn index(&self, index: StmtRef) -> &Self::Output {
        &self.statements[index]
    }
}

pub type TraitFunc = (String, Vec<FuncArg<FieldType>>, FieldType);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VTableFuncKind<S = ()> {
    Local(BlockRef),
    External(&'static str),
    Special(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum IntrinsicKind {
    SizeOf,
    AlignOf,
    SizeOfValue,
    AlignOfValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub postfix: bool,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: TypeInfoRef,
    pub kind: VTableFuncKind,
}

impl Func {
    pub fn external<T: Into<String>>(name: T, ty: TypeInfoRef, ext_name: &'static str) -> Self {
        Self {
            postfix: false,
            name: name.into(),
            arg_names: Vec::new(),
            ty,
            kind: VTableFuncKind::External(ext_name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VTableFunc<T = TypeInfoRef, S = ()> {
    pub trait_func: Option<TraitFuncRef>,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: T,
    pub kind: VTableFuncKind<S>,
}

#[derive(Debug)]
pub struct VTableGenerator<S = ()> {
    pub origin_trait: Option<TraitRef>,
    pub generics: Box<[TypeInfoRef]>,
    pub applied_generics: Box<[TypeInfoRef]>,
    pub used_items: Vec<UsedItem>,
    pub functions: IndexVec<VFuncRef, VTableFunc<TypeInfoRef, S>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    SubModule(ModuleId),
    Adt(AdtRef),
    Trait(TraitRef),
    Func(FuncRef),
    Intrinsic(IntrinsicKind, TypeInfoRef),
}

pub struct Module {
    pub parent: Option<ModuleId>,
    pub name: String,
    pub id: ModuleId,
    pub items: IndexMap<String, ModuleItem>,
}

impl Module {
    pub fn new<T: Into<String>>(id: ModuleId, name: T) -> Self {
        Self {
            parent: None,
            name: name.into(),
            id,
            items: IndexMap::new(),
        }
    }
}

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
            if func.0 == name {
                break;
            }

            offset += size_of::<usize>();
        }

        offset
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LangItemValue {
    Adt(AdtRef),
    AdtVariant(AdtRef, AdtVariantRef),
    Trait(TraitRef),
    TraitFunc(TraitRef, TraitFuncRef),
}

pub struct TypeChecker<S = ()> {
    pub core_types: CoreTypes,
    pub solver: TypeSolver,

    pub infer: Option<TypeInfoRef>,

    pub current_vtable: Option<VTableRef>,
    pub available_generics: HashMap<String, (usize, Option<TypeInfoRef>)>,
    pub captures: Vec<(String, Variable)>,

    pub language_items: HashMap<LangItem, LangItemValue>,

    pub modules: IndexVec<ModuleId, Module>,
    pub adt_types: IndexVec<AdtRef, Adt>,
    pub local_functions: IndexVec<FuncRef, Func>,
    pub traits: IndexVec<TraitRef, Trait>,
    pub vtables: IndexVec<VTableRef, VTableGenerator<S>>,

    pub errors: IndexVec<TypeErrorRef, Positioned<TypeError>>,

    pub returns: Option<TypeInfoRef>,
}

impl<S> TypeChecker<S> {
    pub fn new() -> Self {
        let mut solver = TypeSolver::default();

        solver.push_frame();

        let core_types = CoreTypes {
            void: solver.add_info(TypeInfo::Primitive(PrimitiveType::Void)),
            any: solver.add_info(TypeInfo::Primitive(PrimitiveType::Any)),
            boolean: solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean)),
            int8: solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::I8))),
            int16: solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::I16))),
            int32: solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::I32))),
            int64: solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::I64))),
            int_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::Int(IntType::ISize))),
            uint8: solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U8))),
            uint16: solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U16))),
            uint32: solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U32))),
            uint64: solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::U64))),
            uint_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::UInt(UIntType::USize))),
            float: solver.add_info(TypeInfo::Primitive(PrimitiveType::Float)),
            component: solver.add_info(TypeInfo::Primitive(PrimitiveType::Component)),
            string: solver.add_info(TypeInfo::Primitive(PrimitiveType::String)),
        };

        Self {
            returns: None,
            current_vtable: None,
            core_types,
            solver,
            infer: None,
            available_generics: HashMap::new(),
            language_items: HashMap::new(),
            captures: Vec::new(),
            modules: IndexVec::from_iter([Module::new(ModuleId::ZERO, "<anonymous>")]),
            adt_types: IndexVec::new(),
            local_functions: IndexVec::new(),
            traits: IndexVec::new(),
            errors: IndexVec::new(),
            vtables: IndexVec::new(),
        }
    }

    pub fn add_error(&mut self, error: TypeError, span: Span) -> TypeErrorRef {
        let result = TypeErrorRef(self.errors.len());

        self.errors.push(span.wrap(error));

        result
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

    pub fn type_check<'a, T: AsRef<str>>(&'a mut self, code: T, ast: &mut TypedAST) -> TypeResult<'a, BlockRef> {
        let mut parser = mollie_parser::Parser::new(mollie_lexer::Lexer::lex(code.as_ref()));

        let (stmts, final_stmt) = match mollie_parser::parse_statements_until(&mut parser, &mollie_lexer::Token::EOF) {
            Ok(it) => it,
            Err(err) => {
                self.errors.push(Span::default().wrap(TypeError::Parse(err)));

                return Err(TypeErrors(&self.errors.raw));
            }
        };

        let block = mollie_parser::BlockExpr {
            stmts,
            final_stmt: final_stmt.map(Box::new),
        }
        .into_typed_ast(self, ast, Span::default());

        if !self.errors.is_empty() {
            return Err(TypeErrors(&self.errors.raw));
        }

        if let Some(returns) = self.returns
            && let &TypeInfo::Trait(t, _) = self.solver.get_info(returns)
            && let Some(vtables) = self.solver.find_vtable(&FieldType::from_type_info_ref(ast[block].ty, &self.solver))
            && let Some(&vtable) = vtables.get(&Some(t))
        {
            for item in &self.vtables[vtable].used_items {
                ast.use_item(item.clone());
            }

            let vtable = UsedItem::VTable(ast[block].ty, vtable, Box::new([]));

            ast.use_item(vtable);
        }

        Ok(block)
    }

    pub fn type_errors(&mut self) -> impl Iterator<Item = TypeErrorDisplay<'_, S>> {
        mem::take(&mut self.solver.errors).into_iter().map(|error| self.display_of_error(error))
    }

    pub fn register_module<T: Into<String>>(&mut self, name: T) -> ModuleId {
        let id = self.modules.next_index();
        let name = name.into();

        self.modules[ModuleId::ZERO].items.insert(name.clone(), ModuleItem::SubModule(id));
        self.modules.insert(Module {
            parent: Some(ModuleId::ZERO),
            name,
            id,
            items: IndexMap::new(),
        })
    }

    pub fn register_adt(&mut self, ty: Adt) -> AdtRef {
        self.register_adt_in_module(ModuleId::ZERO, ty)
    }

    pub fn register_adt_in_module(&mut self, module: ModuleId, ty: Adt) -> AdtRef {
        let type_ref = self.adt_types.insert(ty);

        if let Some(name) = self.adt_types[type_ref].name.clone() {
            self.modules[module].items.insert(name, ModuleItem::Adt(type_ref));
        }

        type_ref
    }

    pub fn register_trait(&mut self, r#trait: Trait) -> TraitRef {
        self.register_trait_in_module(ModuleId::ZERO, r#trait)
    }

    pub fn register_trait_in_module(&mut self, module: ModuleId, r#trait: Trait) -> TraitRef {
        let trait_ref = self.traits.insert(r#trait);

        self.modules[module]
            .items
            .insert(self.traits[trait_ref].name.clone(), ModuleItem::Trait(trait_ref));

        trait_ref
    }

    pub fn register_func_in_module(&mut self, module: ModuleId, func: Func) -> FuncRef {
        let func = self.local_functions.insert(func);

        self.modules[module]
            .items
            .insert(self.local_functions[func].name.clone(), ModuleItem::Func(func));

        func
    }

    pub fn register_intrinsic_in_module<T: Into<String>>(&mut self, module: ModuleId, name: T, kind: IntrinsicKind, ty: TypeInfo) {
        self.modules[module]
            .items
            .insert(name.into(), ModuleItem::Intrinsic(kind, self.solver.add_info(ty)));
    }

    pub fn instantiate_adt(&mut self, ty: AdtRef, generic_args: &[TypeInfoRef]) -> (TypeInfoRef, FieldType) {
        let kind = self.adt_types[ty].kind;
        let generics = self.adt_types[ty].generics;
        let args = generic_args
            .iter()
            .copied()
            .map(Some)
            .chain(iter::repeat(None))
            .take(generics)
            .map(|arg| arg.unwrap_or_else(|| self.solver.add_info(TypeInfo::Unknown(None))))
            .collect();

        let type_ref = self.solver.add_info(TypeInfo::Adt(ty, kind, args));

        (type_ref, FieldType::from_type_info_ref(type_ref, &self.solver))
    }

    pub fn try_cast(&self, module: ModuleId, input: &str) -> Result<CastedType, TypeInfoCastError> {
        let result = match input {
            "int_size" => Ok(CastedType::Primitive(PrimitiveType::Int(IntType::ISize))),
            "uint_size" => Ok(CastedType::Primitive(PrimitiveType::UInt(UIntType::USize))),
            "int64" => Ok(CastedType::Primitive(PrimitiveType::Int(IntType::I64))),
            "uint64" => Ok(CastedType::Primitive(PrimitiveType::UInt(UIntType::U64))),
            "int32" => Ok(CastedType::Primitive(PrimitiveType::Int(IntType::I32))),
            "uint32" => Ok(CastedType::Primitive(PrimitiveType::UInt(UIntType::U32))),
            "int16" => Ok(CastedType::Primitive(PrimitiveType::Int(IntType::I16))),
            "uint16" => Ok(CastedType::Primitive(PrimitiveType::UInt(UIntType::U16))),
            "int8" => Ok(CastedType::Primitive(PrimitiveType::Int(IntType::I8))),
            "uint8" => Ok(CastedType::Primitive(PrimitiveType::UInt(UIntType::U8))),
            "float" => Ok(CastedType::Primitive(PrimitiveType::Float)),
            "boolean" => Ok(CastedType::Primitive(PrimitiveType::Boolean)),
            "string" => Ok(CastedType::Primitive(PrimitiveType::String)),
            "component" => Ok(CastedType::Primitive(PrimitiveType::Component)),
            input => self.modules[module].items.get(input).map_or_else(
                || {
                    if module == ModuleId::ZERO && self.solver.get_var(input).is_some() {
                        Err(TypeInfoCastError::IsVariable)
                    } else {
                        Err(TypeInfoCastError::NotFound)
                    }
                },
                |item| match item {
                    ModuleItem::SubModule(_) => Err(TypeInfoCastError::IsModule),
                    &ModuleItem::Adt(adt_ref) => Ok(CastedType::Adt(adt_ref)),
                    &ModuleItem::Trait(trait_ref) => Ok(CastedType::Trait(trait_ref)),
                    &ModuleItem::Func(func_ref) => Ok(CastedType::Func(func_ref)),
                    &ModuleItem::Intrinsic(kind, ty) => Ok(CastedType::Intrinsic(kind, ty)),
                },
            ),
        }?;

        if let CastedType::Primitive(primitive) = result
            && module != ModuleId::ZERO
        {
            Err(TypeInfoCastError::TypeInPath(primitive))
        } else {
            Ok(result)
        }
    }
}

pub enum CastedType {
    Primitive(PrimitiveType),
    Adt(AdtRef),
    Trait(TraitRef),
    Func(FuncRef),
    Intrinsic(IntrinsicKind, TypeInfoRef),
}

pub enum TypeInfoCastError {
    IsModule,
    IsVariable,
    TypeInPath(PrimitiveType),
    NotFound,
}

pub struct ModuleDisplay<'a>(ModuleId, &'a IndexVec<ModuleId, Module>);

impl fmt::Display for ModuleDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.1[self.0].parent.map_or(Ok(()), |parent| {
            if parent == ModuleId::ZERO {
                write!(f, "{}", self.1[self.0].name)
            } else {
                write!(f, "{}::{}", Self(parent, self.1), self.1[self.0].name)
            }
        })
    }
}

impl<S> Default for TypeChecker<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S> Index<(VTableRef, VFuncRef)> for TypeChecker<S> {
    type Output = VTableFunc<TypeInfoRef, S>;

    fn index(&self, (vtable_ref, vfunc_ref): (VTableRef, VFuncRef)) -> &Self::Output {
        &self.vtables[vtable_ref].functions[vfunc_ref]
    }
}

impl<S> TypeStorage for TypeChecker<S> {
    fn get_adt(&self, adt_ref: AdtRef) -> &Adt {
        &self.adt_types[adt_ref]
    }

    fn get_trait_name(&self, trait_ref: TraitRef) -> Option<&str> {
        Some(self.traits[trait_ref].name.as_str())
    }
}

pub struct TypeDisplay<'a, S> {
    ty: TypeInfoRef,
    this: Option<TypeInfoRef>,
    checker: &'a TypeChecker<S>,
    short: bool,
}

impl<S> fmt::Display for TypeDisplay<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.checker.solver.fmt_type(f, self.ty, self.this, self.checker, self.short)
    }
}

pub struct TypeInfoDisplay<'a, 'b, S> {
    ty: &'a TypeInfo,
    this: Option<TypeInfoRef>,
    checker: &'b TypeChecker<S>,
}

impl<S> fmt::Display for TypeInfoDisplay<'_, '_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.checker.solver.fmt_type_info(f, self.ty, self.this, self.checker, false)
    }
}

impl<S> TypeChecker<S> {
    pub const fn save_state(&self) -> TypeCheckerState {
        TypeCheckerState {
            type_infos: self.solver.type_infos.len(),
            adt_types: self.adt_types.len(),
            traits: self.traits.len(),
            vtables: self.vtables.len(),
            local_functions: self.local_functions.len(),
        }
    }

    pub fn load_state(&mut self, state: &TypeCheckerState) {
        self.solver.type_infos.truncate(state.type_infos);
        self.adt_types.truncate(state.adt_types);
        self.traits.truncate(state.traits);
        self.vtables.truncate(state.vtables);
        self.local_functions.truncate(state.local_functions);
    }

    pub fn name_of_trait(&self, trait_ref: TraitRef) -> &str {
        self.traits[trait_ref].name.as_str()
    }

    pub fn find_trait<T: AsRef<str>>(&self, trait_name: T) -> Option<TraitRef> {
        let name = trait_name.as_ref();

        for (trait_ref, trait_data) in self.traits.iter() {
            if trait_data.name == name {
                return Some(trait_ref);
            }
        }

        None
    }

    pub const fn display_of_error(&self, error: TypeUnificationError) -> TypeErrorDisplay<'_, S> {
        TypeErrorDisplay { checker: self, error }
    }

    pub const fn display_of_module(&self, module: ModuleId) -> ModuleDisplay<'_> {
        ModuleDisplay(module, &self.modules)
    }

    pub const fn display_of_type(&self, ty: TypeInfoRef, this: Option<TypeInfoRef>) -> TypeDisplay<'_, S> {
        TypeDisplay {
            ty,
            this,
            checker: self,
            short: false,
        }
    }

    pub const fn short_display_of_type(&self, ty: TypeInfoRef, this: Option<TypeInfoRef>) -> TypeDisplay<'_, S> {
        TypeDisplay {
            ty,
            this,
            checker: self,
            short: true,
        }
    }

    pub const fn display_of_type_info<'a, 'b>(&'b self, ty: &'a TypeInfo, this: Option<TypeInfoRef>) -> TypeInfoDisplay<'a, 'b, S> {
        TypeInfoDisplay { ty, this, checker: self }
    }
}

pub struct TypeErrorDisplay<'a, S> {
    checker: &'a TypeChecker<S>,
    error: TypeUnificationError,
}

impl<S> fmt::Display for TypeErrorDisplay<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            TypeUnificationError::TypeMismatch(expected, got) => {
                let expected_str = self.checker.display_of_type(expected, None);
                let got_str = self.checker.display_of_type(got, None);

                write!(f, "Type mismatch: `{expected_str}` was expected, found `{got_str}`")
            }
            TypeUnificationError::UnimplementedTrait(trait_ref, ty) => {
                let trait_name = self.checker.name_of_trait(trait_ref);
                let ty_str = self.checker.display_of_type(ty, None);

                write!(f, "Trait cast error: `{trait_name}` is unimplemented for `{ty_str}`")
            }
            TypeUnificationError::ArraySizeMismatch(expected, got) => {
                write!(f, "Array type size mismatch: `{expected}` was expected, found `{got}`")
            }
            TypeUnificationError::UnknownType(ty) => write!(f, "Type infer error: type of {ty:?} is <unknown>"),
        }
    }
}

pub struct TypeCheckerState {
    pub type_infos: usize,
    pub adt_types: usize,
    pub traits: usize,
    pub vtables: usize,
    pub local_functions: usize,
}

#[derive(Debug, Serialize)]
pub struct Typed<T> {
    pub ty: TypeInfoRef,
    #[serde(flatten)]
    pub value: T,
    pub span: Span,
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

pub trait IntoConstantValue<S> {
    fn into_constant_value(self, checker: &TypeChecker<S>, ast: &TypedAST, context: &mut ConstantContext) -> Result<ConstantValue, ()>;
}

pub trait IntoTypedAST<S, T> {
    fn into_typed_ast(self, checker: &mut TypeChecker<S>, ast: &mut TypedAST, span: Span) -> T;
}

pub trait IntoPositionedTypedAST<S, T> {
    fn into_typed_ast(self, checker: &mut TypeChecker<S>, ast: &mut TypedAST) -> T;
}

impl<O, S, T: IntoTypedAST<S, O>> IntoPositionedTypedAST<S, O> for Positioned<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker<S>, ast: &mut TypedAST) -> O {
        self.value.into_typed_ast(checker, ast, self.span)
    }
}
