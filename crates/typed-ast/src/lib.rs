mod expression;
mod statement;
pub mod visitor;

use std::{collections::HashMap, fmt, iter, mem, ops::Index};

use indexmap::IndexMap;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec, new_idx_type};
use mollie_shared::{Positioned, Span};
use mollie_typing::{
    Adt, AdtRef, CoreTypes, FieldType, FuncArg, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver, TypeStorage, TypeUnificationError,
    UIntType, VFuncRef, VTableRef, Variable,
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
    NoField { adt: AdtRef, name: String },
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

#[derive(Debug, Default, Serialize)]
pub struct TypedAST {
    pub module: ModuleId,
    pub blocks: IndexVec<BlockRef, Typed<Block>>,
    pub statements: IndexVec<StmtRef, Stmt>,
    pub exprs: IndexVec<ExprRef, Typed<Expr>>,

    pub used_vtables: Vec<(TypeInfoRef, VTableRef, Box<[TypeInfoRef]>)>,
    pub used_adt_types: Vec<(AdtRef, Box<[TypeInfoRef]>)>,
    pub used_functions: Vec<FuncRef>,
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
pub enum VTableFuncKind {
    Local(BlockRef),
    External(&'static str),
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
pub struct VTableFunc<T = TypeInfoRef> {
    pub trait_func: Option<TraitFuncRef>,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: T,
    pub kind: VTableFuncKind,
}

#[derive(Debug)]
pub struct VTableGenerator {
    pub origin_trait: Option<TraitRef>,
    pub generics: Box<[TypeInfoRef]>,
    pub used_vtables: Vec<(TypeInfoRef, VTableRef, Box<[TypeInfoRef]>)>,
    pub used_adt_types: Vec<(AdtRef, Box<[TypeInfoRef]>)>,
    pub functions: IndexVec<VFuncRef, VTableFunc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    SubModule(ModuleId),
    Adt(AdtRef),
    Trait(TraitRef),
    Func(FuncRef),
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

pub struct TypeChecker {
    pub core_types: CoreTypes,
    pub solver: TypeSolver,

    pub infer: Option<TypeInfoRef>,

    pub available_generics: HashMap<String, (usize, Option<TypeInfoRef>)>,
    pub captures: Vec<(String, Variable)>,

    pub modules: IndexVec<ModuleId, Module>,
    pub adt_types: IndexVec<AdtRef, Adt>,
    pub local_functions: IndexVec<FuncRef, Func>,
    pub traits: IndexVec<TraitRef, Trait>,
    pub vtables: IndexVec<VTableRef, VTableGenerator>,

    pub errors: IndexVec<TypeErrorRef, Positioned<TypeError>>,

    pub returns: Option<TypeInfoRef>,
}

impl TypeChecker {
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
            core_types,
            solver,
            infer: None,
            available_generics: HashMap::new(),
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
            for (adt_ref, type_args) in &self.vtables[vtable].used_adt_types {
                let adt = (*adt_ref, type_args.clone());

                if !ast.used_adt_types.contains(&adt) {
                    ast.used_adt_types.push(adt);
                }
            }

            for (ty, vtable, type_args) in &self.vtables[vtable].used_vtables {
                let vtable = (*ty, *vtable, type_args.clone());

                if !ast.used_vtables.contains(&vtable) {
                    ast.used_vtables.push(vtable);
                }
            }

            let vtable = (ast[block].ty, vtable, Box::new([]) as Box<[_]>);

            if !ast.used_vtables.contains(&vtable) {
                ast.used_vtables.push(vtable);
            }
        }

        Ok(block)
    }

    pub fn type_errors(&mut self) -> impl Iterator<Item = TypeErrorDisplay<'_>> {
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

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<(VTableRef, VFuncRef)> for TypeChecker {
    type Output = VTableFunc;

    fn index(&self, (vtable_ref, vfunc_ref): (VTableRef, VFuncRef)) -> &Self::Output {
        &self.vtables[vtable_ref].functions[vfunc_ref]
    }
}

impl TypeStorage for TypeChecker {
    fn get_adt(&self, adt_ref: AdtRef) -> &Adt {
        &self.adt_types[adt_ref]
    }

    fn get_trait_name(&self, trait_ref: TraitRef) -> Option<&str> {
        Some(self.traits[trait_ref].name.as_str())
    }
}

pub struct TypeDisplay<'a> {
    ty: TypeInfoRef,
    this: Option<TypeInfoRef>,
    checker: &'a TypeChecker,
    short: bool,
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.checker.solver.fmt_type(f, self.ty, self.this, self.checker, self.short)
    }
}

pub struct TypeInfoDisplay<'a, 'b> {
    ty: &'a TypeInfo,
    this: Option<TypeInfoRef>,
    checker: &'b TypeChecker,
}

impl fmt::Display for TypeInfoDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.checker.solver.fmt_type_info(f, self.ty, self.this, self.checker, false)
    }
}

impl TypeChecker {
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

    pub const fn display_of_error(&self, error: TypeUnificationError) -> TypeErrorDisplay<'_> {
        TypeErrorDisplay { checker: self, error }
    }

    pub const fn display_of_module(&self, module: ModuleId) -> ModuleDisplay<'_> {
        ModuleDisplay(module, &self.modules)
    }

    pub const fn display_of_type(&self, ty: TypeInfoRef, this: Option<TypeInfoRef>) -> TypeDisplay<'_> {
        TypeDisplay {
            ty,
            this,
            checker: self,
            short: false,
        }
    }

    pub const fn short_display_of_type(&self, ty: TypeInfoRef, this: Option<TypeInfoRef>) -> TypeDisplay<'_> {
        TypeDisplay {
            ty,
            this,
            checker: self,
            short: true,
        }
    }

    pub const fn display_of_type_info<'a, 'b>(&'b self, ty: &'a TypeInfo, this: Option<TypeInfoRef>) -> TypeInfoDisplay<'a, 'b> {
        TypeInfoDisplay { ty, this, checker: self }
    }
}

pub struct TypeErrorDisplay<'a> {
    checker: &'a TypeChecker,
    error: TypeUnificationError,
}

impl fmt::Display for TypeErrorDisplay<'_> {
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

pub trait IntoConstantValue {
    fn into_constant_value(self, checker: &TypeChecker, ast: &TypedAST, context: &mut ConstantContext) -> Result<ConstantValue, ()>;
}

pub trait IntoTypedAST<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> T;
}

pub trait IntoPositionedTypedAST<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST) -> T;
}

impl<O, T: IntoTypedAST<O>> IntoPositionedTypedAST<O> for Positioned<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST) -> O {
        self.value.into_typed_ast(checker, ast, self.span)
    }
}
