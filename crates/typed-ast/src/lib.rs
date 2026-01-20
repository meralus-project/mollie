mod expression;
mod statement;
pub mod visitor;

use std::{collections::HashMap, fmt, ops::Index};

use indexmap::IndexMap;
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexVec, new_idx_type};
use mollie_shared::{Positioned, Span};
use mollie_typing::{
    Adt, AdtRef, CoreTypes, FieldType, FuncArg, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver, TypeStorage, TypeUnificationError,
    UIntType, VFuncRef, VTableRef,
};
use serde::Serialize;

pub use crate::{
    expression::{Block, Expr, IsPattern, LiteralExpr, TypePath, VFunc},
    statement::Stmt,
};

new_idx_type!(BlockRef);
new_idx_type!(StmtRef);
new_idx_type!(ExprRef);
new_idx_type!(FuncRef);
new_idx_type!(TraitFuncRef);
new_idx_type!(ModuleId);

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
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: TypeInfoRef,
    pub kind: VTableFuncKind,
}

impl Func {
    pub fn external<T: Into<String>>(name: T, ty: TypeInfoRef, ext_name: &'static str) -> Self {
        Self {
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

pub struct TypeChecker {
    pub core_types: CoreTypes,
    pub solver: TypeSolver,

    pub infer: Option<TypeInfoRef>,

    pub available_generics: HashMap<String, (usize, Option<TypeInfoRef>)>,

    pub modules: IndexVec<ModuleId, Module>,
    pub adt_types: IndexVec<AdtRef, Adt>,
    pub local_functions: IndexVec<FuncRef, Func>,
    pub traits: IndexVec<TraitRef, Trait>,
    pub vtables: IndexVec<VTableRef, VTableGenerator>,
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
            core_types,
            solver,
            infer: None,
            available_generics: HashMap::new(),
            modules: IndexVec::from_iter([Module::new(ModuleId::ZERO, "<anonymous>")]),
            adt_types: IndexVec::new(),
            local_functions: IndexVec::new(),
            traits: IndexVec::new(),
            vtables: IndexVec::new(),
        }
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

    pub const fn display_of_error(&self, error: TypeUnificationError) -> TypeErrorDisplay<'_> {
        TypeErrorDisplay { checker: self, error }
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
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST, span: Span) -> Result<T, ()>;
}

pub trait IntoPositionedTypedAST<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST) -> Result<T, ()>;
}

impl<O, T: IntoTypedAST<O>> IntoPositionedTypedAST<O> for Positioned<T> {
    fn into_typed_ast(self, checker: &mut TypeChecker, ast: &mut TypedAST) -> Result<O, ()> {
        self.value.into_typed_ast(checker, ast, self.span)
    }
}
