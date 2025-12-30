mod expression;
mod statement;
pub mod visitor;

use std::{collections::HashMap, fmt, ops::Index};

use indexmap::IndexMap;
use mollie_const::ConstantValue;
use mollie_index::{IndexVec, new_idx_type};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ComplexType, ComplexTypeRef, CoreTypes, FieldType, TraitRef, TypeInfoRef, TypeSolver, TypeUnificationError, VFuncRef, VTableRef};
use serde::Serialize;

pub use crate::{
    expression::{Block, Expr, IsPattern, LiteralExpr, VFunc},
    statement::Stmt,
};

new_idx_type!(BlockRef);
new_idx_type!(StmtRef);
new_idx_type!(ExprRef);
new_idx_type!(FuncRef);
new_idx_type!(TraitFuncRef);

#[derive(Debug, Serialize)]
pub struct TypedAST {
    pub blocks: IndexVec<BlockRef, Typed<Block>>,
    pub statements: IndexVec<StmtRef, Stmt>,
    pub exprs: IndexVec<ExprRef, Typed<Expr>>,
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

pub type TraitFunc = (String, Vec<FieldType>, FieldType);

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VTableFunc<T = TypeInfoRef> {
    pub trait_func: Option<TraitFuncRef>,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: T,
    pub kind: VTableFuncKind,
}

pub struct TypeChecker {
    pub core_types: CoreTypes,
    pub solver: TypeSolver,

    pub available_generics: HashMap<String, usize>,

    pub name_to_complex_type: HashMap<String, ComplexTypeRef>,
    pub complex_types: IndexVec<ComplexTypeRef, ComplexType>,

    pub name_to_trait: HashMap<String, TraitRef>,
    pub traits: IndexVec<TraitRef, (usize, IndexVec<TraitFuncRef, TraitFunc>)>,

    pub vtables: IndexVec<VTableRef, IndexVec<VFuncRef, VTableFunc>>,

    pub local_functions: IndexVec<FuncRef, Func>,
}

pub struct TypeDisplay<'a> {
    ty: TypeInfoRef,
    this: Option<TypeInfoRef>,
    complex_types: &'a IndexVec<ComplexTypeRef, ComplexType>,
    solver: &'a TypeSolver,
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.solver.fmt_type(f, self.ty, self.this, self.complex_types)
    }
}

impl TypeChecker {
    pub const fn save_state(&self) -> TypeCheckerState {
        TypeCheckerState {
            type_infos: self.solver.type_infos.len(),
            complex_types: self.complex_types.len(),
            traits: self.traits.len(),
            vtables: self.vtables.len(),
            local_functions: self.local_functions.len(),
        }
    }

    pub fn load_state(&mut self, state: &TypeCheckerState) {
        self.solver.type_infos.truncate(state.type_infos);
        self.complex_types.truncate(state.complex_types);
        self.traits.truncate(state.traits);
        self.vtables.truncate(state.vtables);
        self.local_functions.truncate(state.local_functions);
    }

    pub fn name_of_trait(&self, trait_ref: TraitRef) -> &str {
        self.name_to_trait
            .iter()
            .find_map(|(name, actual_trait)| if actual_trait == &trait_ref { Some(name.as_str()) } else { None })
            .unwrap_or_default()
    }

    pub const fn display_of_error(&self, error: TypeUnificationError) -> TypeErrorDisplay<'_> {
        TypeErrorDisplay { checker: self, error }
    }

    pub const fn display_of_type(&self, ty: TypeInfoRef, this: Option<TypeInfoRef>) -> TypeDisplay<'_> {
        TypeDisplay {
            ty,
            this,
            complex_types: &self.complex_types,
            solver: &self.solver,
        }
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

                write!(f, "type mismatch: `{expected_str}` was expected, found `{got_str}`")
            }
            TypeUnificationError::UnimplementedTrait(trait_ref, ty) => {
                let trait_name = self.checker.name_of_trait(trait_ref);
                let ty_str = self.checker.display_of_type(ty, None);

                write!(f, "trait cast error: `{trait_name}` is unimplemented for `{ty_str}`")
            }
            TypeUnificationError::ArraySizeMismatch(expected, got) => {
                write!(f, "array type size mismatch: `{expected}` was expected, found `{got}`")
            }
            TypeUnificationError::UnknownType(ty) => write!(f, "type infer error: type of {ty:?} is <unknown>"),
        }
    }
}

pub struct TypeCheckerState {
    pub type_infos: usize,
    pub complex_types: usize,
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
