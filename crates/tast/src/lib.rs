mod expression;
mod index;
mod statement;
pub mod visitor;

use std::{fmt::Debug, hash::Hash};

use mollie_shared::{Positioned, Span};
use mollie_typing::resolver::{CoreTypes, TypeInfoRef, TypeSolver, TypeStorage};
use serde::Serialize;

use crate::{
    expression::{Block, Expr},
    index::IndexVec,
    statement::Stmt,
};

new_idx_type!(BlockRef);
new_idx_type!(StmtRef);
new_idx_type!(ExprRef);

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

impl std::ops::Index<ExprRef> for TypedAST {
    type Output = Typed<Expr>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.exprs[index]
    }
}

impl std::ops::Index<BlockRef> for TypedAST {
    type Output = Typed<Block>;

    fn index(&self, index: BlockRef) -> &Self::Output {
        &self.blocks[index]
    }
}

impl std::ops::Index<StmtRef> for TypedAST {
    type Output = Stmt;

    fn index(&self, index: StmtRef) -> &Self::Output {
        &self.statements[index]
    }
}

pub struct TypeChecker {
    pub core_types: CoreTypes,
    pub types: TypeStorage,
    pub solver: TypeSolver,
}

#[derive(Debug, Serialize)]
pub struct Typed<T> {
    pub ty: TypeInfoRef,
    #[serde(flatten)]
    pub value: T,
    pub span: Span,
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
