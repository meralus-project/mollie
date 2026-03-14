use mollie_const::ConstantValue;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeContext, TypeInfo};

use crate::{
    ConstantContext, FirstPass, FromParsed, IntoConstVal, SolvedPass, TypedAST, TypedASTContextRef,
    expr::{Expr, ExprRef},
    stmt::{Stmt, StmtRef},
};

mollie_index::new_idx_type!(BlockRef);

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Box<[StmtRef]>,
    /// Final expression (returning value)
    pub expr: Option<ExprRef>,
}

impl<E> FromParsed<E, mollie_parser::BlockExpr, BlockRef> for Block {
    fn from_parsed(expr: mollie_parser::BlockExpr, ast: &mut TypedAST<FirstPass>, context: &mut TypedASTContextRef<'_, E>, span: Span) -> BlockRef {
        let mut stmts = Vec::new();

        context.solver.push_frame();

        for stmt in expr.stmts {
            if let Some(stmt) = Stmt::from_parsed(stmt.value, ast, context, stmt.span) {
                stmts.push(stmt);
            }
        }

        let stmts = stmts.into_boxed_slice();

        let (expr, ty) = match expr.final_stmt.map(|v| *v) {
            Some(Positioned {
                value: mollie_parser::Stmt::Expression(expr),
                span,
            }) => {
                let expr = Expr::from_parsed(expr, ast, context, span);

                (Some(expr), ast[expr].ty)
            }
            _ => (None, context.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None)),
        };

        context.solver.pop_frame();

        ast.add_block(Self { stmts, expr }, ty, span)
    }
}

impl IntoConstVal for BlockRef {
    fn into_const_val(self, ast: &TypedAST<SolvedPass>, type_context: &TypeContext, const_context: &mut ConstantContext) -> Result<ConstantValue, ()> {
        for &stmt in &ast[self].value.stmts {
            match &ast[stmt] {
                Stmt::Expr(expr) => {
                    expr.into_const_val(ast, type_context, const_context)?;
                }
                Stmt::NewVar { name, value, .. } => {
                    let value = value.into_const_val(ast, type_context, const_context)?;

                    const_context.set_var(name, value);
                }
            }
        }

        ast[self]
            .value
            .expr
            .map_or(Ok(ConstantValue::Nothing), |expr| expr.into_const_val(ast, type_context, const_context))
    }
}
