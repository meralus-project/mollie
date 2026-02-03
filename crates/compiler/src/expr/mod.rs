mod array;
mod array_index;
mod binary;
mod call;
mod closure;
mod construct;
mod field_access;
mod if_else;
mod is_pattern;
mod literal;
mod type_index;
mod var;
mod vtable_access;
mod r#while;

use cranelift::module::Module;
use mollie_typed_ast::{Expr, ExprRef, TypedAST};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> CompileTypedAST<M, MolValue> for ExprRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, M>) -> CompileResult<MolValue> {
        match &ast[self].value {
            Expr::Literal(literal_expr) => compiler.compile_lit_expr(ast, self, literal_expr),
            &Expr::If { condition, block, else_block } => compiler.compile_if_expr(ast, condition, block, else_block),
            Expr::Block(block_ref) => block_ref.compile(ast, compiler),
            Expr::Var(name) => compiler.compile_var_expr(ast, self, name.as_str()),
            &Expr::Access { target, field } => compiler.compile_field_access_expr(ast, self, target, field),
            &Expr::VTableAccess { target, func } => compiler.compile_vtable_access(ast, target, func),
            &Expr::Index { target, index } => compiler.compile_array_index(ast, self, target, index),
            &Expr::While { condition, block } => compiler.compile_while_expr(ast, condition, block),
            Expr::Array(elements) => compiler.compile_array_expr(ast, self, elements.as_ref()),
            &Expr::Binary { operator, lhs, rhs } => compiler.compile_bin_expr(ast, lhs, operator, rhs),
            Expr::Call { func, args } => compiler.compile_call_expr(ast, *func, args.as_ref()),
            Expr::Closure { args, captures, body } => compiler.compile_closure_expr(ast, self, args.as_ref(), captures.as_ref(), *body),
            Expr::Construct { ty, variant, fields } => compiler.compile_construct(ast, *ty, *variant, fields.as_ref()),
            Expr::IsPattern { target, pattern } => compiler.compile_is_pattern_expr(ast, *target, pattern),
            &Expr::TypeIndex { ty, path } => compiler.compile_type_index_expr(ast, ty, path),
            Expr::Nothing => Ok(MolValue::Nothing),
        }
    }
}
