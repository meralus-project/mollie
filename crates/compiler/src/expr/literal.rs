use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{ExprRef, LitExpr, TypedAST};

use crate::{AsIrType, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, ML: mollie_typed_ast::ModuleLoader<S>, M: Module> FunctionCompiler<'_, S, ML, M> {
    pub fn compile_lit_expr(&mut self, ast: &TypedAST, expr: ExprRef, lit_expr: &LitExpr) -> CompileResult<MolValue> {
        Ok(MolValue::Value(match lit_expr {
            &LitExpr::Int(value) => match ast[expr].ty.as_ir_type(&self.type_context.type_context.types, self.compiler.isa()) {
                MollieType::Regular(ty) => self.fn_builder.ins().iconst(ty, value),
                MollieType::Fat(..) => return Ok(MolValue::Nothing),
            },
            &LitExpr::F32(value) => self.fn_builder.ins().f32const(value),
            LitExpr::String(value) => {
                let data_id = self.compiler.codegen.static_data(value.as_bytes())?;
                let data_id = self.compiler.codegen.module.declare_data_in_func(data_id, self.fn_builder.func);
                let ptr_type = self.compiler.ptr_type();
                let ptr = self.fn_builder.ins().global_value(ptr_type, data_id);
                let size = self.fn_builder.ins().iconst(ptr_type, value.len().cast_signed() as i64);

                return Ok(MolValue::FatPtr(ptr, size));
            }
            &LitExpr::Bool(value) => self.fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
        }))
    }
}
