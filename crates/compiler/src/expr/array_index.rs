use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{ExprRef, TypedAST};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_array_index(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, index: ExprRef) -> CompileResult<MolValue> {
        let target = target.compile(ast, self)?;
        let index = index.compile(ast, self)?;

        if let (&MolValue::FatPtr(ptr, _), &MolValue::Value(index)) = (&target, &index) {
            let element_type = ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa());
            let size = match element_type {
                MollieType::Fat(ty, metadata_ty) => ty.bytes() + metadata_ty.bytes(),
                MollieType::Regular(ty) => ty.bytes(),
            };

            let size = self.fn_builder.ins().iconst(self.compiler.ptr_type(), i64::from(size));
            let offset = self.fn_builder.ins().imul(size, index);
            let ptr = self.fn_builder.ins().iadd(ptr, offset);

            if let Some((_, assign)) = self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
                if let MolValue::Value(value) = assign.compile(ast, self)? {
                    self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, 0);

                    return Ok(MolValue::Nothing);
                }
            } else {
                return match element_type {
                    MollieType::Fat(ty, metadata_ty) => Ok(MolValue::FatPtr(
                        self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0),
                        self.fn_builder.ins().load(metadata_ty, ir::MemFlags::trusted(), ptr, ty.bytes().cast_signed()),
                    )),
                    MollieType::Regular(ty) => Ok(MolValue::Value(self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0))),
                };
            }
        }

        todo!("no target[index] for {target:?}[{index:?}]")
    }
}
