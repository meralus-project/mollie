use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{ExprRef, TypedAST};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, ML: mollie_typed_ast::ModuleLoader<S>, M: Module> FunctionCompiler<'_, S, ML, M> {
    /// Compiles the expression `target[index]( = value)?`. Returns
    /// [`MolValue::Nothing`] if an assignment operation occurs, or
    /// [`MolValue::FatPtr`]/[`MolValue::Value`] depending on the type of the
    /// array element.
    ///
    /// # Errors
    /// Returns [`CompileError`] if there is a compilation error of `target`,
    /// `index` or optional `value`.
    ///
    /// [`CompileError`]: crate::error::CompileError
    pub fn compile_array_index(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, index: ExprRef) -> CompileResult<MolValue> {
        let target = target.compile(ast, self)?;
        let index = index.compile(ast, self)?;

        if let (&MolValue::Value(ptr), &MolValue::Value(index)) = (&target, &index) {
            let element_type = ast[expr].ty.as_ir_type(&self.type_context.type_context.types, self.compiler.isa());
            let size = match element_type {
                MollieType::Fat(ty, metadata_ty) => ty.bytes() + metadata_ty.bytes(),
                MollieType::Regular(ty) => ty.bytes(),
            };

            let ptr_type = self.compiler.ptr_type();
            let ptr = self
                .fn_builder
                .ins()
                .load(ptr_type, ir::MemFlags::trusted(), ptr, size_of::<usize>().cast_signed() as i32 * 2);

            let offset = self.fn_builder.ins().imul_imm(index, i64::from(size));
            let ptr = self.fn_builder.ins().iadd(ptr, offset);

            if let Some((_, operator, assign)) = self.assign_ref.take_if(|(lhs_ref, ..)| *lhs_ref == expr) {
                if let MolValue::Value(value) = assign.compile(ast, self)? {
                    let value = match operator {
                        mollie_shared::Operator::Assign => value,
                        op if let MollieType::Regular(ty) = element_type
                            && let Some(operator) = op.lower() =>
                        {
                            let current_value = self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0);

                            self.bin_op(current_value, ast[expr].ty, operator, value, ast[assign].ty)
                        }
                        _ => return Ok(MolValue::Nothing),
                    };

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
