use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{BlockRef, ExprRef, TypedAST};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_if_expr(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef, else_block: Option<ExprRef>) -> CompileResult<MolValue> {
        self.push_frame();

        let cond_result = condition.compile(ast, self)?.expect_value();
        let then_block = self.fn_builder.create_block();
        let after_block = self.fn_builder.create_block();

        let returning_param = if let Some(final_stmt) = &ast[block].value.expr {
            Some(match ast[*final_stmt].ty.as_ir_type(&self.checker.solver, self.compiler.isa()) {
                MollieType::Fat(ty, metadata_ty) => MolValue::FatPtr(
                    self.fn_builder.append_block_param(after_block, ty),
                    self.fn_builder.append_block_param(after_block, metadata_ty),
                ),
                MollieType::Regular(ty) => MolValue::Value(self.fn_builder.append_block_param(after_block, ty)),
            })
        } else {
            None
        };

        let returned = if let Some(otherwise) = else_block {
            let else_block = self.fn_builder.create_block();

            self.fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

            self.fn_builder.switch_to_block(then_block);
            self.fn_builder.seal_block(then_block);

            let returned = block.compile(ast, self)?;

            match returned {
                MolValue::Value(value) => self.fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
                MolValue::Values(values) => self
                    .fn_builder
                    .ins()
                    .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
                MolValue::FuncRef(_) | MolValue::CaptureFuncRef(..) => todo!(),
                MolValue::FatPtr(value, metadata) => self
                    .fn_builder
                    .ins()
                    .jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)]),
                MolValue::Nothing => self.fn_builder.ins().jump(after_block, &[]),
            };

            self.pop_frame();

            self.fn_builder.switch_to_block(else_block);
            self.fn_builder.seal_block(else_block);

            otherwise.compile(ast, self)?
        } else {
            self.fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

            self.fn_builder.switch_to_block(then_block);
            self.fn_builder.seal_block(then_block);

            let returned = block.compile(ast, self)?;

            self.pop_frame();

            returned
        };

        match returned {
            MolValue::Value(value) => self.fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
            MolValue::Values(values) => self
                .fn_builder
                .ins()
                .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
            MolValue::FuncRef(_) | MolValue::CaptureFuncRef(..) => todo!(),
            MolValue::FatPtr(value, metadata) => self
                .fn_builder
                .ins()
                .jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)]),
            MolValue::Nothing => self.fn_builder.ins().jump(after_block, &[]),
        };

        self.fn_builder.switch_to_block(after_block);
        self.fn_builder.seal_block(after_block);

        returning_param.map_or(Ok(MolValue::Nothing), Ok)
    }
}
