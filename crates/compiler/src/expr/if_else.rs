use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{BlockRef, ExprRef, TypedAST};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, ML: mollie_typed_ast::ModuleLoader<S>, M: Module> FunctionCompiler<'_, S, ML, M> {
    pub fn compile_if_expr(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef, else_block: Option<ExprRef>) -> CompileResult<MolValue> {
        self.push_frame();

        let then_block = self.fn_builder.create_block();
        let after_block = self.fn_builder.create_block();
        let else_block = else_block.map(|block| (block, self.fn_builder.create_block()));

        self.branches = Some((then_block, else_block.map_or(after_block, |b| b.1)));

        let cond_result = condition.compile(ast, self)?;

        self.branches.take();

        let returning_param = if let Some(final_stmt) = &ast[block].value.expr {
            Some(
                match ast[*final_stmt].ty.as_ir_type(&self.type_context.type_context.types, self.compiler.isa()) {
                    MollieType::Fat(ty, metadata_ty) => MolValue::FatPtr(
                        self.fn_builder.append_block_param(after_block, ty),
                        self.fn_builder.append_block_param(after_block, metadata_ty),
                    ),
                    MollieType::Regular(ty) => MolValue::Value(self.fn_builder.append_block_param(after_block, ty)),
                },
            )
        } else {
            None
        };

        let returned = if let Some((otherwise, else_block)) = else_block {
            if let MolValue::Value(cond_result) = cond_result {
                self.fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);
            }

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
            if let MolValue::Value(cond_result) = cond_result {
                self.fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);
            }

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
