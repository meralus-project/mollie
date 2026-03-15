use cranelift::{module::Module, prelude::InstBuilder};
use mollie_typed_ast::{BlockRef, ExprRef, TypedAST};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, ML: mollie_typed_ast::ModuleLoader<S>, M: Module> FunctionCompiler<'_, S, ML, M> {
    pub fn compile_while_expr(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef) -> CompileResult<MolValue> {
        self.push_frame();

        let condition_block = self.fn_builder.create_block();
        let inner_block = self.fn_builder.create_block();
        let after_block = self.fn_builder.create_block();

        self.fn_builder.ins().jump(condition_block, &[]);
        self.fn_builder.switch_to_block(condition_block);

        self.branches = Some((inner_block, after_block));

        if let MolValue::Value(value) = condition.compile(ast, self)? {
            self.fn_builder.ins().brif(value, inner_block, &[], after_block, &[]);
        }

        self.branches.take();

        self.fn_builder.switch_to_block(inner_block);
        self.fn_builder.seal_block(inner_block);

        let value = block.compile(ast, self)?;

        self.unmark_variables();
        self.pop_frame();

        self.fn_builder.ins().jump(condition_block, &[]);
        self.fn_builder.switch_to_block(after_block);
        self.fn_builder.seal_block(condition_block);
        self.fn_builder.seal_block(after_block);

        Ok(value)
    }
}
