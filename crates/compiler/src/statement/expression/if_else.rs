use cranelift::prelude::{FunctionBuilder, InstBuilder};
use mollie_parser::IfElseExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IfElseExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let condition_type = self.value.condition.get_type(compiler)?;

        if !condition_type.variant.same_as(&TypeVariant::boolean(), &compiler.generics) {
            return Err(TypeError::Unexpected {
                expected: Box::new(TypeVariant::boolean().kind().into()),
                got: Box::new(condition_type.kind()),
            }
            .into());
        }

        if let ValueOrFunc::Value(cond_result) = self.value.condition.compile(compiler, fn_builder)? {
            let then_block = fn_builder.create_block();
            let after_block = fn_builder.create_block();

            if let Some(otherwise) = self.value.else_block {
                let else_block = fn_builder.create_block();

                fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

                fn_builder.switch_to_block(then_block);
                fn_builder.seal_block(then_block);

                self.value.block.compile(compiler, fn_builder)?;

                fn_builder.ins().jump(after_block, &[]);

                fn_builder.switch_to_block(else_block);
                fn_builder.seal_block(else_block);
                fn_builder.ins().jump(after_block, &[]);

                otherwise.compile(compiler, fn_builder)?;
            } else {
                fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

                fn_builder.switch_to_block(then_block);
                fn_builder.seal_block(then_block);

                self.value.block.compile(compiler, fn_builder)?;

                fn_builder.ins().jump(after_block, &[]);
            }

            fn_builder.switch_to_block(after_block);
            fn_builder.seal_block(after_block);
        }

        Ok(ValueOrFunc::Nothing)
    }
}

impl GetType for IfElseExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.block.get_type(compiler)
    }
}
