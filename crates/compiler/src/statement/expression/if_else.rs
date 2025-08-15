use cranelift::prelude::FunctionBuilder;
use mollie_parser::IfElseExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IfElseExpr> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let condition_type = self.value.condition.get_type(compiler)?;

        if !condition_type.variant.same_as(&TypeVariant::boolean(), &compiler.generics) {
            return Err(TypeError::Unexpected {
                expected: Box::new(TypeVariant::boolean().kind().into()),
                got: Box::new(condition_type.kind()),
            }
            .into());
        }

        // chunk.push_frame();
        // compiler.push_frame();

        // self.value.condition.compile(compiler, fn_builder)?;

        // let start = chunk.len();

        // chunk.jump_if_false(0);

        // let returns = self.value.block.compile(compiler, fn_builder)?;

        // chunk[start] = Inst::JumpIfFalse(chunk.len() - start);

        // if let Some(else_block) = self.value.else_block {
        //     chunk[start] = Inst::JumpIfFalse(chunk.len() - start + 1);

        //     let start = chunk.len();

        //     chunk.jump(0);

        //     else_block.compile(compiler, fn_builder)?;

        //     chunk[start] = Inst::Jump(chunk.len().cast_signed() - start.cast_signed());
        // }

        // compiler.pop_frame();
        // chunk.pop_frame();

        unimplemented!()
    }
}

impl GetType for IfElseExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.block.get_type(compiler)
    }
}
