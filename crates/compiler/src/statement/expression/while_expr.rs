use cranelift::prelude::FunctionBuilder;
use mollie_parser::WhileExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<WhileExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let condition_type = self.value.condition.get_type(compiler)?;

        if !condition_type.variant.same_as(&TypeVariant::boolean(), &compiler.generics) {
            return Err(TypeError::Unexpected {
                expected: Box::new(TypeVariant::boolean().kind().into()),
                got: Box::new(condition_type.kind()),
            }
            .into());
        }

        // let loop_start = chunk.len();

        // chunk.push_frame();
        compiler.push_frame();

        self.value.condition.compile(compiler, fn_builder)?;

        // let start = chunk.len();

        // chunk.jump_if_false(0);

        self.value.block.compile(compiler, fn_builder)?;

        compiler.pop_frame();
        // chunk.pop_frame();

        // chunk.jump(-((chunk.len() - loop_start) as isize));

        // chunk[start] = Inst::JumpIfFalse(chunk.len() - start);

        // chunk.pop_frame();

        unimplemented!()
    }
}

impl GetType for WhileExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.block.get_type(compiler)
    }
}
