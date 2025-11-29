use cranelift::prelude::{FunctionBuilder, InstBuilder};
use mollie_parser::WhileExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

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

        compiler.push_frame();

        let condition_block = fn_builder.create_block();
        let inner_block = fn_builder.create_block();
        let after_block = fn_builder.create_block();

        fn_builder.ins().jump(condition_block, &[]);
        fn_builder.switch_to_block(condition_block);

        if let ValueOrFunc::Value(value) = self.value.condition.compile(compiler, fn_builder)? {
            fn_builder.ins().brif(value, inner_block, &[], after_block, &[]);
        }

        fn_builder.switch_to_block(inner_block);
        fn_builder.seal_block(inner_block);

        let value = self.value.block.compile(compiler, fn_builder)?;

        fn_builder.ins().jump(condition_block, &[]);

        fn_builder.switch_to_block(after_block);

        fn_builder.seal_block(condition_block);
        fn_builder.seal_block(after_block);

        compiler.pop_frame();

        Ok(value)
    }
}

impl GetType for WhileExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.block.get_type(compiler)
    }
}

impl GetNewType for WhileExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        let condition_expected = type_solver.add_info(mollie_typing::TypeInfo::Type(core_types.boolean));
        let condition = self.condition.get_new_type(compiler, core_types, type_storage, type_solver)?;

        type_solver.unify(condition_expected, condition);

        let result_expected = type_solver.add_info(mollie_typing::TypeInfo::Type(core_types.void));
        let result = self.block.get_new_type(compiler, core_types, type_storage, type_solver)?;

        type_solver.unify(result_expected, result);

        Ok(result_expected)
    }
}
