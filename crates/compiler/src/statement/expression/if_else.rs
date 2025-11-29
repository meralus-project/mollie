use cranelift::{
    codegen::ir::BlockArg,
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use mollie_parser::IfElseExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

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

            let returning_param = if let Some(final_stmt) = &self.value.block.value.final_stmt {
                let ty = final_stmt.get_type(compiler)?;

                Some(fn_builder.append_block_param(after_block, ty.variant.as_ir_type(compiler.jit.module.isa())))
            } else {
                None
            };

            let returned = if let Some(otherwise) = self.value.else_block {
                let else_block = fn_builder.create_block();

                fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

                fn_builder.switch_to_block(then_block);
                fn_builder.seal_block(then_block);

                let returned = self.value.block.compile(compiler, fn_builder)?;

                if let ValueOrFunc::Value(returned) = returned {
                    fn_builder.ins().jump(after_block, &[BlockArg::Value(returned)]);
                } else {
                    fn_builder.ins().jump(after_block, &[]);
                }

                fn_builder.switch_to_block(else_block);
                fn_builder.seal_block(else_block);

                otherwise.compile(compiler, fn_builder)?
            } else {
                fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

                fn_builder.switch_to_block(then_block);
                fn_builder.seal_block(then_block);

                self.value.block.compile(compiler, fn_builder)?
            };

            if let ValueOrFunc::Value(returned) = returned {
                fn_builder.ins().jump(after_block, &[BlockArg::Value(returned)]);
            } else {
                fn_builder.ins().jump(after_block, &[]);
            }

            fn_builder.switch_to_block(after_block);
            fn_builder.seal_block(after_block);

            returning_param.map_or(Ok(ValueOrFunc::Nothing), |returning_param| Ok(ValueOrFunc::Value(returning_param)))
        } else {
            Ok(ValueOrFunc::Nothing)
        }
    }
}

impl GetType for IfElseExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.block.get_type(compiler)
    }
}

impl GetNewType for IfElseExpr {
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

        let result_expected = self.block.get_new_type(compiler, core_types, type_storage, type_solver)?;

        if let Some(else_block) = self.else_block.as_deref() {
            let block_result = else_block.get_new_type(compiler, core_types, type_storage, type_solver)?;

            type_solver.unify(result_expected, block_result);
        }

        Ok(result_expected)
    }
}

#[cfg(test)]
mod tests {
    use mollie_parser::{Expr, Parse};
    use mollie_typing::{CoreTypes, TypeSolver, TypeStorage, TypeVariant};

    use crate::{Compiler, GetNewPositionedType};

    #[test]
    fn test_if_else_expr_solving() {
        let mut storage = TypeStorage::default();
        let mut solver = TypeSolver::default();

        let types = CoreTypes {
            void: storage.add_type(TypeVariant::void()),
            any: storage.add_type(TypeVariant::any()),
            boolean: storage.add_named_type("boolean", TypeVariant::boolean()),
            int8: storage.add_named_type("int8", TypeVariant::int8()),
            int16: storage.add_named_type("int16", TypeVariant::int16()),
            int32: storage.add_named_type("int32", TypeVariant::int32()),
            int64: storage.add_named_type("int64", TypeVariant::int64()),
            int_size: storage.add_named_type("int_size", TypeVariant::isize()),
            uint8: storage.add_named_type("uint8", TypeVariant::uint8()),
            uint16: storage.add_named_type("uint16", TypeVariant::uint16()),
            uint32: storage.add_named_type("uint32", TypeVariant::uint32()),
            uint64: storage.add_named_type("uint64", TypeVariant::uint64()),
            uint_size: storage.add_named_type("uint_size", TypeVariant::usize()),
            float: storage.add_named_type("float", TypeVariant::float()),
            string: storage.add_type(TypeVariant::string()),
        };

        let mut compiler = Compiler::default();

        let expr = Expr::parse_value("if 60 == 30 { \"eblan\" } else { 4int32 }").unwrap();
        let ty = expr.get_new_type(&mut compiler, &types, &mut storage, &mut solver);

        if let Ok(ty) = ty {
            let ty = solver.get_actual_type(ty).unwrap();

            println!("{:?}", solver.format_type(ty, &storage));
        }
    }
}
