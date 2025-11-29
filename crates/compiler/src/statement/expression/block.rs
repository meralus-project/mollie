use cranelift::{
    codegen::ir,
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use mollie_parser::BlockExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<BlockExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        for statement in self.value.stmts {
            compiler.compile(fn_builder, statement)?;
        }

        Ok(if let Some(final_statement) = self.value.final_stmt {
            let stmt = final_statement.get_type(compiler)?;
            let value = compiler.compile(fn_builder, *final_statement)?;

            if let Some(structure) = stmt.variant.as_struct()
                && let ValueOrFunc::Value(ptr) = value
            {
                let mut values = Vec::new();

                for field in &structure.structure.fields {
                    values.push(fn_builder.ins().load(field.ty, ir::MemFlags::trusted(), ptr, field.offset));
                }

                ValueOrFunc::Values(values)
            } else if matches!(stmt.variant, TypeVariant::Primitive(PrimitiveType::String))
                && let ValueOrFunc::Value(ptr) = value
            {
                let size_ty = compiler.jit.module.isa().pointer_type();

                ValueOrFunc::Values(vec![
                    fn_builder.ins().load(size_ty, ir::MemFlags::trusted(), ptr, 0),
                    fn_builder.ins().load(size_ty, ir::MemFlags::trusted(), ptr, size_ty.bytes().cast_signed()),
                ])
            } else {
                value
            }
        } else {
            ValueOrFunc::Nothing
        })
    }
}

impl GetType for BlockExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.final_stmt
            .as_ref()
            .map_or_else(|| Ok(TypeVariant::void().into()), |final_statement| final_statement.get_type(compiler))
    }
}

impl GetNewType for BlockExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        let expected = type_solver.add_info(mollie_typing::TypeInfo::Type(core_types.void));

        type_solver.push_frame();

        for statement in &self.stmts {
            let statement = statement.get_new_type(compiler, core_types, type_storage, type_solver)?;

            type_solver.unify(expected, statement);
        }

        let result = self.final_stmt.as_ref().map_or(Ok(expected), |final_statement| {
            final_statement.get_new_type(compiler, core_types, type_storage, type_solver)
        })?;

        type_solver.pop_frame();

        Ok(result)
    }
}
