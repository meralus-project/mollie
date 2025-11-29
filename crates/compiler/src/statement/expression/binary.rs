use cranelift::prelude::{FloatCC, FunctionBuilder, InstBuilder, IntCC};
use mollie_parser::BinaryExpr;
use mollie_shared::{Operator, Positioned, Span};
use mollie_typing::{PrimitiveType, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<BinaryExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        if self.value.operator.value == Operator::Assign {
            let old = compiler.assign.replace(*self.value.rhs);

            let result = compiler.compile(fn_builder, *self.value.lhs)?;

            if old.is_some() {
                compiler.assign = old;
            } else if compiler.assign.is_some() {
                compiler.assign.take();
            }

            Ok(result)
        } else {
            let lhs_ty = self.value.lhs.get_type(compiler)?;
            let rhs_ty = self.value.rhs.get_type(compiler)?;

            let lhs = compiler.compile(fn_builder, *self.value.lhs)?;
            let rhs = compiler.compile(fn_builder, *self.value.rhs)?;

            if let (ValueOrFunc::Value(lhs), ValueOrFunc::Value(rhs)) = (lhs, rhs) {
                if lhs_ty.variant.is_unsigned_integer() && rhs_ty.variant.is_unsigned_integer() {
                    Ok(ValueOrFunc::Value(match self.value.operator.value {
                        Operator::Add => fn_builder.ins().uadd_overflow(lhs, rhs).0,
                        Operator::Sub => fn_builder.ins().usub_overflow(lhs, rhs).0,
                        Operator::Mul => fn_builder.ins().umul_overflow(lhs, rhs).0,
                        Operator::Div => fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => fn_builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                        Operator::GreaterThan => fn_builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if lhs_ty.variant.is_signed_integer() && rhs_ty.variant.is_signed_integer() {
                    Ok(ValueOrFunc::Value(match self.value.operator.value {
                        Operator::Add => fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => fn_builder.ins().sdiv(lhs, rhs),
                        Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if matches!(lhs_ty.variant, TypeVariant::Primitive(PrimitiveType::Float))
                    && matches!(rhs_ty.variant, TypeVariant::Primitive(PrimitiveType::Float))
                {
                    Ok(ValueOrFunc::Value(match self.value.operator.value {
                        Operator::Add => fn_builder.ins().fadd(lhs, rhs),
                        Operator::Sub => fn_builder.ins().fsub(lhs, rhs),
                        Operator::Mul => fn_builder.ins().fmul(lhs, rhs),
                        Operator::Div => fn_builder.ins().fdiv(lhs, rhs),
                        Operator::Equal => fn_builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                        Operator::NotEqual => fn_builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                        Operator::LessThan => fn_builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                        Operator::GreaterThan => fn_builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else {
                    Ok(ValueOrFunc::Value(match self.value.operator.value {
                        Operator::Add => fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                }
            } else {
                unimplemented!()
            }
        }
    }
}

impl GetType for BinaryExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        if matches!(
            self.operator.value,
            Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan
        ) {
            Ok(TypeVariant::boolean().into())
        } else {
            self.lhs.get_type(compiler)
        }
    }
}

impl GetNewType for BinaryExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        let expected = self.lhs.get_new_type(compiler, core_types, type_storage, type_solver)?;
        let got = self.rhs.get_new_type(compiler, core_types, type_storage, type_solver)?;

        type_solver.unify(expected, got);

        if matches!(
            self.operator.value,
            Operator::Equal | Operator::NotEqual | Operator::LessThan | Operator::GreaterThan
        ) {
            Ok(type_solver.add_info(mollie_typing::TypeInfo::Type(core_types.boolean)))
        } else {
            Ok(expected)
        }
    }
}

#[cfg(test)]
mod tests {
    use mollie_parser::{Expr, Parse};
    use mollie_typing::{CoreTypes, TypeSolver, TypeStorage, TypeVariant};

    use crate::{Compiler, GetNewPositionedType};

    #[test]
    fn test_binary_expr_solving() {
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

        let expr = Expr::parse_value("1int64 == 2").unwrap();
        let ty = expr.get_new_type(&mut compiler, &types, &mut storage, &mut solver);

        if let Ok(ty) = ty {
            let ty = solver.get_actual_type(ty).unwrap();

            println!("{:?}", solver.format_type(ty, &storage));
        }
    }
}
