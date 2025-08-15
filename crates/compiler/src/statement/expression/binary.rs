use cranelift::prelude::{FunctionBuilder, InstBuilder, IntCC};
use mollie_parser::BinaryExpr;
use mollie_shared::{Operator, Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

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
        if matches!(self.operator.value, Operator::Equal | Operator::NotEqual) {
            Ok(TypeVariant::boolean().into())
        } else {
            self.lhs.get_type(compiler)
        }
    }
}
