use cranelift::{
    module::Module,
    prelude::{FloatCC, InstBuilder, IntCC},
};
use mollie_shared::Operator;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{PrimitiveType, Type};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    /// Compiles `lhs <operator> rhs` expression. Returns [`MolValue::Value`],
    /// or [`MolValue::Nothing`] if an assignment operation occurs.
    ///
    /// # Errors
    ///     
    /// Returns [`CompileError`] if there is a compilation error in `lhs` or
    /// `rhs`.
    ///
    /// [`CompileError`]: crate::error::CompileError
    pub fn compile_bin_expr(&mut self, ast: &TypedAST, lhs_ref: ExprRef, operator: Operator, rhs_ref: ExprRef) -> CompileResult<MolValue> {
        if matches!(operator, Operator::Assign) {
            let old = self.assign_ref.replace((lhs_ref, rhs_ref));
            let lhs = lhs_ref.compile(ast, self)?;

            if old.is_some() {
                self.assign_ref = old;
            } else if self.assign_ref.is_some() {
                self.assign_ref.take();
            }

            Ok(lhs)
        } else if matches!(operator, Operator::And | Operator::Or) {
            if let Some((true_branch, false_branch)) = self.branches {
                let rhs_block = self.fn_builder.create_block();

                if operator == Operator::And {
                    self.branches = Some((rhs_block, false_branch));

                    let lhs = lhs_ref.compile(ast, self)?;

                    if let MolValue::Value(lhs) = lhs {
                        self.fn_builder.ins().brif(lhs, rhs_block, &[], false_branch, &[]);
                    }
                } else {
                    self.branches = Some((true_branch, rhs_block));

                    let lhs = lhs_ref.compile(ast, self)?;

                    if let MolValue::Value(lhs) = lhs {
                        self.fn_builder.ins().brif(lhs, true_branch, &[], rhs_block, &[]);
                    }
                }

                self.fn_builder.switch_to_block(rhs_block);
                self.branches = Some((true_branch, false_branch));

                if let MolValue::Value(rhs) = rhs_ref.compile(ast, self)? {
                    self.fn_builder.ins().brif(rhs, true_branch, &[], false_branch, &[]);
                }

                self.fn_builder.seal_block(rhs_block);
            }

            Ok(MolValue::Nothing)
        } else {
            let lhs = lhs_ref.compile(ast, self)?;
            let rhs = rhs_ref.compile(ast, self)?;

            let lhs_ty = &self.type_context.type_context.types[ast[lhs_ref].ty];
            let rhs_ty = &self.type_context.type_context.types[ast[rhs_ref].ty];

            if let (&MolValue::Value(lhs), &MolValue::Value(rhs)) = (&lhs, &rhs) {
                if matches!(
                    (lhs_ty, rhs_ty),
                    (Type::Primitive(PrimitiveType::UInt(_)), Type::Primitive(PrimitiveType::UInt(_)))
                ) {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().uadd_overflow(lhs, rhs).0,
                        Operator::Sub => self.fn_builder.ins().usub_overflow(lhs, rhs).0,
                        Operator::Mul => self.fn_builder.ins().umul_overflow(lhs, rhs).0,
                        Operator::Div => self.fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                        Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                        Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if matches!(
                    (lhs_ty, rhs_ty),
                    (Type::Primitive(PrimitiveType::Int(_)), Type::Primitive(PrimitiveType::Int(_)))
                ) {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().sdiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                        Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if matches!(lhs_ty, Type::Primitive(PrimitiveType::Float)) && matches!(rhs_ty, Type::Primitive(PrimitiveType::Float)) {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().fadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().fsub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().fmul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().fdiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                        Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                        Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else {
                    tracing::warn!(
                        lhs = %self.type_context.type_context.display_of(ast[lhs_ref].ty),
                        operator = %operator,
                        rhs = %self.type_context.type_context.display_of(ast[rhs_ref].ty),
                        "unknown binary operation, falling back to integer math"
                    );

                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                        Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                        operator => unreachable!(
                            "{} {operator} {}",
                            self.type_context.type_context.display_of(ast[lhs_ref].ty),
                            self.type_context.type_context.display_of(ast[rhs_ref].ty)
                        ),
                    }))
                }
            } else {
                unimplemented!("{lhs:?} {operator} {rhs:?}")
            }
        }
    }
}
