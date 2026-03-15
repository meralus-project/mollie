use cranelift::{
    codegen::ir,
    module::Module,
    prelude::{FloatCC, InstBuilder, IntCC},
};
use mollie_shared::Operator;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{PrimitiveType, Type, TypeRef};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn bin_op(&mut self, lhs: ir::Value, lhs_ty: TypeRef, operator: Operator, rhs: ir::Value, rhs_ty: TypeRef) -> ir::Value {
        let lhs_type = &self.type_context.type_context.types[lhs_ty];
        let rhs_type = &self.type_context.type_context.types[rhs_ty];

        if matches!(
            (lhs_type, rhs_type),
            (Type::Primitive(PrimitiveType::UInt(_)), Type::Primitive(PrimitiveType::UInt(_)))
        ) {
            match operator {
                Operator::Add => self.fn_builder.ins().uadd_overflow(lhs, rhs).0,
                Operator::Sub => self.fn_builder.ins().usub_overflow(lhs, rhs).0,
                Operator::Mul => self.fn_builder.ins().umul_overflow(lhs, rhs).0,
                Operator::Div => self.fn_builder.ins().udiv(lhs, rhs),
                Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                Operator::LessThan => self.fn_builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                Operator::LessThanEqual => self.fn_builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs),
                Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                Operator::GreaterThanEqual => self.fn_builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs),
                Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                _ => unimplemented!(),
            }
        } else if matches!(
            (lhs_type, rhs_type),
            (Type::Primitive(PrimitiveType::Int(_)), Type::Primitive(PrimitiveType::Int(_)))
        ) {
            match operator {
                Operator::Add => self.fn_builder.ins().iadd(lhs, rhs),
                Operator::Sub => self.fn_builder.ins().isub(lhs, rhs),
                Operator::Mul => self.fn_builder.ins().imul(lhs, rhs),
                Operator::Div => self.fn_builder.ins().sdiv(lhs, rhs),
                Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                Operator::LessThan => self.fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                Operator::LessThanEqual => self.fn_builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
                Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                Operator::GreaterThanEqual => self.fn_builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
                Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                _ => unimplemented!(),
            }
        } else if matches!(lhs_type, Type::Primitive(PrimitiveType::F32)) && matches!(rhs_type, Type::Primitive(PrimitiveType::F32)) {
            match operator {
                Operator::Add => self.fn_builder.ins().fadd(lhs, rhs),
                Operator::Sub => self.fn_builder.ins().fsub(lhs, rhs),
                Operator::Mul => self.fn_builder.ins().fmul(lhs, rhs),
                Operator::Div => self.fn_builder.ins().fdiv(lhs, rhs),
                Operator::Equal => self.fn_builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                Operator::NotEqual => self.fn_builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                Operator::LessThan => self.fn_builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                Operator::LessThanEqual => self.fn_builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs),
                Operator::GreaterThan => self.fn_builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                Operator::GreaterThanEqual => self.fn_builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs),
                Operator::BitAnd => self.fn_builder.ins().band(lhs, rhs),
                Operator::BitOr => self.fn_builder.ins().bor(lhs, rhs),
                _ => unimplemented!(),
            }
        } else {
            tracing::warn!(
                lhs = %self.type_context.type_context.display_of(lhs_ty),
                operator = %operator,
                rhs = %self.type_context.type_context.display_of(rhs_ty),
                "unknown binary operation, falling back to integer math"
            );

            match operator {
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
                operator => unimplemented!(
                    "{} {operator} {}",
                    self.type_context.type_context.display_of(lhs_ty),
                    self.type_context.type_context.display_of(rhs_ty)
                ),
            }
        }
    }

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
        if matches!(
            operator,
            Operator::Assign
                | Operator::AddAssign
                | Operator::SubAssign
                | Operator::MulAssign
                | Operator::DivAssign
                | Operator::BitAndAssign
                | Operator::BitOrAssign
        ) {
            let old = self.assign_ref.replace((lhs_ref, operator, rhs_ref));
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

            let lhs_ty = ast[lhs_ref].ty;
            let rhs_ty = ast[rhs_ref].ty;

            if let (&MolValue::Value(lhs), &MolValue::Value(rhs)) = (&lhs, &rhs) {
                Ok(MolValue::Value(self.bin_op(lhs, lhs_ty, operator, rhs, rhs_ty)))
            } else {
                unimplemented!("{lhs:?} {operator} {rhs:?}")
            }
        }
    }
}
