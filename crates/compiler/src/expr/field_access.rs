use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_index::Idx;
use mollie_ir::{Field, MollieType};
use mollie_shared::Operator;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{AdtVariantRef, FieldRef, Type};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_field_access_expr(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, field: FieldRef) -> CompileResult<MolValue> {
        let v = target.compile(ast, self)?;

        if let MolValue::Value(ptr) = v {
            if let Type::Adt(..) = self.type_context.type_context.types[ast[target].ty] {
                let hash = self.type_context.type_context.types.hash_of(ast[target].ty);
                let assign_value = match self.assign_ref.take_if(|(lhs_ref, ..)| *lhs_ref == expr) {
                    Some((_, operator, assign)) => Some((operator, assign.compile(ast, self)?, ast[assign].ty)),
                    None => None,
                };

                let &(Field { ty, offset, .. }, _) = &self.compiler.get_adt_variant(hash, AdtVariantRef::ZERO).fields[field];

                Ok(match (ty, assign_value) {
                    (MollieType::Regular(ty), None) => MolValue::Value(self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, offset)),
                    (MollieType::Fat(ty, metadata_ty), None) => MolValue::FatPtr(
                        self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, offset),
                        self.fn_builder
                            .ins()
                            .load(metadata_ty, ir::MemFlags::trusted(), ptr, offset + ty.bytes().cast_signed()),
                    ),
                    (MollieType::Regular(ty), Some((operator, MolValue::Value(value), assign_ty))) => {
                        let value = match operator {
                            mollie_shared::Operator::Assign => value,
                            op if let Some(operator) = op.lower() => {
                                let current_value = self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, offset);

                                self.bin_op(current_value, ast[expr].ty, operator, value, assign_ty)
                            }
                            _ => return Ok(MolValue::Nothing),
                        };

                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, offset);

                        MolValue::Nothing
                    }
                    (MollieType::Fat(ty, _), Some((Operator::Assign, MolValue::FatPtr(value, metadata), _))) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, offset);
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, ptr, offset + ty.bytes().cast_signed());

                        MolValue::Nothing
                    }
                    _ => unimplemented!(),
                })
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    }
}
