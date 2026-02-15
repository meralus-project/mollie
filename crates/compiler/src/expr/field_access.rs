use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_index::Idx;
use mollie_ir::MollieType;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{AdtVariantRef, FieldRef, TypeInfo};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_field_access_expr(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, field: FieldRef) -> CompileResult<MolValue> {
        let v = target.compile(ast, self)?;

        if let MolValue::Value(v) = v {
            if let TypeInfo::Adt(..) = self.checker.solver.get_info2(ast[target].ty) {
                let hash = self.checker.solver.hash_of(ast[target].ty);
                let assign_value = match self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
                    Some((_, assign)) => Some(assign.compile(ast, self)?),
                    None => None,
                };

                let (field, _) = &self.compiler.get_adt_variant(hash, AdtVariantRef::ZERO).fields[field];

                Ok(match (field.ty, assign_value) {
                    (MollieType::Regular(ty), None) => MolValue::Value(self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset)),
                    (MollieType::Fat(ty, metadata_ty), None) => MolValue::FatPtr(
                        self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset),
                        self.fn_builder
                            .ins()
                            .load(metadata_ty, ir::MemFlags::trusted(), v, field.offset + ty.bytes().cast_signed()),
                    ),
                    (MollieType::Regular(_), Some(MolValue::Value(value))) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);

                        MolValue::Nothing
                    }
                    (MollieType::Fat(ty, _), Some(MolValue::FatPtr(value, metadata))) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, v, field.offset + ty.bytes().cast_signed());

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
