use cranelift::{module::Module, prelude::InstBuilder};
use mollie_index::Idx;
use mollie_ir::{MollieType, compile_constant};
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{AdtKind, AdtVariantRef, FieldRef, TypeInfo, TypeInfoRef};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_construct(
        &mut self,
        ast: &TypedAST,
        ty: TypeInfoRef,
        variant: AdtVariantRef,
        fields: &[(FieldRef, String, Option<ExprRef>)],
    ) -> CompileResult<MolValue> {
        let mut values = Vec::new();
        let hash = self.hash_of(ty);
        let &TypeInfo::Adt(adt_ref, adt_kind, ..) = self.checker.solver.get_info(ty) else {
            unreachable!()
        };

        let is_enum = matches!(adt_kind, AdtKind::Enum);

        if is_enum {
            values.push(self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index() as i64));
        }

        for (field_ref, (field, _)) in self.compiler.get_adt_variant(hash, variant).fields.clone().into_iter() {
            if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                if is_enum && field_ref.index() == 0 {
                    continue;
                }

                let (got_ty, got) = match property.2 {
                    Some(expr) => (Some(ast[expr].ty), expr.compile(ast, self)?),
                    None => match &field.default_value {
                        Some(value) => (
                            None,
                            compile_constant(
                                value,
                                &mut self.compiler.codegen.module,
                                &mut self.compiler.codegen.data_desc,
                                &mut self.fn_builder,
                            )
                            .map_or(MolValue::Nothing, MolValue::Value),
                        ),
                        None => panic!(
                            "can't compile {}: {field:?} => no value",
                            self.checker.adt_types[adt_ref].variants[variant].fields[field_ref].0
                        ),
                    },
                };

                match (field.ty, &got) {
                    (MollieType::Regular(ty), &MolValue::Value(v)) => {
                        assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                        values.push(v);
                    }
                    (MollieType::Fat(ty, meta), got) => match *got {
                        MolValue::Value(v) => {
                            if let Some(got_ty) = got_ty {
                                assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                let hash = self.checker.solver.hash_of(got_ty);
                                let metadata = self.fn_builder.ins().iconst(meta, hash.cast_signed());

                                values.push(v);
                                values.push(metadata);
                            }
                        }
                        MolValue::FatPtr(v, m) => {
                            assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                            assert_eq!(meta, self.fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                            values.push(v);
                            values.push(m);
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        }

        Ok(MolValue::Value(self.construct(ty, variant, &values)?))
    }
}
