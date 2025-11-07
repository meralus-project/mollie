use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder, IntCC, MemFlags},
};
use mollie_ir::{FatPtr, VTablePtr};
use mollie_parser::{IsExpr, IsPattern};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ComplexType, Type, TypeVariant};

use crate::{Compile, CompileError, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IsPattern> {
    #[allow(clippy::too_many_lines)]
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        if let Some(ValueOrFunc::Value(fat_ptr)) = compiler.infer_val.take() {
            match self.value {
                IsPattern::Literal(literal) => {
                    let rhs = self.span.wrap(literal).compile(compiler, fn_builder)?;

                    Ok(ValueOrFunc::Value(fn_builder.ins().icmp(IntCC::Equal, fat_ptr, rhs)))
                }
                IsPattern::TypeName { ty, name } => {
                    let (name_ty, idx): (mollie_typing::Type, i64) = if let Some(trait_index) = compiler.traits.get_index_of(&ty.value.name.value.0) {
                        (
                            TypeVariant::complex(ComplexType::TraitInstance(compiler.infer.take().unwrap(), trait_index)).into(),
                            trait_index.try_into()?,
                        )
                    } else {
                        let mut name_ty = ty.value.name.get_type(compiler)?;
                        let idx = compiler.vtables.get_index_of(&name_ty.variant).unwrap().try_into()?;

                        for generic in ty.value.generics {
                            name_ty.applied_generics.push(generic.get_type(compiler)?);
                        }

                        (name_ty, idx)
                    };

                    let var = fn_builder.declare_var(name_ty.variant.as_ir_type(compiler.jit.module.isa()));
                    let ty_idx = compiler.idx_of_type(&name_ty).unwrap();

                    compiler.var(&name.value.0, ty_idx);

                    let size_t = compiler.jit.module.isa().pointer_type();
                    let vtable_ptr = FatPtr::get_metadata(compiler.jit.module.isa(), fn_builder, fat_ptr);
                    let type_idx = VTablePtr::get_type_idx(compiler.jit.module.isa(), fn_builder, vtable_ptr);
                    let target_type_idx = fn_builder.ins().iconst(size_t, idx);
                    let ptr = FatPtr::get_ptr(compiler.jit.module.isa(), fn_builder, fat_ptr);

                    compiler.variables.insert(name.value.0, var);
                    fn_builder.def_var(var, ptr);

                    Ok(ValueOrFunc::Value(fn_builder.ins().icmp(IntCC::Equal, type_idx, target_type_idx)))
                }
                IsPattern::Enum { target, index, values } => {
                    let ty = target.get_type(compiler)?;
                    let ty = Type {
                        variant: ty.variant,
                        applied_generics: if let Some(infer) = compiler.infer.take_if(|ty| !ty.applied_generics.is_empty()) {
                            infer.applied_generics
                        } else {
                            index.value.generics.iter().map(|ty| ty.get_type(compiler)).collect::<TypeResult<Vec<_>>>()?
                        },
                        declared_at: ty.declared_at,
                    };

                    let variant = ty
                        .variant
                        .as_enum()
                        .unwrap()
                        .variants
                        .iter()
                        .position(|variant| variant.0 == index.value.name.value.0)
                        .unwrap();

                    let _ty_index = compiler
                        .get_type_idx(&target.value.0)
                        .ok_or(CompileError::VariableNotFound { name: target.value.0 })?;

                    let size_t = compiler.jit.module.isa().pointer_type();
                    let variant_idx = fn_builder.ins().load(size_t, MemFlags::trusted(), fat_ptr, 0);
                    let target_variant_idx = fn_builder.ins().iconst(size_t, i64::try_from(variant)?);

                    let result = fn_builder.ins().icmp(IntCC::Equal, variant_idx, target_variant_idx);

                    let declaration_block = fn_builder.create_block();
                    let after_block = fn_builder.create_block();

                    fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                    fn_builder.switch_to_block(declaration_block);
                    fn_builder.seal_block(declaration_block);

                    if let Some(values) = values {
                        for value in values.value {
                            let property = ty.variant.as_enum().unwrap().variants[variant]
                                .1
                                .properties
                                .as_ref()
                                .unwrap()
                                .iter()
                                .position(|property| property.0 == value.value.name.value.0)
                                .unwrap();

                            // chunk.get_property(property);

                            if let Some(val) = value.value.value {
                                val.compile(compiler, fn_builder)?;
                            } else {
                                let name_ty = ty.variant.as_enum().unwrap().variants[variant].1.properties.as_ref().unwrap()[property]
                                    .1
                                    .clone()
                                    .resolve_type(&ty.applied_generics);

                                let var = fn_builder.declare_var(name_ty.variant.as_ir_type(compiler.jit.module.isa()));
                                let ty_idx = compiler.idx_of_type(&name_ty).unwrap();

                                compiler.var(&value.value.name.value.0, ty_idx);

                                let ptr = fn_builder.ins().load(
                                    ty.variant.as_enum().unwrap().variants[variant].1.structure.as_ref().unwrap().fields[property].ty,
                                    MemFlags::trusted(),
                                    fat_ptr,
                                    size_t.bytes().cast_signed()
                                        + ty.variant.as_enum().unwrap().variants[variant].1.structure.as_ref().unwrap().fields[property].offset,
                                );

                                compiler.variables.insert(value.value.name.value.0, var);
                                fn_builder.def_var(var, ptr);
                            }
                        }
                    }

                    fn_builder.ins().jump(after_block, &[]);

                    fn_builder.switch_to_block(after_block);
                    fn_builder.seal_block(after_block);

                    Ok(ValueOrFunc::Value(result))
                }
            }
        } else {
            Ok(ValueOrFunc::Nothing)
        }
    }
}

impl Compile<ValueOrFunc> for Positioned<IsExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.value.target.get_type(compiler)?;

        let value = self.value.target.compile(compiler, fn_builder)?;

        compiler.infer.replace(ty);
        compiler.infer_val.replace(value);

        let value = self.value.pattern.compile(compiler, fn_builder)?;

        compiler.infer.take();
        compiler.infer_val.take();

        if let ValueOrFunc::Value(value) = value {
            Ok(ValueOrFunc::Value(value))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for IsExpr {
    fn get_type(&self, _: &mut Compiler, _: Span) -> TypeResult {
        Ok(TypeVariant::boolean().into())
    }
}
