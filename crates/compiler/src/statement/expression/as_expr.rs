use cranelift::{
    codegen::ir::Endianness,
    module::Module,
    prelude::{FunctionBuilder, InstBuilder, IntCC, MemFlags, types},
};
use mollie_parser::{IsExpr, IsPattern};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ComplexType, FatPtr, Type, TypeVariant, VTablePtr};

use crate::{Compile, CompileError, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IsPattern> {
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
                        let idx = compiler.impls.get_index_of(&name_ty.variant).unwrap().try_into()?;

                        for generic in ty.value.generics {
                            name_ty.applied_generics.push(generic.get_type(compiler)?);
                        }

                        (name_ty, idx)
                    };

                    let var = fn_builder.declare_var(name_ty.variant.as_ir_type(compiler.jit.module.isa()));

                    compiler.var(&name.value.0, name_ty);
                    compiler.variables.insert(name.value.0, var);

                    let size_t = compiler.jit.module.isa().pointer_type();
                    let vtable_ptr = FatPtr::get_metadata(compiler.jit.module.isa(), fn_builder, fat_ptr);
                    let type_idx = VTablePtr::get_type_idx(compiler.jit.module.isa(), fn_builder, vtable_ptr);
                    let target_type_idx = fn_builder.ins().iconst(size_t, idx);
                    let ptr = FatPtr::get_ptr(compiler.jit.module.isa(), fn_builder, fat_ptr);

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

                    println!("{ty:#?}");

                    let variant = ty
                        .variant
                        .as_enum()
                        .unwrap()
                        .variants
                        .iter()
                        .position(|variant| variant.0 == index.value.name.value.0)
                        .unwrap();

                    let ty_index = compiler
                        .types
                        .get_index_of(&target.value.0)
                        .ok_or(CompileError::VariableNotFound { name: target.value.0 })?;

                    // chunk.copy();
                    // chunk.is_instance_of(ty_index, variant);

                    if let Some(values) = values {
                        // for value in values.value {
                        //     let property = ty.variant.as_enum().unwrap().variants[variant]
                        //         .1
                        //         .properties
                        //         .as_ref()
                        //         .unwrap()
                        //         .iter()
                        //         .position(|property| property.0 == value.value.name.value.0)
                        //         .unwrap();

                        //     chunk.get_property(property);

                        //     if let Some(val) = value.value.value {
                        //         val.compile(compiler, fn_builder)?;
                        //     } else {
                        //         let id = compiler.var(
                        //             value.value.name.value.0,
                        //             
                        // ty.variant.as_enum().unwrap().variants[variant].1.properties.as_ref().
                        // unwrap()[property]                 .1
                        //                 .clone()
                        //                 .resolve_type(&ty.applied_generics),
                        //         );

                        //         chunk.set_local(id);

                        //         let constant = chunk.constant(mollie_vm::Value::Boolean(true));

                        //         chunk.load_const(constant);
                        //     }
                        // }

                        // chunk[start] = Inst::JumpIfFalse(chunk.len() - start + 1);

                        // chunk.jump(4);
                        // chunk.pop();
                        // chunk.pop();

                        // let constant = chunk.constant(mollie_vm::Value::Boolean(false));

                        // chunk.load_const(constant);

                        Ok(ValueOrFunc::Nothing)
                    } else {
                        Ok(ValueOrFunc::Nothing)
                    }
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

        println!("{}", fn_builder.func);

        compiler.infer.replace(ty);
        compiler.infer_val.replace(value);

        self.value.pattern.compile(compiler, fn_builder)?;

        compiler.infer.take();
        compiler.infer_val.take();

        if let ValueOrFunc::Value(value) = value {
            Ok(ValueOrFunc::Value(fn_builder.ins().bitcast(
                types::I64,
                MemFlags::new().with_endianness(Endianness::Little),
                value,
            )))
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
