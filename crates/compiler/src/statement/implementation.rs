use std::mem;

use cranelift::{
    codegen::ir::{self, FuncRef, SigRef},
    module::{FuncId, Linkage, Module},
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder},
};
use indexmap::{IndexMap, map::Entry};
use mollie_parser::{Impl, ImplFunction};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ComplexType, FunctionType, PrimitiveType, Type, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<(SigRef, FuncRef, FuncId)> for Positioned<ImplFunction> {
    fn compile(self, compiler: &mut Compiler, original_fn_builder: &mut FunctionBuilder) -> CompileResult<(SigRef, FuncRef, FuncId)> {
        let values = mem::take(&mut compiler.values);
        // let args = usize::from(self.value.this.is_some()) + self.value.args.len();
        let mut signature = compiler.jit.module.make_signature();
        let this = compiler.get_var("self").unwrap().ty;

        if self.value.this.is_some() {
            signature.params.push(AbiParam::new(compiler.jit.module.isa().pointer_type()));
        }

        for arg in &self.value.args {
            let ty = arg.value.ty.get_type(compiler)?;

            signature.params.push(AbiParam::new(ty.variant.as_ir_type(compiler.jit.module.isa())));

            compiler.var_ty(&arg.value.name.value.0, ty);
        }

        if let Some(returns) = self.value.returns {
            let ty = returns.get_type(compiler)?;

            if let Some(structure) = ty.variant.as_struct() {
                for field in &structure.structure.fields {
                    signature.returns.push(AbiParam::new(field.ty));
                }
            } else if matches!(ty.variant, TypeVariant::Primitive(PrimitiveType::String)) {
                signature.returns.push(AbiParam::new(ty.variant.as_ir_type(compiler.jit.module.isa())));
                signature.returns.push(AbiParam::new(ty.variant.as_ir_type(compiler.jit.module.isa())));
            } else {
                signature.returns.push(AbiParam::new(ty.variant.as_ir_type(compiler.jit.module.isa())));
            }
        }

        let func_name = format!("{}>{}", this, self.value.name.value.0);

        let mut ctx = compiler.jit.module.make_context();
        let func_id = compiler.jit.module.declare_function(&func_name, Linkage::Local, &signature).unwrap();

        compiler.func_names.insert(func_id, self.value.name.value.0);

        ctx.func.signature = signature.clone();

        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        if self.value.this.is_some() {
            let value = fn_builder.block_params(entry_block)[0];
            let ty = fn_builder.func.signature.params[0].value_type;

            let var = fn_builder.declare_var(ty);

            fn_builder.def_var(var, value);

            compiler.variables.insert(String::from("self"), var);
        }

        for (index, arg) in self.value.args.iter().enumerate() {
            let value = fn_builder.block_params(entry_block)[index + usize::from(self.value.this.is_some())];
            let ty = fn_builder.func.signature.params[index + usize::from(self.value.this.is_some())].value_type;

            let var = fn_builder.declare_var(ty);

            fn_builder.def_var(var, value);

            compiler.variables.insert(arg.value.name.value.0.clone(), var);
        }

        let result = compiler.compile(&mut fn_builder, self.value.body)?;

        match result {
            ValueOrFunc::Value(value) => fn_builder.ins().return_(&[value]),
            ValueOrFunc::Values(values) => fn_builder.ins().return_(&values),
            _ => fn_builder.ins().return_(&[]),
        };

        compiler.jit.module.define_function(func_id, &mut ctx).unwrap();
        compiler.jit.module.clear_context(&mut ctx);
        compiler.jit.module.finalize_definitions().unwrap();

        for arg in self.value.args {
            compiler.remove_var(&arg.value.name.value.0);
            compiler.variables.shift_remove(&arg.value.name.value.0);
        }

        let signature = original_fn_builder.import_signature(signature);

        if self.value.this.is_some() {
            compiler.variables.shift_remove("self");
        }

        let func = compiler.jit.module.declare_func_in_func(func_id, original_fn_builder.func);

        compiler.values = values;

        Ok((signature, func, func_id))
    }
}

impl GetType for ImplFunction {
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult {
        let this = compiler.get_local_type("self").ok();

        let mut args = Vec::new();

        for arg in &self.args {
            args.push(arg.value.ty.get_type(compiler)?);
        }

        let returns = self
            .returns
            .as_ref()
            .map_or_else(|| Ok(TypeVariant::void().into()), |returns| returns.get_type(compiler))?;

        Ok(Type {
            applied_generics: Vec::new(),
            variant: TypeVariant::complex(ComplexType::Function(FunctionType {
                is_native: false,
                this,
                args,
                returns: Box::new(returns),
            })),
            declared_at: Some(span),
        })
    }
}

impl Compile for Positioned<Impl> {
    #[allow(clippy::too_many_lines)]
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult {
        for (index, name) in self.value.generics.iter().enumerate() {
            compiler.add_type(&name.value.0, TypeVariant::Generic(index));
        }

        let target_ty = self.value.target.get_type(compiler)?;
        let mut trait_index = None;

        if let Some(trait_name) = self.value.trait_name {
            let r#trait = compiler
                .traits
                .get_index_of(&trait_name.value.name.value.0)
                .ok_or_else(|| TypeError::NotFound {
                    ty: Some(Box::new(TypeKind::Trait)),
                    name: trait_name.value.name.value.0.clone(),
                })?;

            trait_index.replace(r#trait);

            if !trait_name.value.generics.is_empty() {
                compiler.generics = trait_name.value.generics.iter().map(|g| g.get_type(compiler)).collect::<TypeResult<Vec<_>>>()?;
            }

            for function in compiler.traits[r#trait].functions.clone() {
                if let Some(func) = self.value.functions.value.iter().find(|func| func.value.name.value.0 == function.name) {
                    if function.args.len() != func.value.args.len() {
                        return Err(TypeError::FunctionDefinitionInvalid { name: function.name.clone() }.into());
                    }

                    if function.this && func.value.this.is_none() || !function.this && func.value.this.is_some() {
                        return Err(TypeError::FunctionDefinitionInvalid { name: function.name.clone() }.into());
                    }

                    for (got, expected) in func.value.args.iter().zip(&function.args) {
                        let ty = got.value.ty.get_type(compiler)?;

                        if !ty.variant.same_as(&expected.variant, &compiler.generics) {
                            return Err(TypeError::InvalidArgumentType {
                                got: Box::new(ty.resolved_kind(&compiler.generics)),
                                expected: Box::new(expected.resolved_kind(&compiler.generics)),
                            }
                            .into());
                        }
                    }

                    let returns = func
                        .value
                        .returns
                        .as_ref()
                        .map_or_else(|| Ok(TypeVariant::void().into()), |r| r.get_type(compiler))?;

                    if !returns.variant.same_as(&function.returns.variant, &compiler.generics) {
                        return Err(TypeError::Unexpected {
                            got: Box::new(returns.resolved_kind(&compiler.generics)),
                            expected: Box::new(function.returns.resolved_kind(&compiler.generics)),
                        }
                        .into());
                    }
                } else {
                    return Err(TypeError::TraitFunctionNotFound {
                        trait_name: trait_name.value.name.value.0,
                        name: function.name.clone(),
                    }
                    .into());
                }
            }

            if !trait_name.value.generics.is_empty() {
                compiler.generics = Vec::new();
            }

            for function in &self.value.functions.value {
                if !compiler.traits[r#trait].functions.iter().any(|func| func.name == function.value.name.value.0) {
                    return Err(TypeError::UnknownTraitFunction {
                        trait_name: trait_name.value.name.value.0,
                        name: function.value.name.value.0.clone(),
                    }
                    .into());
                }
            }
        }

        let functions: IndexMap<String, (Type, (SigRef, FuncRef, FuncId))> = self
            .value
            .functions
            .value
            .into_iter()
            .map(|function| {
                let name = function.value.name.value.0.clone();
                let have_self = function.value.this.is_some();

                // chunk.push_frame();
                compiler.push_frame();

                if have_self {
                    // let ty = compiler.idx_of_type(&target_ty).unwrap_or_else(|| panic!("there's
                    // no type idx in compiler for {target_ty}"));

                    compiler.var_ty("self", target_ty.clone());
                }

                let ty = function.get_type(compiler)?;
                let value = compiler.compile(fn_builder, function)?;

                if have_self {
                    compiler.remove_var("self");
                }

                compiler.pop_frame();
                // chunk.pop_frame();

                Ok((name, (ty, value)))
            })
            .collect::<CompileResult<_>>()?;

        for name in &self.value.generics {
            compiler.remove_type(&name.value.0);
        }

        if let Some(trait_index) = trait_index {
            match compiler.impls.entry(target_ty.variant.clone()) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(trait_index);
                }
                Entry::Vacant(entry) => {
                    entry.insert(vec![trait_index]);
                }
            }
        }

        let size_t = compiler.jit.module.isa().pointer_type();
        let data_size = usize::try_from(size_t.bytes())? * (functions.len() + 1);
        let mut data = vec![0; data_size];

        data[0..usize::try_from(size_t.bytes())?].copy_from_slice(&{
            let value = compiler.vtables.get_index_of(&target_ty.variant).unwrap_or_else(|| compiler.vtables.len());

            match compiler.jit.module.isa().endianness() {
                ir::Endianness::Little => value.to_le_bytes(),
                ir::Endianness::Big => value.to_be_bytes(),
            }
        });

        compiler.jit.data_desc.define(data.into_boxed_slice());

        for (i, (_, (_, _, func_id))) in functions.values().enumerate() {
            let func_ref = compiler.jit.module.declare_func_in_data(*func_id, &mut compiler.jit.data_desc);

            compiler.jit.data_desc.write_function_addr(size_t.bytes() * (u32::try_from(i)? + 1), func_ref);
        }

        let id = compiler.jit.module.declare_anonymous_data(true, false).unwrap();

        compiler.jit.module.define_data(id, &compiler.jit.data_desc).unwrap();
        compiler.jit.data_desc.clear();
        compiler.jit.module.finalize_definitions().unwrap();

        let functions = (id, functions);

        match compiler.vtables.entry(target_ty.variant) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(trait_index, functions);
            }
            Entry::Vacant(entry) => {
                entry.insert(IndexMap::from_iter([(trait_index, functions)]));
            }
        }

        Ok(())
    }
}
