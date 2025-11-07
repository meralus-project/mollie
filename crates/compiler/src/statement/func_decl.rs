use cranelift::{
    module::{Linkage, Module},
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder},
};
use mollie_parser::FuncDecl;
use mollie_shared::Positioned;
use mollie_typing::{ComplexType, FunctionType, PrimitiveType, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, ValueOrFunc};

impl Compile for Positioned<FuncDecl> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult {
        let values = std::mem::take(&mut compiler.values);
        // let args = usize::from(self.value.this.is_some()) + self.value.args.len();
        let mut signature = compiler.jit.module.make_signature();
        let mut args = Vec::new();
        let mut returns = Box::new(TypeVariant::void().into());

        for arg in &self.value.args {
            let ty = arg.value.ty.get_type(compiler)?;

            signature.params.push(AbiParam::new(ty.variant.as_ir_type(compiler.jit.module.isa())));

            compiler.var_ty(&arg.value.name.value.0, ty.clone());
            args.push(ty);
        }

        if let Some(returns_ty) = self.value.returns {
            let ty = returns_ty.get_type(compiler)?;

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

            returns = Box::new(ty);
        }

        let ty = Type {
            variant: TypeVariant::complex(ComplexType::Function(FunctionType {
                is_native: false,
                this: None,
                args,
                returns,
            })),
            applied_generics: Vec::new(),
            declared_at: Some(self.span),
        };

        let mut ctx = compiler.jit.module.make_context();
        let func_id = compiler
            .jit
            .module
            .declare_function(
                &self.value.name.value.0,
                if self.value.func_vis.is_some() { Linkage::Export } else { Linkage::Local },
                &signature,
            )
            .unwrap();

        let func_name = self.value.name.value.0.clone();

        compiler.add_declared_type(&self.value.name.value.0, ty);
        compiler.func_names.insert(func_id, self.value.name.value.0);

        ctx.func.signature = signature.clone();

        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        for (index, arg) in self.value.args.iter().enumerate() {
            let value = fn_builder.block_params(entry_block)[index];
            let ty = fn_builder.func.signature.params[index].value_type;

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

        compiler.values = values;
        compiler.globals.insert(func_name, func_id);

        Ok(())
    }
}
