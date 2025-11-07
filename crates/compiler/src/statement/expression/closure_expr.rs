use cranelift::{
    module::Module,
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder},
};
use mollie_parser::ClosureExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<ClosureExpr> {
    fn compile(self, compiler: &mut Compiler, original_fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = compiler.infer.clone().unwrap();

        if let Some(func) = ty.variant.as_function() {
            let values = std::mem::take(&mut compiler.values);
            let mut signature = compiler.jit.module.make_signature();

            for (name, arg) in self.value.args.value.iter().zip(&func.args) {
                signature.params.push(AbiParam::new(arg.variant.as_ir_type(compiler.jit.module.isa())));

                compiler.var_ty(&name.value.0, arg.clone());
            }

            if !matches!(func.returns.variant, TypeVariant::Primitive(PrimitiveType::Void)) {
                if let Some(structure) = func.returns.variant.as_struct() {
                    for field in &structure.structure.fields {
                        signature.returns.push(AbiParam::new(field.ty));
                    }
                } else if matches!(func.returns.variant, TypeVariant::Primitive(PrimitiveType::String)) {
                    signature.returns.push(AbiParam::new(func.returns.variant.as_ir_type(compiler.jit.module.isa())));
                    signature.returns.push(AbiParam::new(func.returns.variant.as_ir_type(compiler.jit.module.isa())));
                } else {
                    signature
                        .returns
                        .push(AbiParam::new(func.returns.variant.as_ir_type(compiler.jit.module.isa())));
                }
            }

            let mut ctx = compiler.jit.module.make_context();
            let func_id = compiler.jit.module.declare_anonymous_function(&signature).unwrap();

            ctx.func.signature = signature;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            for (index, arg) in self.value.args.value.iter().enumerate() {
                let value = fn_builder.block_params(entry_block)[index];
                let ty = fn_builder.func.signature.params[index].value_type;

                let var = fn_builder.declare_var(ty);

                fn_builder.def_var(var, value);

                compiler.variables.insert(arg.value.0.clone(), var);
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

            for arg in self.value.args.value {
                compiler.remove_var(&arg.value.0);
                compiler.variables.shift_remove(&arg.value.0);
            }

            compiler.values = values;

            let func = compiler.jit.module.declare_func_in_func(func_id, original_fn_builder.func);
            let ptr = original_fn_builder.ins().func_addr(compiler.jit.module.isa().pointer_type(), func);

            Ok(ValueOrFunc::Value(ptr))
        } else {
            Ok(ValueOrFunc::Nothing)
        }
    }
}

impl GetType for ClosureExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        Ok(compiler.infer.clone().unwrap())
    }
}
