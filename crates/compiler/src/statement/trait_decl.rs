use cranelift::{
    module::Module,
    prelude::{AbiParam, FunctionBuilder},
};
use mollie_parser::TraitDecl;
use mollie_shared::Positioned;
use mollie_typing::{Trait, TraitFunc, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType};

impl Compile for Positioned<TraitDecl> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult {
        for (index, name) in self.value.name.value.generics.iter().enumerate() {
            compiler.add_type(&name.value.0, TypeVariant::Generic(index));
        }

        let functions = self
            .value
            .functions
            .value
            .into_iter()
            .map(|function| {
                let mut args = Vec::new();

                // if function.value.this.is_some() {
                //     args.push(TypeVariant::This.into());
                // }

                for arg in function.value.args {
                    args.push(arg.value.ty.get_type(compiler)?);
                }

                let returns = if let Some(returns) = function.value.returns {
                    Some(returns.get_type(compiler)?)
                } else {
                    None
                };

                let mut signature = compiler.jit.module.make_signature();

                if function.value.this.is_some() {
                    signature.params.push(AbiParam::new(compiler.jit.module.isa().pointer_type()));
                }

                for arg in &args {
                    signature.params.push(AbiParam::new(arg.variant.as_ir_type(compiler.jit.module.isa())));
                }

                if let Some(returns) = &returns {
                    signature.returns.push(AbiParam::new(returns.variant.as_ir_type(compiler.jit.module.isa())));
                }

                let signature_ref = fn_builder.import_signature(signature.clone());

                Ok(TraitFunc {
                    signature,
                    signature_ref,
                    name: function.value.name.value.0,
                    this: function.value.this.is_some(),
                    args,
                    returns: returns.unwrap_or_else(|| TypeVariant::void().into()),
                })
            })
            .collect::<CompileResult<_>>()?;

        for name in &self.value.name.value.generics {
            compiler.remove_type(&name.value.0);
        }

        let (index, _) = compiler.traits.insert_full(self.value.name.value.name.value.0.clone(), Trait {
            generics: self.value.name.value.generics.into_iter().map(|v| v.value.0).collect(),
            functions,
            declared_at: Some(self.span),
        });

        compiler.add_declared_type(self.value.name.value.name.value.0, Type {
            variant: TypeVariant::Trait(index),
            applied_generics: Vec::new(),
            declared_at: Some(self.span),
        });

        Ok(())
    }
}
