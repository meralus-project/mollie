use cranelift::{
    codegen::ir::SourceLoc,
    module::Module,
    prelude::{AbiParam, FunctionBuilder, InstBuilder},
};
use mollie_ir::FatPtr;
use mollie_parser::FuncCallExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<FuncCallExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.value.function.get_type(compiler)?;

        if let Some(function) = ty.variant.as_function() {
            let v = compiler.compile(fn_builder, *self.value.function)?;

            let this_ty = compiler.this_ty.take();
            let this = compiler.this.take_if(|this| matches!(this, ValueOrFunc::Value(_)));

            if self.value.args.value.len() + usize::from(this.is_some()) != function.args.len() + usize::from(function.this.is_some()) {
                return Err(TypeError::InvalidArguments {
                    got: self.value.args.value.len() + usize::from(this.is_some()),
                    expected: function.args.len() + usize::from(function.this.is_some()),
                }
                .into());
            }

            let mut args = Vec::new();

            compiler.generics = ty.applied_generics;

            if function.this.is_some()
                && let Some(ValueOrFunc::Value(this)) = this
            {
                args.push(this);
            }

            for (arg, expected) in self.value.args.value.into_iter().zip(function.args.iter()) {
                let old = compiler.infer.replace(expected.clone());
                let got = arg.get_type(compiler)?;

                if !got.variant.same_as(&expected.variant, &compiler.generics) {
                    return Err(TypeError::Unexpected {
                        got: Box::new(got.kind()),
                        expected: Box::new(expected.kind()),
                    }
                    .into());
                }

                let value = compiler.compile(fn_builder, arg.clone())?;

                if old.is_some() {
                    compiler.infer = old;
                } else if compiler.infer.is_some() {
                    compiler.infer.take();
                }

                if let ValueOrFunc::Value(value) = value {
                    args.push(value);
                } else {
                    panic!("received incorrect value for argument: {arg:?} {value:?}");
                }
            }

            compiler.generics = Vec::new();

            let value = if let ValueOrFunc::FuncRef(func) = v {
                let result = fn_builder.ins().call(func, &args);

                fn_builder.func.stencil.srclocs[result].expand(SourceLoc::new(self.span.line.try_into()?));

                let results = fn_builder.inst_results(result);

                if results.len() > 1 {
                    ValueOrFunc::Values(results.to_vec())
                } else {
                    results.first().copied().map_or(ValueOrFunc::Nothing, ValueOrFunc::Value)
                }
            } else if let ValueOrFunc::ExtFunc(_, func_addr) | ValueOrFunc::Value(func_addr) = v {
                let mut signature = compiler.jit.module.make_signature();

                if let Some(this_ty) = this_ty {
                    signature.params.push(this_ty.variant.as_abi_param(compiler.jit.module.isa()));
                }

                for arg in &function.args {
                    signature.params.push(arg.variant.as_abi_param(compiler.jit.module.isa()));
                }

                if let Some(structure) = function.returns.variant.as_struct() {
                    for field in &structure.structure.fields {
                        signature.returns.push(AbiParam::new(field.ty));
                    }
                } else if matches!(function.returns.variant, TypeVariant::Primitive(PrimitiveType::String)) {
                    signature.returns.push(function.returns.variant.as_abi_param(compiler.jit.module.isa()));
                    signature.returns.push(function.returns.variant.as_abi_param(compiler.jit.module.isa()));
                } else if !matches!(function.returns.variant, TypeVariant::Primitive(PrimitiveType::Void)) {
                    signature.returns.push(function.returns.variant.as_abi_param(compiler.jit.module.isa()));
                }

                let sig = fn_builder.import_signature(signature);

                let result = fn_builder.ins().call_indirect(sig, func_addr, &args);
                let results = fn_builder.inst_results(result);

                if results.len() > 1 {
                    ValueOrFunc::Values(results.to_vec())
                } else {
                    results.first().copied().map_or(ValueOrFunc::Nothing, ValueOrFunc::Value)
                }
            } else {
                ValueOrFunc::Nothing
            };

            if let Some(structure) = function.returns.variant.as_struct()
                && let ValueOrFunc::Values(values) = value
            {
                Ok(ValueOrFunc::Value(structure.structure.instance(compiler.jit.module.isa(), fn_builder, values)))
            } else if matches!(function.returns.variant, TypeVariant::Primitive(PrimitiveType::String))
                && let ValueOrFunc::Values(values) = value
            {
                Ok(ValueOrFunc::Value(FatPtr::new(compiler.jit.module.isa(), fn_builder, values[0], values[1])))
            } else {
                Ok(value)
            }
        } else {
            Err(TypeError::Unexpected {
                got: Box::new(ty.kind()),
                expected: Box::new(TypeKind::Function.into()),
            }
            .into())
        }
    }
}

impl GetType for FuncCallExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        let ty = self.function.get_type(compiler)?;

        if let Some(function) = ty.variant.as_function() {
            Ok(function.returns.clone().resolve_type(&ty.applied_generics))
        } else {
            Err(TypeError::Unexpected {
                got: Box::new(ty.kind()),
                expected: Box::new(TypeKind::Function.into()),
            })
        }
    }
}
