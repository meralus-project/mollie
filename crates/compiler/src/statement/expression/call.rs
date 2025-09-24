use cranelift::{
    codegen::ir::SourceLoc,
    module::Module,
    prelude::{AbiParam, FunctionBuilder, InstBuilder},
};
use mollie_parser::FuncCallExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<FuncCallExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.value.function.get_type(compiler)?;

        if let Some(function) = ty.variant.as_function() {
            // if function.have_self {
            //     if self.value.args.value.len() != function.args.len() - 1 {
            //         return Err(TypeError::InvalidArguments {
            //             got: self.value.args.value.len(),
            //             expected: function.args.len() - 1,
            //         }
            //         .into());
            //     }

            //     let v = compiler.compile(fn_builder, *self.value.function)?;
            //     let args = Vec::new();

            //     compiler.generics = ty.applied_generics;

            //     for (arg, expected) in
            // self.value.args.value.into_iter().zip(function.args.iter().skip(1)) {
            //         let got = arg.get_type(compiler)?;

            //         if !got.variant.same_as(&expected.variant, &compiler.generics) {
            //             return Err(TypeError::Unexpected {
            //                 got: Box::new(got.kind()),
            //                 expected: Box::new(expected.kind()),
            //             }
            //             .into());
            //         }

            //         if let ValueOrFunc::Value(value) = compiler.compile(chunk,
            // fn_builder, arg)? {             args.push(value);
            //         }
            //     }

            //     v
            // } else {
            let v = compiler.compile(fn_builder, *self.value.function)?;

            let this_ty = compiler.this_ty.take();
            let this = compiler.this.take_if(|this| {
                println!("this: {this:?}");

                matches!(this, ValueOrFunc::Value(_))
            });

            println!(
                "call: {}, have self? {}. this: {this:?}",
                function.args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "),
                function.have_self
            );

            if self.value.args.value.len() + usize::from(this.is_some() && function.is_native) != function.args.len() {
                return Err(TypeError::InvalidArguments {
                    got: self.value.args.value.len() + usize::from(this.is_some() && function.is_native),
                    expected: function.args.len(),
                }
                .into());
            }

            let mut args = Vec::new();

            compiler.generics = ty.applied_generics;

            if function.have_self
                && let Some(ValueOrFunc::Value(this)) = this
            {
                args.push(this);
            }

            println!("{:?}", self.value.args.value);
            println!("{:?}", function.args);

            for (arg, expected) in self
                .value
                .args
                .value
                .into_iter()
                .zip(function.args.iter().skip(usize::from(function.have_self && function.is_native)))
            {
                let got = arg.get_type(compiler)?;

                if !got.variant.same_as(&expected.variant, &compiler.generics) {
                    return Err(TypeError::Unexpected {
                        got: Box::new(got.kind()),
                        expected: Box::new(expected.kind()),
                    }
                    .into());
                }

                if let ValueOrFunc::Value(v) = compiler.compile(fn_builder, arg)? {
                    args.push(v);
                } else {
                    panic!("AAA WE GOT WRNG");
                }
            }
            // }

            println!("{args:?}");

            compiler.generics = Vec::new();

            if let ValueOrFunc::Func(func) = v {
                println!("funcref: {func}");
                let result = fn_builder.ins().call(func, &args);

                fn_builder.func.stencil.srclocs[result].expand(SourceLoc::new(self.span.line.try_into()?));

                Ok(fn_builder
                    .inst_results(result)
                    .first()
                    .copied()
                    .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value))
            } else if let ValueOrFunc::ExtFunc(_, func_addr) = v {
                println!("extfunc: {func_addr}");

                let mut signature = compiler.jit.module.make_signature();

                if let Some(this_ty) = this_ty {
                    signature.params.push(AbiParam::new(this_ty.variant.as_ir_type(compiler.jit.module.isa())));
                }

                for arg in &function.args {
                    signature.params.push(AbiParam::new(arg.variant.as_ir_type(compiler.jit.module.isa())));
                }

                if !matches!(function.returns.variant, TypeVariant::Primitive(PrimitiveType::Void)) {
                    signature
                        .returns
                        .push(AbiParam::new(function.returns.variant.as_ir_type(compiler.jit.module.isa())));
                }

                let sig = fn_builder.import_signature(signature);

                let result = fn_builder.ins().call_indirect(sig, func_addr, &args);

                Ok(fn_builder
                    .inst_results(result)
                    .first()
                    .copied()
                    .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value))
            } else {
                panic!("DA ALO")
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
