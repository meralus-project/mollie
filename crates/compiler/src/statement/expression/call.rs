use cranelift::prelude::{FunctionBuilder, InstBuilder};
use mollie_parser::FuncCallExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeKind;

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
            if self.value.args.value.len() != function.args.len().max(usize::from(function.have_self)) - usize::from(function.have_self) {
                return Err(TypeError::InvalidArguments {
                    got: self.value.args.value.len(),
                    expected: function.args.len() - usize::from(function.have_self),
                }
                .into());
            }

            let v = compiler.compile(fn_builder, *self.value.function)?;
            let mut args = Vec::new();

            compiler.generics = ty.applied_generics;

            if function.have_self
                && let Some(ValueOrFunc::Value(this)) = compiler.this.take_if(|this| matches!(this, ValueOrFunc::Value(_)))
            {
                args.push(this);
            }

            for (arg, expected) in self
                .value
                .args
                .value
                .into_iter()
                .zip(function.args.iter().skip(usize::from(function.have_self)))
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
                }
            }
            // }

            compiler.generics = Vec::new();

            if let ValueOrFunc::Func(func) = v {
                let result = fn_builder.ins().call(func, &args);

                Ok(fn_builder
                    .inst_results(result)
                    .first()
                    .copied()
                    .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value))
            } else if let ValueOrFunc::ExtFunc(sig, func_addr) = v {
                let result = fn_builder.ins().call_indirect(sig, func_addr, &args);
                // let result = fn_builder.ins().call(ir::FuncRef::from_u32(1), &args);

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
