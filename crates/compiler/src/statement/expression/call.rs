use cranelift::{
    codegen::ir::SourceLoc,
    module::Module,
    prelude::{AbiParam, FunctionBuilder, InstBuilder},
};
use mollie_ir::FatPtr;
use mollie_parser::FuncCallExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{PrimitiveType, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

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

impl GetNewType for FuncCallExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        let expected = self.function.get_new_type(compiler, core_types, type_storage, type_solver)?;
        let args = self
            .args
            .value
            .iter()
            .map(|arg| arg.get_new_type(compiler, core_types, type_storage, type_solver))
            .collect::<TypeResult<Vec<_>>>()?;

        let output = type_solver.add_info(mollie_typing::TypeInfo::Unknown(None));
        let got = type_solver.add_info(mollie_typing::TypeInfo::Func(args, output));

        type_solver.unify(expected, got);

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use mollie_parser::{Expr, Parse, Stmt};
    use mollie_typing::{CoreTypes, TypeSolver, TypeStorage, TypeVariant};

    use crate::{Compiler, GetNewPositionedType};

    #[test]
    fn test_call_expr_solving() {
        let mut storage = TypeStorage::default();
        let mut solver = TypeSolver::default();

        let types = CoreTypes {
            void: storage.add_type(TypeVariant::void()),
            any: storage.add_type(TypeVariant::any()),
            boolean: storage.add_named_type("boolean", TypeVariant::boolean()),
            int8: storage.add_named_type("int8", TypeVariant::int8()),
            int16: storage.add_named_type("int16", TypeVariant::int16()),
            int32: storage.add_named_type("int32", TypeVariant::int32()),
            int64: storage.add_named_type("int64", TypeVariant::int64()),
            int_size: storage.add_named_type("int_size", TypeVariant::isize()),
            uint8: storage.add_named_type("uint8", TypeVariant::uint8()),
            uint16: storage.add_named_type("uint16", TypeVariant::uint16()),
            uint32: storage.add_named_type("uint32", TypeVariant::uint32()),
            uint64: storage.add_named_type("uint64", TypeVariant::uint64()),
            uint_size: storage.add_named_type("uint_size", TypeVariant::usize()),
            float: storage.add_named_type("float", TypeVariant::float()),
            string: storage.add_type(TypeVariant::string()),
        };

        let mut compiler = Compiler::default();

        storage.add_named_type("Hello", TypeVariant::structure([("alo", TypeVariant::usize()), ("da", TypeVariant::uint8())]));
        storage.add_named_type("println", TypeVariant::function([TypeVariant::usize()], TypeVariant::void()));

        let expr = Expr::parse_value(
            "{
                const damn = Hello { alo: 24, da: 50 };
                const b: int8 = 24;
                const value = 24;
                
                println(value);
                
                damn.da
            }",
        )
        .unwrap();

        let ty = expr.get_new_type(&mut compiler, &types, &mut storage, &mut solver).unwrap();
        let ty = solver.get_actual_type(ty).unwrap();

        if let Expr::Block(block) = &expr.value {
            if let Stmt::VariableDecl(v) = &block.stmts[0].value {
                if let Some(ty) = solver.get_type_of(&v.value.value, v.value.span) {
                    let ty = solver.get_actual_type(ty).unwrap();

                    println!("type of variable decl value is {:?}", solver.format_type(ty, &storage));
                }
            }
        }

        println!("{expr:#?} {}", solver.len());
        println!("{:?}", solver.format_type(ty, &storage));
    }
}
