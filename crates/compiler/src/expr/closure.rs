use cranelift::{
    codegen::ir,
    module::Module,
    prelude::{FunctionBuilderContext, InstBuilder},
};
use mollie_index::Idx;
use mollie_ir::{MollieType, Struct};
use mollie_typed_ast::{BlockRef, ExprRef, TypedAST};
use mollie_typing::{AdtKind, AdtVariantRef, Arg, Type, TypeRef};

use crate::{
    AsIrType, CompileTypedAST, MolValue, Var,
    allocator::{TypeLayout, TypeLayoutField},
    error::CompileResult,
    func::{FunctionCompiler, Variable},
};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_closure_expr(
        &mut self,
        ast: &TypedAST,
        expr: ExprRef,
        args: &[Arg<TypeRef>],
        captures: &[(String, TypeRef)],
        body: BlockRef,
    ) -> CompileResult<MolValue> {
        let captures_tuple = Struct::new(
            captures
                .iter()
                .map(|(_, ty)| (ty.as_ir_type(&self.type_context.type_context.types, self.compiler.isa()), None)),
        );

        let mut signature = self.compiler.codegen.module.make_signature();

        if let Type::Func(args, returns) = &self.type_context.type_context.types[ast[expr].ty] {
            for arg in args {
                if arg != &self.type_context.type_context.core_types.void {
                    arg.as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                        .add_to_params(&mut signature.params);
                }
            }

            if !captures.is_empty() {
                signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
            }

            if returns != &self.type_context.type_context.core_types.void {
                returns
                    .as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                    .add_to_params(&mut signature.returns);
            }
        }

        let mut ctx = self.compiler.codegen.module.make_context();
        let mut fn_builder_ctx = FunctionBuilderContext::new();

        let mut compiler = FunctionCompiler::new_anonymous(signature, self.compiler, self.type_context, &mut ctx, &mut fn_builder_ctx)?;

        let mut index = 0;

        if let Type::Func(arg_types, _) = &compiler.type_context.type_context.types[ast[expr].ty] {
            for (arg, (is_fat, arg_type)) in args.iter().zip(arg_types.iter().map(|&arg_type| {
                (
                    arg_type.as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa()).is_fat(),
                    arg_type,
                )
            })) {
                let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                let ty = compiler.fn_builder.func.signature.params[index].value_type;

                let var = compiler.fn_builder.declare_var(ty);

                compiler.fn_builder.def_var(var, value);

                if is_fat {
                    let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                    let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                    let metadata_var = compiler.fn_builder.declare_var(ty);

                    compiler.fn_builder.def_var(metadata_var, value);
                    compiler
                        .frames
                        .current_mut()
                        .insert(arg.name.clone(), Variable::new(Var::Fat(var, metadata_var), arg_type));

                    index += 2;
                } else {
                    compiler
                        .frames
                        .current_mut()
                        .insert(arg.name.clone(), Variable::new(Var::Regular(var), arg_type));

                    index += 1;
                }
            }
        }

        if !captures.is_empty() {
            let ptr = compiler.fn_builder.block_params(compiler.entry_block)[index];

            for ((name, arg_type), field) in captures.iter().zip(&captures_tuple.fields) {
                let var = match field.ty {
                    MollieType::Regular(ty) => {
                        let value_var = compiler.fn_builder.declare_var(ty);
                        let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, field.offset);

                        compiler.fn_builder.def_var(value_var, value);

                        Var::Regular(value_var)
                    }
                    MollieType::Fat(ty, metadata_ty) => {
                        let value_var = compiler.fn_builder.declare_var(ty);
                        let metadata_var = compiler.fn_builder.declare_var(metadata_ty);
                        let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, field.offset);
                        let metadata = compiler
                            .fn_builder
                            .ins()
                            .load(ty, ir::MemFlags::trusted(), ptr, field.offset + ty.bytes().cast_signed());

                        compiler.fn_builder.def_var(value_var, value);
                        compiler.fn_builder.def_var(metadata_var, metadata);

                        Var::Fat(value_var, metadata_var)
                    }
                };

                compiler.frames.current_mut().insert(name.clone(), Variable::new(var, *arg_type));
            }
        }

        let returned = body.compile(ast, &mut compiler)?;

        compiler.return_(returned);

        let func_id = compiler.id;

        self.compiler.codegen.module.define_function(func_id, &mut ctx)?;
        self.compiler.codegen.module.clear_context(&mut ctx);

        let func = self.compiler.codegen.module.declare_func_in_func(func_id, self.fn_builder.func);

        if captures.is_empty() {
            Ok(MolValue::FuncRef(func))
        } else {
            let fields = captures
                .iter()
                .zip(&captures_tuple.fields)
                .map(|(&(_, field_ty), field)| {
                    let info = &self.type_context.type_context.types[field_ty];
                    let offset = field.offset.cast_unsigned();
                    let ir_type = field_ty.as_ir_type(&self.type_context.type_context.types, self.compiler.isa());

                    if matches!(info, Type::Adt(..)) {
                        (AdtVariantRef::ZERO, offset, ir_type, TypeLayoutField::Collectable)
                    } else if let &Type::Array(element, _) = info {
                        let info = &self.type_context.type_context.types[element];

                        if matches!(info, Type::Adt(..)) {
                            (AdtVariantRef::ZERO, offset, ir_type, TypeLayoutField::ArrayOfRegular)
                        } else if matches!(info, Type::Trait(..)) {
                            (AdtVariantRef::ZERO, offset, ir_type, TypeLayoutField::ArrayOfFat)
                        } else {
                            (AdtVariantRef::ZERO, offset, ir_type, TypeLayoutField::Regular)
                        }
                    } else {
                        (AdtVariantRef::ZERO, offset, ir_type, TypeLayoutField::Regular)
                    }
                })
                .collect::<Vec<_>>();

            let gc_managed_fields = Vec::leak::<'static>(fields) as &[_];

            let ptr = self.alloc(Box::leak(Box::new(TypeLayout {
                size: captures_tuple.size as usize,
                align: captures_tuple.align as usize,
                kind: Some(AdtKind::Struct),
                adt_ty: None,
                fields: gc_managed_fields,
            })));

            for ((capture, _), field) in captures.iter().zip(&captures_tuple.fields) {
                if let Some(capture) = self.get_var(capture) {
                    match (field.ty, capture) {
                        (MollieType::Regular(_), Var::Regular(value)) => {
                            let value = self.fn_builder.use_var(value);

                            self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);
                        }
                        (MollieType::Fat(ty, _), Var::Fat(value, metadata)) => {
                            let value = self.fn_builder.use_var(value);
                            let metadata = self.fn_builder.use_var(metadata);

                            self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);
                            self.fn_builder
                                .ins()
                                .store(ir::MemFlags::trusted(), metadata, ptr, field.offset + ty.bytes().cast_signed());
                        }
                        _ => (),
                    }
                }
            }

            Ok(MolValue::CaptureFuncRef(func, ptr))
        }
    }
}
