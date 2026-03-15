use std::hash::{DefaultHasher, Hash, Hasher};

use cranelift::{
    codegen::ir,
    module::Module,
    prelude::{FloatCC, InstBuilder, IntCC},
};
use mollie_index::Idx;
use mollie_ir::{Field, MollieType};
use mollie_typed_ast::{ExprRef, IsPattern, SolvedPass, TypedAST};
use mollie_typing::{PrimitiveType, Type, TypeRef};

use crate::{
    CompileTypedAST, MolValue, Var,
    error::CompileResult,
    func::{FunctionCompiler, Variable},
};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_is_pattern_expr(&mut self, ast: &TypedAST, target: ExprRef, pattern: &IsPattern<SolvedPass>) -> CompileResult<MolValue> {
        fn compile_pattern<M: Module>(
            target: &MolValue,
            target_ty: TypeRef,
            pattern: &IsPattern<SolvedPass>,
            ast: &TypedAST,
            compiler: &mut FunctionCompiler<'_, M>,
        ) -> CompileResult<MolValue> {
            match pattern {
                &IsPattern::Literal(expr_ref) => {
                    if let &MolValue::Value(target) = target {
                        let target_ty = &compiler.type_context.type_context.types[target_ty];
                        let expr_ty = &compiler.type_context.type_context.types[ast[expr_ref].ty];
                        let MolValue::Value(value) = expr_ref.compile(ast, compiler)? else {
                            unreachable!()
                        };

                        Ok(MolValue::Value(
                            if matches!((target_ty, expr_ty), (Type::Primitive(PrimitiveType::F32), Type::Primitive(PrimitiveType::F32))) {
                                compiler.fn_builder.ins().fcmp(FloatCC::Equal, target, value)
                            } else {
                                compiler.fn_builder.ins().icmp(IntCC::Equal, target, value)
                            },
                        ))
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
                IsPattern::EnumVariant {
                    adt,
                    adt_variant,
                    adt_type_args,
                    values,
                } => {
                    if let &MolValue::Value(target) = target {
                        let expected = compiler.fn_builder.ins().iconst(compiler.compiler.ptr_type(), adt_variant.index() as i64);
                        let metadata = compiler.fn_builder.ins().load(compiler.compiler.ptr_type(), ir::MemFlags::trusted(), target, 0);
                        let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, expected);

                        let hash = {
                            let mut state = DefaultHasher::new();

                            "adt".hash(&mut state);

                            adt.index().hash(&mut state);

                            for &arg in adt_type_args {
                                compiler.type_context.type_context.types.hash_into(&mut state, arg);
                            }

                            state.finish()
                        };

                        let declaration_block = compiler.fn_builder.create_block();
                        let otherwise_block = compiler.fn_builder.create_block();
                        let after_block = compiler.fn_builder.create_block();

                        compiler.fn_builder.ins().brif(result, declaration_block, &[], otherwise_block, &[]);

                        compiler.fn_builder.switch_to_block(declaration_block);
                        compiler.fn_builder.seal_block(declaration_block);

                        for (field_ref, name, pattern) in values {
                            let &(Field { ty, offset, .. }, field_ty) = &compiler.compiler.get_adt_variant(hash, *adt_variant).fields[*field_ref];

                            match ty {
                                MollieType::Regular(ty) => {
                                    let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, offset);

                                    if let Some(pattern) = pattern {
                                        compile_pattern(&MolValue::Value(value), field_ty, pattern, ast, compiler)?;
                                    } else {
                                        let var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(var, value);
                                        compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), field_ty));
                                    }
                                }
                                MollieType::Fat(ty, metadata_ty) => {
                                    let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, offset);
                                    let metadata =
                                        compiler
                                            .fn_builder
                                            .ins()
                                            .load(metadata_ty, ir::MemFlags::trusted(), target, offset + ty.bytes().cast_signed());

                                    if let Some(pattern) = pattern {
                                        compile_pattern(&MolValue::FatPtr(value, metadata), field_ty, pattern, ast, compiler)?;
                                    } else {
                                        let var = compiler.fn_builder.declare_var(ty);
                                        let metadata_var = compiler.fn_builder.declare_var(metadata_ty);

                                        compiler.fn_builder.def_var(var, value);
                                        compiler.fn_builder.def_var(metadata_var, metadata);
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.into(), Variable::new(Var::Fat(var, metadata_var), field_ty));
                                    }
                                }
                            }
                        }

                        compiler.fn_builder.ins().jump(after_block, &[]);
                        compiler.fn_builder.switch_to_block(otherwise_block);
                        compiler.fn_builder.seal_block(otherwise_block);
                        compiler.fn_builder.ins().jump(after_block, &[]);
                        compiler.fn_builder.switch_to_block(after_block);
                        compiler.fn_builder.seal_block(after_block);

                        Ok(MolValue::Value(result))
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
                IsPattern::TypeName { ty, name } => {
                    if let &MolValue::FatPtr(ptr, metadata) = target {
                        let ptr_type = compiler.compiler.ptr_type();
                        let expected_hash = compiler.hash_of(*ty);

                        let value = compiler.fn_builder.ins().iconst(ptr_type, expected_hash.cast_signed());

                        if matches!(compiler.type_context.type_context.types[target_ty], Type::Trait(..)) {
                            let metadata = compiler.fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), metadata, 0);
                            let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                            let declaration_block = compiler.fn_builder.create_block();
                            let after_block = compiler.fn_builder.create_block();

                            compiler.fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                            compiler.fn_builder.switch_to_block(declaration_block);
                            compiler.fn_builder.seal_block(declaration_block);

                            let var = compiler.fn_builder.declare_var(ptr_type);

                            compiler.fn_builder.def_var(var, ptr);
                            compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), *ty));
                            compiler.fn_builder.ins().jump(after_block, &[]);
                            compiler.fn_builder.switch_to_block(after_block);
                            compiler.fn_builder.seal_block(after_block);

                            Ok(MolValue::Value(result))
                        } else {
                            let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                            let declaration_block = compiler.fn_builder.create_block();
                            let after_block = compiler.fn_builder.create_block();

                            compiler.fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);
                            compiler.fn_builder.switch_to_block(declaration_block);
                            compiler.fn_builder.seal_block(declaration_block);

                            let var = compiler.fn_builder.declare_var(ptr_type);

                            compiler.fn_builder.def_var(var, ptr);
                            compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), *ty));
                            compiler.fn_builder.ins().jump(after_block, &[]);
                            compiler.fn_builder.switch_to_block(after_block);
                            compiler.fn_builder.seal_block(after_block);

                            Ok(MolValue::Value(result))
                        }
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
            }
        }

        let target_ty = ast[target].ty;
        let target = target.compile(ast, self)?;

        compile_pattern(&target, target_ty, pattern, ast, self)
    }
}
