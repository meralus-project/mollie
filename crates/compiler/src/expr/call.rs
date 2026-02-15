use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use indexmap::map::Entry;
use mollie_typed_ast::{ExprRef, FuncSource, TypedAST};
use mollie_typing::{IntType, PrimitiveType, TypeInfo, TypeInfoRef, UIntType};

use crate::{
    AsIrType, CompileTypedAST, MolValue,
    error::CompileResult,
    func::{FuncKey, FunctionCompiler},
};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    /// Compiles `func(...args)` expression. Returns any of variants
    /// [`MolValue`] depending on return type of function.
    ///
    /// # Errors
    ///
    /// Returns [`CompileError`] if there is a compilation error in `func` or
    /// one of `args`.
    ///
    /// [`CompileError`]: crate::error::CompileError
    pub fn compile_call_expr(&mut self, ast: &TypedAST, func: FuncSource, args: &[ExprRef]) -> CompileResult<MolValue> {
        let func_ty = match func {
            FuncSource::Explicit(func) => self.checker.local_functions[func].ty,
            FuncSource::Expr(func) => ast[func].ty,
            FuncSource::Intrinsic(_, ty) => ty,
        };

        if let Some(TypeInfo::Func(arg_types, returns)) = self.checker.solver.get_maybe_info(func_ty) {
            let v = match func {
                FuncSource::Explicit(func) => MolValue::FuncRef(match self.funcs.entry(FuncKey::Ref(func)) {
                    Entry::Occupied(entry) => *entry.get(),
                    Entry::Vacant(entry) => {
                        let func = self
                            .compiler
                            .codegen
                            .module
                            .declare_func_in_func(self.compiler.func_ref_to_func_id[&func], self.fn_builder.func);

                        *entry.insert(func)
                    }
                }),
                FuncSource::Expr(func) => func.compile(ast, self)?,
                FuncSource::Intrinsic(kind, _) => match kind {
                    mollie_typed_ast::IntrinsicKind::SizeOf => todo!(),
                    mollie_typed_ast::IntrinsicKind::AlignOf => todo!(),
                    mollie_typed_ast::IntrinsicKind::SizeOfValue => {
                        fn get_size<S, M: Module>(ty: TypeInfoRef, func_compiler: &FunctionCompiler<'_, S, M>) -> usize {
                            let ptr_type = func_compiler.compiler.ptr_type();

                            match func_compiler.checker.solver.get_info(ty) {
                                TypeInfo::Primitive(primitive_type) => match primitive_type {
                                    PrimitiveType::Any => unimplemented!(),
                                    PrimitiveType::Int(IntType::ISize)
                                    | PrimitiveType::UInt(UIntType::USize)
                                    | PrimitiveType::String
                                    | PrimitiveType::Component => ptr_type.bytes() as usize,
                                    PrimitiveType::Int(IntType::I64) | PrimitiveType::UInt(UIntType::U64) => 8,
                                    PrimitiveType::Int(IntType::I32) | PrimitiveType::UInt(UIntType::U32) | PrimitiveType::Float => 4,
                                    PrimitiveType::Int(IntType::I16) | PrimitiveType::UInt(UIntType::U16) => 2,
                                    PrimitiveType::Int(IntType::I8) | PrimitiveType::UInt(UIntType::U8) | PrimitiveType::Boolean => 1,
                                    PrimitiveType::Void => 0,
                                    PrimitiveType::Null => unimplemented!(),
                                },
                                TypeInfo::Func(..) | TypeInfo::Trait(..) | TypeInfo::Array(_, None) => ptr_type.bytes() as usize,
                                TypeInfo::Adt(..) => {
                                    let hash = func_compiler.hash_of(ty);

                                    func_compiler.compiler.adt_types[&hash].type_layout.size
                                }
                                &TypeInfo::Array(type_info_ref, Some(size)) => get_size(type_info_ref, func_compiler) * size,
                                _ => unimplemented!(),
                            }
                        }

                        let size = get_size(ast[args[0]].ty, self);

                        return Ok(MolValue::Value(
                            self.fn_builder.ins().iconst(self.compiler.ptr_type(), size.cast_signed() as i64),
                        ));
                    }
                    mollie_typed_ast::IntrinsicKind::AlignOfValue => todo!(),
                },
            };

            let mut arg_values = Vec::new();

            if let Some(this) = self.this.take() {
                match this {
                    MolValue::Value(value) => arg_values.push(value),
                    MolValue::FatPtr(value, metadata) => {
                        arg_values.push(value);
                        arg_values.push(metadata);
                    }
                    MolValue::Nothing => (),
                    _ => unimplemented!("received incorrect value for <self> argument: {this:?}"),
                }
            }

            for arg in args {
                let value = arg.compile(ast, self)?;

                match value {
                    MolValue::Value(value) => arg_values.push(value),
                    MolValue::FatPtr(value, metadata) => {
                        arg_values.push(value);
                        arg_values.push(metadata);
                    }
                    MolValue::FuncRef(func) => {
                        let func = self.fn_builder.ins().func_addr(self.compiler.ptr_type(), func);

                        arg_values.push(func);
                    }
                    MolValue::Nothing => (),
                    _ => unimplemented!("received incorrect value for argument: {arg:?} {value:?}"),
                }
            }

            let value = match v {
                MolValue::FuncRef(func) => {
                    let result = self.fn_builder.ins().call(func, &arg_values);

                    match self.fn_builder.inst_results(result) {
                        [] => MolValue::Nothing,
                        &[value] => MolValue::Value(value),
                        &[value, metadata]
                            if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                        {
                            MolValue::FatPtr(value, metadata)
                        }
                        values => MolValue::Values(values.to_vec()),
                    }
                }
                MolValue::CaptureFuncRef(func, captures) => {
                    arg_values.push(captures);

                    let result = self.fn_builder.ins().call(func, &arg_values);

                    match self.fn_builder.inst_results(result) {
                        [] => MolValue::Nothing,
                        &[value] => MolValue::Value(value),
                        &[value, metadata]
                            if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                        {
                            MolValue::FatPtr(value, metadata)
                        }
                        values => MolValue::Values(values.to_vec()),
                    }
                }
                MolValue::Value(func_addr) => {
                    let mut signature = self.compiler.codegen.module.make_signature();

                    for arg in arg_types {
                        if arg.as_inner() != &self.checker.core_types.void {
                            if arg.is_this() && self.checker.solver.get_info(arg.inner()).is_trait() {
                                signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
                            } else {
                                arg.as_inner()
                                    .as_ir_type(&self.checker.solver, self.compiler.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }
                    }

                    if returns != &self.checker.core_types.void {
                        returns
                            .as_ir_type(&self.checker.solver, self.compiler.isa())
                            .add_to_params(&mut signature.returns);
                    }

                    let sig = self.fn_builder.import_signature(signature);
                    let result = self.fn_builder.ins().call_indirect(sig, func_addr, &arg_values);

                    match self.fn_builder.inst_results(result) {
                        [] => MolValue::Nothing,
                        &[value] => MolValue::Value(value),
                        &[value, metadata]
                            if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                        {
                            MolValue::FatPtr(value, metadata)
                        }
                        values => MolValue::Values(values.to_vec()),
                    }
                }
                MolValue::FatPtr(func_addr, captures) => {
                    arg_values.push(captures);

                    let mut signature = self.compiler.codegen.module.make_signature();

                    for arg in arg_types {
                        if arg.as_inner() != &self.checker.core_types.void {
                            if arg.is_this() && self.checker.solver.get_info(arg.inner()).is_trait() {
                                signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
                            } else {
                                arg.as_inner()
                                    .as_ir_type(&self.checker.solver, self.compiler.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }
                    }

                    signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));

                    if returns != &self.checker.core_types.void {
                        returns
                            .as_ir_type(&self.checker.solver, self.compiler.isa())
                            .add_to_params(&mut signature.returns);
                    }

                    let sig = self.fn_builder.import_signature(signature);
                    let result = self.fn_builder.ins().call_indirect(sig, func_addr, &arg_values);

                    match self.fn_builder.inst_results(result) {
                        [] => MolValue::Nothing,
                        &[value] => MolValue::Value(value),
                        &[value, metadata]
                            if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                        {
                            MolValue::FatPtr(value, metadata)
                        }
                        values => MolValue::Values(values.to_vec()),
                    }
                }
                _ => MolValue::Nothing,
            };

            Ok(value)
        } else {
            unimplemented!("how is this possible");
        }
    }
}
