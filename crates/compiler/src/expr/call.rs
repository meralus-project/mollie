use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::Type;

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

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
    pub fn compile_call_expr(&mut self, ast: &TypedAST, func: ExprRef, args: &[ExprRef]) -> CompileResult<MolValue> {
        let func_ty = ast[func].ty;

        if let Type::Func(arg_types, returns) = &self.type_context.type_context.types[func_ty] {
            let v = func.compile(ast, self)?;

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

            let result = match v {
                MolValue::FuncRef(func) => self.fn_builder.ins().call(func, &arg_values),
                MolValue::CaptureFuncRef(func, captures) => {
                    arg_values.push(captures);

                    self.fn_builder.ins().call(func, &arg_values)
                }
                MolValue::Value(func_addr) => {
                    let mut signature = self.compiler.codegen.module.make_signature();
                    let mut is_first = true;

                    for arg in arg_types {
                        if arg != &self.type_context.type_context.core_types.void {
                            if is_first && matches!(self.type_context.type_context.types[*arg], Type::Trait(..)) {
                                signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
                            } else {
                                arg.as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }

                        is_first = false;
                    }

                    if returns != &self.type_context.type_context.core_types.void {
                        returns
                            .as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                            .add_to_params(&mut signature.returns);
                    }

                    let sig = self.fn_builder.import_signature(signature);

                    self.fn_builder.ins().call_indirect(sig, func_addr, &arg_values)
                }
                MolValue::FatPtr(func_addr, captures) => {
                    arg_values.push(captures);

                    let mut signature = self.compiler.codegen.module.make_signature();
                    let mut is_first = true;

                    for arg in arg_types {
                        if arg != &self.type_context.type_context.core_types.void {
                            if is_first && matches!(self.type_context.type_context.types[*arg], Type::Trait(..)) {
                                signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
                            } else {
                                arg.as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }

                        is_first = false;
                    }

                    signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));

                    if returns != &self.type_context.type_context.core_types.void {
                        returns
                            .as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
                            .add_to_params(&mut signature.returns);
                    }

                    let sig = self.fn_builder.import_signature(signature);

                    self.fn_builder.ins().call_indirect(sig, func_addr, &arg_values)
                }
                _ => return Ok(MolValue::Nothing),
            };

            Ok(match self.fn_builder.inst_results(result) {
                [] => MolValue::Nothing,
                &[value] => MolValue::Value(value),
                &[value, metadata]
                    if returns != &self.type_context.type_context.core_types.void
                        && returns.as_ir_type(&self.type_context.type_context.types, self.compiler.isa()).is_fat() =>
                {
                    MolValue::FatPtr(value, metadata)
                }
                values => MolValue::Values(values.to_vec()),
            })
        } else {
            unimplemented!("how is this possible");
        }
    }
}
