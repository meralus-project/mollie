use cranelift::module::Module;
use indexmap::map::Entry;
use mollie_typed_ast::{ExprRef, TypedAST};

use crate::{
    CompileTypedAST, MolValue, Var,
    error::CompileResult,
    func::{FuncKey, FunctionCompiler},
};

impl<M: Module> FunctionCompiler<'_, M> {
    /// Retrieves the variable's value and returns it, or assigns a new value to
    /// the variable.
    ///
    /// # Errors
    ///
    /// Returns a [`CompileError`] if an error occurs while compiling the new
    /// value for the variable.
    ///
    /// [`CompileError`]: crate::error::CompileError
    pub fn compile_var_expr(&mut self, ast: &TypedAST, expr: ExprRef, name: &str) -> CompileResult<MolValue> {
        if let Some((_, operator, value_ref)) = self.assign_ref.take_if(|(lhs_ref, ..)| *lhs_ref == expr) {
            let value = value_ref.compile(ast, self)?;

            match (self.get_var(name), value) {
                (Some(Var::Regular(v)), MolValue::Value(value)) => {
                    let value = match operator {
                        mollie_shared::Operator::Assign => value,
                        op if let Some(operator) = op.lower() => {
                            let current_value = self.fn_builder.use_var(v);

                            self.bin_op(current_value, ast[value_ref].ty, operator, value, ast[value_ref].ty)
                        }
                        _ => return Ok(MolValue::Nothing),
                    };

                    self.fn_builder.def_var(v, value);

                    Ok(MolValue::Nothing)
                }
                (Some(Var::Fat(v, m)), MolValue::FatPtr(value, metadata)) => {
                    self.fn_builder.def_var(v, value);
                    self.fn_builder.def_var(m, metadata);

                    Ok(MolValue::Nothing)
                }
                (var, value) => unimplemented!("var: {var:?}, value: {value:?}"),
            }
        } else if let Some(v) = self.get_var(name) {
            match v {
                Var::Regular(v) => Ok(MolValue::Value(self.fn_builder.use_var(v))),
                Var::Fat(v, m) => Ok(MolValue::FatPtr(self.fn_builder.use_var(v), self.fn_builder.use_var(m))),
            }
        } else if let Some(&func) = self.compiler.name_to_func_id.get(name) {
            Ok(MolValue::FuncRef(match self.funcs.entry(FuncKey::Id(func)) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let func = self.compiler.codegen.module.declare_func_in_func(func, self.fn_builder.func);

                    *entry.insert(func)
                }
            }))
        } else {
            unimplemented!("there's no anything for {name}: {:#?}", self.frames)
        }
    }
}
