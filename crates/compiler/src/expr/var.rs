use cranelift::module::Module;
use indexmap::map::Entry;
use mollie_typed_ast::{ExprRef, TypedAST};

use crate::{
    CompileTypedAST, MolValue, Var,
    error::CompileResult,
    func::{FuncKey, FunctionCompiler},
};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_var_expr(&mut self, ast: &TypedAST, expr: ExprRef, name: &str) -> CompileResult<MolValue> {
        if let Some((_, value_ref)) = self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
            let value = value_ref.compile(ast, self)?;

            match (self.get_var(name), value) {
                (Some(Var::Regular(v)), MolValue::Value(value)) => {
                    self.fn_builder.def_var(v, value);

                    Ok(MolValue::Nothing)
                }
                (Some(Var::Fat(v, m)), MolValue::FatPtr(value, metadata)) => {
                    self.fn_builder.def_var(v, value);
                    self.fn_builder.def_var(m, metadata);

                    Ok(MolValue::Nothing)
                }
                _ => unimplemented!(),
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
