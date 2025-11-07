use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::Ident;
use mollie_shared::{Positioned, Span};

use crate::{Compile, CompileResult, Compiler, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<Ident> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        if let Some(value) = compiler.assign.take() {
            let value = compiler.compile(fn_builder, value)?;

            if let (Some(v), ValueOrFunc::Value(value)) = (compiler.variables.get(&self.value.0), value) {
                fn_builder.def_var(*v, value);

                Ok(ValueOrFunc::Nothing)
            } else {
                unimplemented!()
            }
        } else if let Some(v) = compiler.get(&self.value.0) {
            Ok(v)
        } else if let Some(&func) = compiler.globals.get(&self.value.0) {
            let func = compiler.jit.module.declare_func_in_func(func, fn_builder.func);

            compiler.values.insert(self.value.0, ValueOrFunc::FuncRef(func));

            Ok(ValueOrFunc::FuncRef(func))
        } else if let Some(v) = compiler.variables.get(&self.value.0) {
            Ok(ValueOrFunc::Value(fn_builder.use_var(*v)))
        } else {
            unimplemented!("there's no anything for {}", self.value.0)
        }
    }
}

impl GetType for Ident {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        compiler
            .get_type(&self.0)
            .cloned()
            .ok_or_else(|| TypeError::NotFound {
                ty: None,
                name: self.0.clone(),
            })
            .or_else(|_| compiler.get_local_type(&self.0))
    }
}
