use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::VariableDecl;
use mollie_shared::Positioned;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, ValueOrFunc};

impl Compile for Positioned<VariableDecl> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult {
        let ty = self.value.value.get_type(compiler)?;

        let var = fn_builder.declare_var(ty.variant.as_ir_type(compiler.jit.module.isa()));

        compiler.var(&self.value.name.value.0, ty);
        compiler.variables.insert(self.value.name.value.0, var);

        let value = compiler.compile(fn_builder, self.value.value)?;

        if let ValueOrFunc::Value(value) = value {
            fn_builder.def_var(var, value);
        }

        Ok(())
    }
}
