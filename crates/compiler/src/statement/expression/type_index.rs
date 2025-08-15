use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use mollie_parser::TypeIndexExpr;
use mollie_shared::{Positioned, Span};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<TypeIndexExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.value.target.get_type(compiler)?;

        let (vtable, trait_index, function) = compiler.find_vtable_function_index(&ty.variant, &self.value.index.value.0).unwrap();
        let (sig, func) = compiler.vtables[vtable][&trait_index].1[function].1;

        Ok(ValueOrFunc::ExtFunc(
            sig,
            fn_builder.ins().func_addr(compiler.jit.module.isa().pointer_type(), func),
        ))
    }
}

impl GetType for TypeIndexExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        let ty = self.target.get_type(compiler)?;

        let (vtable, trait_index, function) = compiler.find_vtable_function_index(&ty.variant, &self.index.value.0).unwrap();

        Ok(compiler.vtables[vtable][&trait_index].1[function].0.clone())
    }
}
