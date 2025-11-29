use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::VariableDecl;
use mollie_shared::{Positioned, Span};

// use mollie_typing::{Type, TypeVariant};
use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, TypeResult, ValueOrFunc};

impl Compile for Positioned<VariableDecl> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult {
        if let Some(ty) = self.value.ty {
            let ty = ty.get_type(compiler)?;

            compiler.infer.replace(ty);
        }

        let ty = self.value.value.get_type(compiler)?;
        let var = fn_builder.declare_var(ty.variant.as_ir_type(compiler.jit.module.isa()));

        compiler.var_ty(&self.value.name.value.0, ty.clone());
        compiler.infer.replace(ty);

        let value = compiler.compile(fn_builder, self.value.value)?;

        if let ValueOrFunc::Value(value) = value {
            compiler.variables.insert(self.value.name.value.0, var);

            fn_builder.def_var(var, value);
        }

        Ok(())
    }
}

impl GetNewType for VariableDecl {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {        
        let ty = type_solver.add_info(mollie_typing::TypeInfo::Type(core_types.void));

        let value_ty = self.value.get_new_type(compiler, core_types, type_storage, type_solver)?;

        if let Some(ty) = &self.ty {
            let ty = ty.get_new_type(compiler, core_types, type_storage, type_solver)?;

            type_solver.unify(value_ty, ty);
        }

        type_solver.add_var(&self.name.value.0, value_ty);

        Ok(ty)
    }
}
