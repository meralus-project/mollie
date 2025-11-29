use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::Ident;
use mollie_shared::{Positioned, Span};

use crate::{Compile, CompileResult, Compiler, GetNewType, GetType, TypeError, TypeResult, ValueOrFunc};

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

impl GetNewType for Ident {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        type_storage
            .get_named_type_ref(&self.0)
            .map(|ty| match unsafe { type_storage.get_type(ty).unwrap_unchecked() }.variant.as_complex() {
                Some(mollie_typing::ComplexType::Array(arr)) => {
                    let element = type_solver.add_info(mollie_typing::TypeInfo::Type(type_storage.ref_of_type(&arr.element).unwrap()));

                    type_solver.add_info(mollie_typing::TypeInfo::Array(element))
                }
                Some(mollie_typing::ComplexType::Function(func)) => {
                    let args = func
                        .args
                        .iter()
                        .map(|arg| type_solver.add_info(mollie_typing::TypeInfo::Type(type_storage.ref_of_type(arg).unwrap())))
                        .collect();

                    let output = type_solver.add_info(mollie_typing::TypeInfo::Type(type_storage.ref_of_type(func.returns.as_ref()).unwrap()));

                    type_solver.add_info(mollie_typing::TypeInfo::Func(args, output))
                }
                _ => type_solver.add_info(mollie_typing::TypeInfo::Type(ty)),
            })
            .or_else(|| type_solver.get_var(&self.0).map(|var| var.ty))
            .ok_or_else(|| TypeError::NotFound {
                ty: None,
                name: self.0.clone(),
            })
    }
}
