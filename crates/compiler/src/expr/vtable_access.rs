use cranelift::module::Module;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{TypeRef, VFuncRef, VTableRef};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_vtable_index(
        &mut self,
        ast: &TypedAST,
        target: Option<ExprRef>,
        target_ty: TypeRef,
        vtable: VTableRef,
        func: VFuncRef,
    ) -> CompileResult<MolValue> {
        if let Some(target) = target {
            let target = target.compile(ast, self)?;

            self.this.replace(target);
        }

        let target_hash = self.hash_of(target_ty);

        Ok(MolValue::FuncRef(self.get_vfunc(target_hash, vtable, func)))
        // VFunc::Unknown(_, trait_func_ref) => {
        //     if let MolValue::FatPtr(value, vtable_ptr) = target_val {
        //         let vtable_func =
        // VTablePtr::get_func_ptr(self.compiler.isa(), &mut self.fn_builder,
        // vtable_ptr, trait_func_ref.index() as u32);

        //         self.this.replace(MolValue::Value(value));

        //         Ok(MolValue::Value(vtable_func))
        //     } else {
        //         unimplemented!("expected fat ptr for accessing dynamic vtable
        // value")     }
        // }
    }
}
