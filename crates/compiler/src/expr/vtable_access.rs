use cranelift::module::Module;
use mollie_index::Idx;
use mollie_ir::VTablePtr;
use mollie_typed_ast::{ExprRef, TypedAST, VFunc};

use crate::{CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_vtable_access(&mut self, ast: &TypedAST, target: ExprRef, func: VFunc) -> CompileResult<MolValue> {
        let target_val = target.compile(ast, self)?;

        match func {
            VFunc::Known(vtable_ref, func) => {
                self.this.replace(target_val);

                let target_hash = self.hash_of(ast[target].ty);

                Ok(MolValue::FuncRef(self.get_vfunc(target_hash, vtable_ref, func)))
            }
            VFunc::Unknown(_, trait_func_ref) => {
                if let MolValue::FatPtr(value, vtable_ptr) = target_val {
                    let vtable_func = VTablePtr::get_func_ptr(
                        self.compiler.isa(),
                        &mut self.fn_builder,
                        vtable_ptr,
                        trait_func_ref.index().try_into().unwrap(),
                    );

                    self.this.replace(MolValue::Value(value));

                    Ok(MolValue::Value(vtable_func))
                } else {
                    panic!("expected fat ptr for accessing dynamic vtable value")
                }
            }
        }
    }
}
