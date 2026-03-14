// use cranelift::{module::Module, prelude::InstBuilder};
// use indexmap::map::Entry;
// use mollie_index::Idx;
// use mollie_typed_ast::{TypedAST};
// use mollie_typing::TypeInfoRef;

// use crate::{
//     MolValue,
//     error::CompileResult,
//     func::{FuncKey, FunctionCompiler},
// };

// impl<S, M: Module> FunctionCompiler<'_, S, M> {
//     pub fn compile_type_index_expr(&mut self, _: &TypedAST, ty: TypeInfoRef, path: TypePath) -> CompileResult<MolValue> {
//         match path {
//             TypePath::Adt(.., Some((vtable_ref, vfunc_ref))) => Ok(MolValue::FuncRef(self.get_vfunc(self.type_context.solver.hash_of(ty), vtable_ref, vfunc_ref))),
//             TypePath::Adt(.., Some(variant), None) => {
//                 if self.type_context.solver.get_info2(ty).is_enum() {
//                     let discriminant = self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index().cast_signed() as i64);

//                     Ok(MolValue::Value(self.construct(ty, variant, &[discriminant])?))
//                 } else {
//                     Ok(MolValue::Nothing)
//                 }
//             }
//             TypePath::Func(func_ref) => Ok(MolValue::FuncRef(match self.funcs.entry(FuncKey::Ref(func_ref)) {
//                 Entry::Occupied(entry) => *entry.get(),
//                 Entry::Vacant(entry) => {
//                     let func = self
//                         .compiler
//                         .codegen
//                         .module
//                         .declare_func_in_func(self.compiler.func_ref_to_func_id[&func_ref], self.fn_builder.func);

//                     *entry.insert(func)
//                 }
//             })),
//             path => unimplemented!("{path:?}"),
//         }
//     }
// }
