use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use itertools::Itertools;
use mollie_ir::{Array, MollieType};
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::TypeInfo;

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_array_expr(&mut self, ast: &TypedAST, expr: ExprRef, elements: &[ExprRef]) -> CompileResult<MolValue> {
        if let &TypeInfo::Array(element, _) = self.checker.solver.get_info(ast[expr].ty) {
            let mut values = Vec::with_capacity(elements.len());

            for expr_ref in elements {
                if let MolValue::Value(value) = expr_ref.compile(ast, self)? {
                    if self.checker.solver.get_info(element).is_any_component() {
                        let hash = self.checker.solver.hash_of(ast[*expr_ref].ty);
                        let metadata = self.fn_builder.ins().iconst(self.compiler.ptr_type(), hash.cast_signed());

                        values.push(value);
                        values.push(metadata);
                    } else if let &TypeInfo::Trait(t, _) = self.checker.solver.get_info(element) {
                        let hash = self.checker.solver.hash_of(ast[*expr_ref].ty);
                        let data_id = self
                            .compiler
                            .codegen
                            .module
                            .declare_data_in_func(self.compiler.trait_to_vtable[&(hash, Some(t))], self.fn_builder.func);

                        let metadata = self.fn_builder.ins().global_value(self.compiler.ptr_type(), data_id);

                        values.push(value);
                        values.push(metadata);
                    } else {
                        values.push(value);
                    }
                }
            }

            let size = self.fn_builder.ins().iconst(self.compiler.ptr_type(), elements.len() as i64);

            let ir_element = ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa());

            let arr = Array { element: ir_element };

            self.compiler.codegen.data_desc.define_zeroinit(arr.get_size(values.len()) as usize);

            let id = self.compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

            self.compiler.codegen.module.define_data(id, &self.compiler.codegen.data_desc).unwrap();
            self.compiler.codegen.data_desc.clear();

            let data_id = self.compiler.codegen.module.declare_data_in_func(id, self.fn_builder.func);
            let ptr = self.fn_builder.ins().global_value(self.compiler.ptr_type(), data_id);

            match ir_element {
                MollieType::Regular(_) => {
                    for (index, value) in values.into_iter().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index));
                    }
                }
                MollieType::Fat(ty, _) => {
                    for (index, (value, metadata)) in values.into_iter().tuples::<(ir::Value, ir::Value)>().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index));
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, ptr, arr.get_offset_of(index) + ty.bytes().cast_signed());
                    }
                }
            }

            Ok(MolValue::FatPtr(ptr, size))
        } else {
            panic!("expected array")
        }
    }
}
