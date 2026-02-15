use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use itertools::Itertools;
use mollie_ir::{Array, MollieType};
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{TypeInfo, TypeInfoRef};

use crate::{AsIrType, CompileTypedAST, MolValue, allocator::TypeLayout, error::CompileResult, func::FunctionCompiler};

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn type_layout_of(&self, ty: TypeInfoRef) -> &'static TypeLayout {
        match self.checker.solver.get_info(ty) {
            &TypeInfo::Primitive(primitive) => self.compiler.core_types.cast_primitive(primitive),
            TypeInfo::Func(..) | TypeInfo::Adt(..) | TypeInfo::Array(..) => self.compiler.core_types.uint_size,
            TypeInfo::Trait(..) => self.compiler.core_types.string,
            _ => self.compiler.core_types.void,
        }
    }

    /// Compiles `[expr, expr, ...]` expression. Returns [`MolValue::FatPtr`]
    /// with a pointer to beginning of array and its size.
    ///
    /// # Errors
    ///
    /// Returns [`CompileError`] if there is a compilation error in one of
    /// elements or an error when creating array.
    ///
    /// [`CompileError`]: crate::error::CompileError
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

            let type_layout_ptr = &raw const *self.type_layout_of(element);
            let ptr_type = self.compiler.ptr_type();
            let type_layout_ptr = self.fn_builder.ins().iconst(ptr_type, type_layout_ptr as i64);
            let size = self.fn_builder.ins().iconst(ptr_type, elements.len().cast_signed() as i64);
            let ptr = self.fn_builder.ins().call(self.context.alloc_array, &[type_layout_ptr, size]);
            let ptr = self.fn_builder.inst_results(ptr)[0];
            let array_ptr = self
                .fn_builder
                .ins()
                .load(ptr_type, ir::MemFlags::trusted(), ptr, size_of::<usize>().cast_signed() as i32 * 2);

            let ir_element = ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa());
            let arr = Array { element: ir_element };

            match ir_element {
                MollieType::Regular(_) => {
                    for (index, value) in values.into_iter().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, array_ptr, arr.get_offset_of(index));
                    }
                }
                MollieType::Fat(ty, _) => {
                    for (index, (value, metadata)) in values.into_iter().tuples::<(ir::Value, ir::Value)>().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, array_ptr, arr.get_offset_of(index));
                        self.fn_builder.ins().store(
                            ir::MemFlags::trusted(),
                            metadata,
                            array_ptr,
                            arr.get_offset_of(index) + ty.bytes().cast_signed(),
                        );
                    }
                }
            }

            Ok(MolValue::Value(ptr))
        } else {
            unimplemented!("expected array")
        }
    }
}
