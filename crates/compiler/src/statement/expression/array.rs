use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use mollie_ir::{Array, FatPtr};
use mollie_parser::ArrayExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{ArrayType, ComplexType, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<ArrayExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.get_type(compiler)?;

        if let Some(arr) = ty.variant.as_array() {
            let size = self.value.elements.len();
            let mut elements = Vec::with_capacity(size);

            for element in self.value.elements {
                compiler.infer.replace(arr.element.clone());

                if let ValueOrFunc::Value(v) = compiler.compile(fn_builder, element)? {
                    elements.push(v);
                }
            }

            let ptr = arr.array.instance(compiler.jit.module.isa(), fn_builder, elements);
            let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), size.cast_signed() as i64);

            Ok(ValueOrFunc::Value(FatPtr::new(compiler.jit.module.isa(), fn_builder, ptr, size)))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for ArrayExpr {
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult {
        println!("{:#?}", compiler.infer);

        let infer = compiler.infer.take_if(|ty| ty.variant.is_array());
        let element = if let Some(infer) = &infer {
            unsafe { infer.variant.as_array().unwrap_unchecked() }.element.clone()
        } else {
            self.elements[0].get_type(compiler)?
        };

        let element_ir = element.variant.as_ir_type(compiler.jit.module.isa());
        let size = infer.as_ref().map_or_else(
            || if self.elements.is_empty() { None } else { Some(self.elements.len()) },
            |infer| unsafe { infer.variant.as_array().unwrap_unchecked() }.size,
        );

        Ok(Type {
            applied_generics: vec![element.clone()],
            variant: TypeVariant::complex(ComplexType::Array(ArrayType {
                size,
                element,
                array: Array { element: element_ir },
            })),
            declared_at: Some(span),
        })
    }
}

impl GetNewType for ArrayExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        // let infer = compiler.infer.take_if(|ty| ty.variant.is_array());
        // let element = if let Some(infer) = &infer {
        //     unsafe { infer.variant.as_array().unwrap_unchecked() }.element.clone()
        // } else {
        //     self.elements[0].get_type(compiler)?
        // };

        // let element_ir = element.variant.as_ir_type(compiler.jit.module.isa());
        // let size = infer.as_ref().map_or_else(
        //     || if self.elements.is_empty() { None } else { Some(self.elements.len()) },
        //     |infer| unsafe { infer.variant.as_array().unwrap_unchecked() }.size,
        // );
        let element = type_solver.add_info(mollie_typing::TypeInfo::Unknown(None));

        for arr_element in &self.elements {
            let arr_element = arr_element.get_new_type(compiler, core_types, type_storage, type_solver)?;

            type_solver.unify(element, arr_element);
        }

        Ok(type_solver.add_info(mollie_typing::TypeInfo::Array(element)))
    }
}

#[cfg(test)]
mod tests {
    use mollie_parser::{ArrayExpr, Parse};
    use mollie_typing::{CoreTypes, TypeSolver, TypeStorage, TypeVariant};

    use crate::{Compiler, GetNewPositionedType};

    #[test]
    fn test_array_solving() {
        let mut storage = TypeStorage::default();
        let mut solver = TypeSolver::default();

        let types = CoreTypes {
            void: storage.add_type(TypeVariant::void()),
            any: storage.add_type(TypeVariant::any()),
            boolean: storage.add_named_type("boolean", TypeVariant::boolean()),
            int8: storage.add_named_type("int8", TypeVariant::int8()),
            int16: storage.add_named_type("int16", TypeVariant::int16()),
            int32: storage.add_named_type("int32", TypeVariant::int32()),
            int64: storage.add_named_type("int64", TypeVariant::int64()),
            int_size: storage.add_named_type("int_size", TypeVariant::isize()),
            uint8: storage.add_named_type("uint8", TypeVariant::uint8()),
            uint16: storage.add_named_type("uint16", TypeVariant::uint16()),
            uint32: storage.add_named_type("uint32", TypeVariant::uint32()),
            uint64: storage.add_named_type("uint64", TypeVariant::uint64()),
            uint_size: storage.add_named_type("uint_size", TypeVariant::usize()),
            float: storage.add_named_type("float", TypeVariant::float()),
            string: storage.add_type(TypeVariant::string()),
        };

        let mut compiler = Compiler::default();

        let arr = ArrayExpr::parse_value("[\"1\", \"2int64\", \"3\"]").unwrap();
        let ty = arr.get_new_type(&mut compiler, &types, &mut storage, &mut solver);

        if let Ok(ty) = ty {
            let ty = solver.get_actual_type(ty).unwrap();

            println!("{:?}", solver.format_type(ty, &storage));
        }
    }
}
