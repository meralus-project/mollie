use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use mollie_parser::ArrayExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{ArrayType, ComplexType, FatPtr, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<ArrayExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.get_type(compiler)?;

        if let Some(arr) = ty.variant.as_array() {
            let size = self.value.elements.len();
            let mut elements = Vec::with_capacity(size);

            for element in self.value.elements {
                if let ValueOrFunc::Value(v) = compiler.compile(fn_builder, element)? {
                    elements.push(v);
                }
            }

            let ptr = arr.instance(compiler.jit.module.isa(), fn_builder, elements);
            let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), size.cast_signed() as i64);

            Ok(ValueOrFunc::Value(FatPtr::new(compiler.jit.module.isa(), fn_builder, ptr, size)))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for ArrayExpr {
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult {
        let element = self.elements[0].get_type(compiler)?;
        let size = self.elements.len();

        Ok(Type {
            applied_generics: vec![element.clone()],
            variant: TypeVariant::complex(ComplexType::Array(ArrayType { size: Some(size), element })),
            declared_at: Some(span),
        })
    }
}
