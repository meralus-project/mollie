use cranelift::{module::Module, prelude::types};
use mollie_parser::{PrimitiveType, Type};
use mollie_shared::Span;
use mollie_typing::{ArrayOfType, ArrayType, ComplexType, TypeVariant};

use crate::{Compiler, GetPositionedType, GetType, TypeError, TypeResult};

impl GetType for Type {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        use PrimitiveType::{Boolean, Component, Float, Int8, Int16, Int32, Int64, IntSize, Null, String, UInt8, UInt16, UInt32, UInt64, UIntSize, Void};
        use Type::{Array, Custom, OneOf, Primitive};

        Ok(match self {
            Primitive(IntSize) => TypeVariant::of::<isize>().into(),
            Primitive(Int64) => TypeVariant::of::<i64>().into(),
            Primitive(Int32) => TypeVariant::of::<i32>().into(),
            Primitive(Int16) => TypeVariant::of::<i16>().into(),
            Primitive(Int8) => TypeVariant::of::<i8>().into(),
            Primitive(UIntSize) => TypeVariant::of::<usize>().into(),
            Primitive(UInt64) => TypeVariant::of::<u64>().into(),
            Primitive(UInt32) => TypeVariant::of::<u32>().into(),
            Primitive(UInt16) => TypeVariant::of::<u16>().into(),
            Primitive(UInt8) => TypeVariant::of::<u8>().into(),
            Primitive(Float) => TypeVariant::of::<f32>().into(),
            Primitive(Boolean) => TypeVariant::of::<bool>().into(),
            Primitive(String) => TypeVariant::of::<&str>().into(),
            Primitive(Component) => TypeVariant::Primitive(mollie_typing::PrimitiveType::Component).into(),
            Primitive(Void) => TypeVariant::of::<()>().into(),
            Primitive(Null) => TypeVariant::null().into(),
            Array(ty, size) => {
                let element = ty.get_type(compiler)?;

                TypeVariant::complex(ComplexType::Array(ArrayType {
                    arr: ArrayOfType::new(match &element.variant {
                        TypeVariant::Primitive(primitive_type) => match primitive_type {
                            mollie_typing::PrimitiveType::I64 | mollie_typing::PrimitiveType::U64 => types::I64,
                            mollie_typing::PrimitiveType::I32 | mollie_typing::PrimitiveType::U32 => types::I32,
                            mollie_typing::PrimitiveType::I16 | mollie_typing::PrimitiveType::U16 => types::I16,
                            mollie_typing::PrimitiveType::I8 | mollie_typing::PrimitiveType::U8 | mollie_typing::PrimitiveType::Boolean => types::I8,
                            mollie_typing::PrimitiveType::Float => types::F32,
                            _ => compiler.jit.module.isa().pointer_type(),
                        },
                        _ => compiler.jit.module.isa().pointer_type(),
                    }),
                    element,
                    size: size.map(|v| v.value),
                }))
                .into()
            }
            OneOf(types) => TypeVariant::complex(ComplexType::OneOf(types.iter().map(|ty| ty.get_type(compiler)).collect::<TypeResult<_>>()?)).into(),
            Custom(name) => compiler
                .types
                .get(&name.name.value.0)
                .cloned()
                .map(|ty| {
                    name.generics
                        .iter()
                        .map(|generic| generic.get_type(compiler))
                        .collect::<TypeResult<Vec<_>>>()
                        .map(|applied_generics| mollie_typing::Type {
                            variant: ty.variant.clone(),
                            applied_generics,
                            declared_at: ty.declared_at,
                        })
                })
                .ok_or_else(|| TypeError::NotFound {
                    ty: None,
                    name: name.name.value.0.clone(),
                })??,
        })
    }
}
