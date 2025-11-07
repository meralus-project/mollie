use cranelift::module::Module;
use mollie_parser::{PrimitiveType, Type};
use mollie_shared::Span;
use mollie_typing::{ArrayType, ComplexType, FunctionType, TypeVariant};

use crate::{Compiler, GetPositionedType, GetType, TypeError, TypeResult};

impl GetType for Type {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        use PrimitiveType::{Boolean, Component, Float, Int8, Int16, Int32, Int64, IntSize, Null, String, UInt8, UInt16, UInt32, UInt64, UIntSize, Void};
        use Type::{Array, Custom, Func, OneOf, Primitive};

        Ok(match self {
            Primitive(IntSize) => TypeVariant::isize().into(),
            Primitive(Int64) => TypeVariant::int64().into(),
            Primitive(Int32) => TypeVariant::int32().into(),
            Primitive(Int16) => TypeVariant::int16().into(),
            Primitive(Int8) => TypeVariant::int8().into(),
            Primitive(UIntSize) => TypeVariant::usize().into(),
            Primitive(UInt64) => TypeVariant::uint64().into(),
            Primitive(UInt32) => TypeVariant::uint32().into(),
            Primitive(UInt16) => TypeVariant::uint16().into(),
            Primitive(UInt8) => TypeVariant::uint8().into(),
            Primitive(Float) => TypeVariant::float().into(),
            Primitive(Boolean) => TypeVariant::boolean().into(),
            Primitive(String) => TypeVariant::string().into(),
            Primitive(Component) => TypeVariant::Primitive(mollie_typing::PrimitiveType::Component).into(),
            Primitive(Void) => TypeVariant::void().into(),
            Primitive(Null) => TypeVariant::null().into(),
            Array(ty, size) => {
                let element = ty.get_type(compiler)?;
                let element_ir = element.variant.as_ir_type(compiler.jit.module.isa());

                mollie_typing::Type {
                    variant: TypeVariant::complex(ComplexType::Array(ArrayType {
                        element: element.clone(),
                        size: size.map(|v| v.value),
                        array: mollie_ir::Array { element: element_ir },
                    })),
                    applied_generics: vec![element],
                    declared_at: None,
                }
            }
            OneOf(types) => TypeVariant::complex(ComplexType::OneOf(types.iter().map(|ty| ty.get_type(compiler)).collect::<TypeResult<_>>()?)).into(),
            Custom(name) => compiler
                .get_type(&name.name.value.0)
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
            Func(args, returns) => TypeVariant::complex(ComplexType::Function(FunctionType {
                is_native: false,
                this: None,
                args: args.iter().map(|arg| arg.get_type(compiler)).collect::<TypeResult<_>>()?,
                returns: Box::new(returns.as_ref().map_or_else(|| Ok(TypeVariant::void().into()), |ty| ty.get_type(compiler))?),
            }))
            .into(),
        })
    }
}
