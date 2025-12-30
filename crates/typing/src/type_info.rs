use crate::{ComplexTypeKind, ComplexTypeRef, PrimitiveType, TraitRef, TypeInfoRef};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Unknown(Option<TypeInfoRef>),
    Primitive(PrimitiveType),
    Ref(TypeInfoRef),
    Func(Box<[TypeInfoRef]>, TypeInfoRef),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Complex(ComplexTypeRef, ComplexTypeKind, Box<[TypeInfoRef]>),
    Array(TypeInfoRef, Option<usize>),
}

impl TypeInfo {
    pub const fn is_func(&self) -> bool {
        matches!(self, Self::Func(..))
    }

    pub const fn is_complex(&self) -> bool {
        matches!(self, Self::Complex(..))
    }

    pub const fn is_struct(&self) -> bool {
        matches!(self, Self::Complex(_, ComplexTypeKind::Struct, _))
    }

    pub const fn is_component(&self) -> bool {
        matches!(self, Self::Complex(_, ComplexTypeKind::Component, _))
    }

    pub const fn is_enum(&self) -> bool {
        matches!(self, Self::Complex(_, ComplexTypeKind::Enum, _))
    }

    pub const fn is_array(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    pub const fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::Primitive(
                PrimitiveType::I8
                    | PrimitiveType::I16
                    | PrimitiveType::I32
                    | PrimitiveType::I64
                    | PrimitiveType::ISize
                    | PrimitiveType::U8
                    | PrimitiveType::U16
                    | PrimitiveType::U32
                    | PrimitiveType::U64
                    | PrimitiveType::USize
            )
        )
    }

    pub const fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            Self::Primitive(PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64)
        )
    }

    pub const fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            Self::Primitive(PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64)
        )
    }

    pub const fn is_float(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Float))
    }
}
