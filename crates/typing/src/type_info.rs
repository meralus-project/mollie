use crate::{AdtKind, AdtRef, PrimitiveType, TraitRef, TypeInfoRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
pub enum FuncArg<T> {
    This(T),
    Regular(T),
}

impl<T> FuncArg<T> {
    pub const fn as_ref(&self) -> FuncArg<&T> {
        match self {
            Self::This(inner) => FuncArg::This(inner),
            Self::Regular(inner) => FuncArg::Regular(inner),
        }
    }

    pub fn map<F: FnOnce(T) -> O, O>(self, func: F) -> FuncArg<O> {
        match self {
            Self::This(inner) => FuncArg::This(func(inner)),
            Self::Regular(inner) => FuncArg::Regular(func(inner)),
        }
    }

    pub const fn as_inner(&self) -> &T {
        match self {
            Self::This(inner) | Self::Regular(inner) => inner,
        }
    }

    pub fn inner(self) -> T {
        match self {
            Self::This(inner) | Self::Regular(inner) => inner,
        }
    }

    /// Returns `true` if the func arg is [`This`].
    ///
    /// [`This`]: FuncArg::This
    #[must_use]
    pub const fn is_this(&self) -> bool {
        matches!(self, Self::This(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub enum TypeInfo {
    Unknown(Option<TypeInfoRef>),
    Generic(usize, Option<TypeInfoRef>),
    Primitive(PrimitiveType),
    Ref(TypeInfoRef),
    Func(Box<[FuncArg<TypeInfoRef>]>, TypeInfoRef),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Adt(AdtRef, AdtKind, Box<[TypeInfoRef]>),
    Array(TypeInfoRef, Option<usize>),
}

impl TypeInfo {
    pub const fn is_func(&self) -> bool {
        matches!(self, Self::Func(..))
    }

    pub const fn is_adt(&self) -> bool {
        matches!(self, Self::Adt(..))
    }

    pub const fn is_struct_like(&self) -> bool {
        matches!(self, Self::Adt(_, AdtKind::Struct | AdtKind::Component, _))
    }

    pub const fn is_struct(&self) -> bool {
        matches!(self, Self::Adt(_, AdtKind::Struct, _))
    }

    pub const fn is_component(&self) -> bool {
        matches!(self, Self::Adt(_, AdtKind::Component, _))
    }

    pub const fn is_enum(&self) -> bool {
        matches!(self, Self::Adt(_, AdtKind::Enum, _))
    }

    pub const fn is_trait(&self) -> bool {
        matches!(self, Self::Trait(..))
    }

    pub const fn is_array(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    pub const fn is_any_component(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_component())
    }

    pub const fn is_number(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_number())
    }

    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_integer())
    }

    pub const fn is_signed_integer(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_signed_integer())
    }

    pub const fn is_unsigned_integer(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_unsigned_integer())
    }

    pub const fn is_float(&self) -> bool {
        matches!(self, Self::Primitive(primitive) if primitive.is_float())
    }
}
