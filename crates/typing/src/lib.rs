mod kind;
mod resolver;
mod ty;

use std::fmt::{self, Write};

use cranelift::{codegen::ir, prelude::isa::TargetIsa};
use mollie_shared::{MaybePositioned, Span, SpanType, pretty_fmt::PrettyFmt};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub use self::{
    kind::TypeKind,
    resolver::{TypeRef, TypeResolver, TypeVTable, VFuncRef, VTableRef},
    ty::*,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitFunc {
    pub name: String,
    pub this: bool,
    pub args: Vec<Type>,
    pub returns: Type,
    pub signature: ir::Signature,
    pub signature_ref: ir::SigRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub generics: Vec<String>,
    pub functions: Vec<TraitFunc>,
    pub declared_at: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Type {
    pub variant: TypeVariant,
    pub applied_generics: Vec<Type>,
    pub declared_at: Option<Span>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.variant.fmt(f)?;

        if !self.applied_generics.is_empty() {
            f.write_char('<')?;
            f.write_array_like(&self.applied_generics, false)?;
            f.write_char('>')?;
        }

        if let Some(declared_at) = self.declared_at {
            write!(f, " declared at {}:{}", declared_at.line + 1, declared_at.column + 1)?;
        }

        Ok(())
    }
}

impl Type {
    #[must_use]
    pub fn resolve_type(mut self, applied_generics: &[Self]) -> Self {
        if let TypeVariant::Generic(generic) = self.variant {
            return applied_generics.get(generic).unwrap_or_else(|| panic!("there's no applied generic {generic} for {self}")).clone();
        } else if let TypeVariant::Complex(variant) = &self.variant {
            if let ComplexType::OneOf(types) = &**variant {
                let mut result_types = Vec::with_capacity(types.len());

                for ty in types {
                    result_types.push(ty.clone().resolve_type(applied_generics));
                }

                return Self {
                    variant: TypeVariant::complex(ComplexType::OneOf(result_types)),
                    applied_generics: self.applied_generics,
                    declared_at: self.declared_at,
                };
            } else if self.variant.is_enum() || self.variant.is_struct() {
                self.applied_generics = applied_generics.to_vec();
            }
        }

        self
    }

    pub fn resolved_kind(&self, applied_generics: &[Self]) -> MaybePositioned<TypeKind> {
        if let TypeVariant::Generic(generic) = self.variant {
            applied_generics.get(generic).map_or_else(|| self.kind(), Self::kind)
        } else {
            self.kind()
        }
    }

    pub fn kind(&self) -> MaybePositioned<TypeKind> {
        MaybePositioned {
            value: self.variant.kind(),
            span: self.declared_at.map(|span| (SpanType::Definition, span)),
        }
    }
}

impl From<TypeVariant> for Type {
    fn from(value: TypeVariant) -> Self {
        Self {
            variant: value,
            applied_generics: Vec::new(),
            declared_at: None,
        }
    }
}

pub trait SizedType {
    fn bytes(&self, isa: &dyn TargetIsa) -> usize;
}

impl SizedType for PrimitiveType {
    fn bytes(&self, isa: &dyn TargetIsa) -> usize {
        use PrimitiveType::{Any, Boolean, Component, Float, I8, I16, I32, I64, ISize, Null, String, U8, U16, U32, U64, USize, Void};

        match self {
            Any | Void | Null => 0,
            ISize | USize | String | Component => isa.pointer_bytes().into(),
            I64 | U64 => 8,
            I32 | U32 | Float => 4,
            I16 | U16 => 2,
            I8 | U8 | Boolean => 1,
        }
    }
}

impl SizedType for ComplexType {
    fn bytes(&self, isa: &dyn TargetIsa) -> usize {
        match self {
            Self::OneOf(items) => items.iter().map(|ty| ty.variant.bytes(isa)).max().unwrap_or_default(),
            _ => isa.pointer_bytes().into(),
        }
    }
}

impl SizedType for TypeVariant {
    fn bytes(&self, isa: &dyn TargetIsa) -> usize {
        match self {
            Self::Primitive(ty) => ty.bytes(isa),
            Self::Complex(ty) => ty.bytes(isa),
            _ => isa.pointer_bytes().into(),
        }
    }
}
