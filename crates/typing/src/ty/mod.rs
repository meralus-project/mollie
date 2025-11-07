mod complex;
mod primitive;

use std::sync::Arc;

use cranelift::{
    codegen::ir,
    prelude::{AbiParam, Signature, isa::TargetIsa},
};
use derive_more::Display;
use mollie_ir::Struct;

pub use self::{complex::*, primitive::PrimitiveType};
use crate::{Type, TypeKind};

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum TypeVariant {
    Primitive(PrimitiveType),
    Complex(Arc<ComplexType>),
    #[display("<trait>")]
    Trait(usize),
    #[display("<generic>")]
    Generic(usize),
    #[display("&{}{}", if *mutable { "mut " } else { "" }, ty.variant)]
    Ref {
        ty: Box<Type>,
        mutable: bool,
    },
    #[display("<self>")]
    This,
}

impl TypeVariant {
    pub fn kind(&self) -> TypeKind {
        match self {
            Self::Primitive(PrimitiveType::Any) => TypeKind::Any,
            Self::Primitive(PrimitiveType::Float) => TypeKind::Float,
            Self::Primitive(PrimitiveType::Boolean) => TypeKind::Boolean,
            Self::Primitive(PrimitiveType::String) => TypeKind::String,
            Self::Primitive(PrimitiveType::Component) => TypeKind::Component,
            Self::Primitive(PrimitiveType::Void) => TypeKind::Void,
            Self::Primitive(PrimitiveType::Null) => panic!("null was deprecated in favor of Option<T>"),
            Self::Primitive(_) => TypeKind::Integer,
            Self::This => panic!("<self> should not be accessible as type-kind"),
            Self::Complex(ty) => match &**ty {
                ComplexType::Function(_) => TypeKind::Function,
                ComplexType::Component(_) => TypeKind::Component,
                ComplexType::TraitInstance(ty, _) => ty.variant.kind(),
                ComplexType::Struct(_) => TypeKind::Struct,
                ComplexType::EnumType(_) => TypeKind::Enum,
                ComplexType::Array(array) => TypeKind::Array(Box::new(array.element.variant.kind()), array.size),
                ComplexType::OneOf(types) => TypeKind::OneOf(types.iter().map(|t| t.kind().value).collect()),
            },
            Self::Generic(_) => TypeKind::Generic,
            Self::Trait(_) => TypeKind::Trait,
            Self::Ref { ty, .. } => ty.variant.kind(),
        }
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
}

impl TypeVariant {
    pub fn complex(ty: ComplexType) -> Self {
        Self::Complex(Arc::new(ty))
    }

    pub const fn any() -> Self {
        Self::Primitive(PrimitiveType::Any)
    }

    pub const fn isize() -> Self {
        Self::Primitive(PrimitiveType::ISize)
    }

    pub const fn int64() -> Self {
        Self::Primitive(PrimitiveType::I64)
    }

    pub const fn int32() -> Self {
        Self::Primitive(PrimitiveType::I32)
    }

    pub const fn int16() -> Self {
        Self::Primitive(PrimitiveType::I16)
    }

    pub const fn int8() -> Self {
        Self::Primitive(PrimitiveType::I8)
    }

    pub const fn usize() -> Self {
        Self::Primitive(PrimitiveType::USize)
    }

    pub const fn uint64() -> Self {
        Self::Primitive(PrimitiveType::U64)
    }

    pub const fn uint32() -> Self {
        Self::Primitive(PrimitiveType::U32)
    }

    pub const fn uint16() -> Self {
        Self::Primitive(PrimitiveType::U16)
    }

    pub const fn uint8() -> Self {
        Self::Primitive(PrimitiveType::U8)
    }

    pub const fn float() -> Self {
        Self::Primitive(PrimitiveType::Float)
    }

    pub const fn string() -> Self {
        Self::Primitive(PrimitiveType::String)
    }

    pub const fn null() -> Self {
        Self::Primitive(PrimitiveType::Null)
    }

    pub const fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub const fn void() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }

    pub fn borrow<T: Into<Type>>(value: T) -> Self {
        Self::Ref {
            ty: Box::new(value.into()),
            mutable: false,
        }
    }

    pub fn borrow_mut<T: Into<Type>>(value: T) -> Self {
        Self::Ref {
            ty: Box::new(value.into()),
            mutable: true,
        }
    }

    pub fn one_of<T: IntoIterator<Item = R>, R: Into<Type>>(types: T) -> Self {
        Self::complex(ComplexType::OneOf(types.into_iter().map(Into::into).collect()))
    }

    pub fn function_ir<T: Into<Type>, I: IntoIterator<Item = Self>, R: Into<Type>>(
        isa: &dyn TargetIsa,
        this: Option<T>,
        args: I,
        returns: R,
    ) -> (Self, Signature) {
        let this = this.map(Into::into);
        let args: Vec<Type> = args.into_iter().map(Into::into).collect();
        let returns = returns.into();
        let mut signature = Signature::new(isa.default_call_conv());

        if let Some(this) = &this {
            signature.params.push(AbiParam::new(this.variant.as_ir_type(isa)));
        }

        for arg in &args {
            signature.params.push(AbiParam::new(arg.variant.as_ir_type(isa)));
        }

        if !matches!(returns.variant, Self::Primitive(PrimitiveType::Void)) {
            signature.returns.push(AbiParam::new(returns.variant.as_ir_type(isa)));
        }

        (
            Self::complex(ComplexType::Function(FunctionType {
                is_native: true,
                this,
                args,
                returns: Box::new(returns),
            })),
            signature,
        )
    }

    pub fn function_with_self<T: Into<Type>, I: IntoIterator<Item = Self>, R: Into<Type>>(this: T, args: I, returns: R) -> Self {
        Self::complex(ComplexType::Function(FunctionType {
            is_native: true,
            this: Some(this.into()),
            args: args.into_iter().map(Into::into).collect(),
            returns: Box::new(returns.into()),
        }))
    }

    pub fn function<I: IntoIterator<Item = Self>, R: Into<Type>>(args: I, returns: R) -> Self {
        Self::complex(ComplexType::Function(FunctionType {
            is_native: true,
            this: None,
            args: args.into_iter().map(Into::into).collect(),
            returns: Box::new(returns.into()),
        }))
    }

    pub fn structure_ir<K: Into<String>, V: Into<Type>, T: IntoIterator<Item = (K, V)>>(isa: &dyn TargetIsa, properties: T) -> Self {
        let properties: Vec<(String, Type)> = properties.into_iter().map(|(key, value)| (key.into(), value.into())).collect();

        Self::complex(ComplexType::Struct(StructType {
            structure: Struct::new(properties.iter().map(|prop| (prop.1.variant.as_ir_type(isa), None))),
            generics: Vec::new(),
            properties,
        }))
    }

    pub fn enumeration<K: Into<String>, T: IntoIterator<Item = K>>(variants: T) -> Self {
        let variants: Vec<(String, EnumVariant)> = variants
            .into_iter()
            .map(|key| {
                (key.into(), EnumVariant {
                    properties: None,
                    structure: None,
                })
            })
            .collect();

        Self::complex(ComplexType::EnumType(EnumType {
            variants,
            generics: Vec::new(),
        }))
    }

    pub fn structure<K: Into<String>, V: Into<Type>, T: IntoIterator<Item = (K, V)>>(properties: T) -> Self {
        let properties = properties.into_iter().map(|(key, value)| (key.into(), value.into())).collect();

        Self::complex(ComplexType::Struct(StructType {
            structure: Struct::default(),
            generics: Vec::new(),
            properties,
        }))
    }

    pub fn component<K: Into<String>, V: Into<Type>, T: IntoIterator<Item = (K, bool, V)>>(properties: T, children: Option<Type>) -> Self {
        Self::complex(ComplexType::Component(ComponentType {
            structure: Struct::default(),
            properties: properties
                .into_iter()
                .map(|(key, nullable, value)| (key.into(), nullable, value.into()))
                .collect(),
            children,
        }))
    }

    pub fn same_as(&self, expected: &Self, applied_generics: &[Type]) -> bool {
        if let (Self::Generic(a), Self::Generic(b)) = (self, expected) {
            return a == b;
        }

        let me = if let Self::Generic(generic) = self {
            &applied_generics[*generic].variant
        } else {
            self
        };

        let expected = if let Self::Generic(generic) = expected {
            &applied_generics[*generic].variant
        } else {
            expected
        };

        if matches!(expected, Self::Primitive(PrimitiveType::Any)) || matches!(me, Self::Primitive(PrimitiveType::Null)) {
            true
        } else {
            match (me, expected) {
                (Self::Primitive(a), Self::Primitive(b)) => a == b,
                (Self::Generic(a), Self::Generic(b)) | (Self::Trait(a), Self::Trait(b)) => a == b,
                (a, Self::Complex(b)) => {
                    if let ComplexType::OneOf(types) = b.as_ref() {
                        if let Some(ComplexType::OneOf(a_types)) = a.as_complex()
                            && a_types == types
                        {
                            true
                        } else {
                            for ty in types {
                                if a.same_as(&ty.variant, applied_generics) {
                                    return true;
                                }
                            }

                            false
                        }
                    } else if let Self::Complex(a) = a {
                        match (a.as_ref(), b.as_ref()) {
                            (ComplexType::Array(got), ComplexType::Array(expected)) => {
                                got.element.variant.same_as(&expected.element.variant, applied_generics)
                                    && expected.size.is_none_or(|size| got.size.is_some_and(|got| got == size))
                            }
                            (a, b) => a == b,
                        }
                    } else {
                        false
                    }
                }
                v => false,
            }
        }
    }

    pub fn as_enum(&self) -> Option<&EnumType> {
        self.as_complex()
            .and_then(|complex| if let ComplexType::EnumType(ty) = complex { Some(ty) } else { None })
    }

    pub fn as_struct(&self) -> Option<&StructType> {
        self.as_complex()
            .and_then(|complex| if let ComplexType::Struct(ty) = complex { Some(ty) } else { None })
    }

    pub fn as_component(&self) -> Option<&ComponentType> {
        self.as_complex()
            .and_then(|complex| if let ComplexType::Component(ty) = complex { Some(ty) } else { None })
    }

    pub fn as_trait_instance(&self) -> Option<(&Type, usize)> {
        self.as_complex().and_then(|complex| {
            if let ComplexType::TraitInstance(ty, index) = complex {
                Some((ty, *index))
            } else {
                None
            }
        })
    }

    pub fn as_array(&self) -> Option<&ArrayType> {
        self.as_complex()
            .and_then(|complex| if let ComplexType::Array(ty) = complex { Some(ty) } else { None })
    }

    pub fn as_function(&self) -> Option<&FunctionType> {
        self.as_complex()
            .and_then(|complex| if let ComplexType::Function(ty) = complex { Some(ty) } else { None })
    }

    pub fn is_enum(&self) -> bool {
        self.as_complex().is_some_and(|complex| matches!(complex, ComplexType::EnumType(_)))
    }

    pub fn is_struct(&self) -> bool {
        self.as_complex().is_some_and(|complex| matches!(complex, ComplexType::Struct(_)))
    }

    pub fn is_component(&self) -> bool {
        self.as_complex().is_some_and(|complex| matches!(complex, ComplexType::Component(_)))
    }

    pub fn is_array(&self) -> bool {
        self.as_complex().is_some_and(|complex| matches!(complex, ComplexType::Array(_)))
    }

    pub fn is_function(&self) -> bool {
        self.as_complex().is_some_and(|complex| matches!(complex, ComplexType::Function(_)))
    }

    pub fn as_complex(&self) -> Option<&ComplexType> {
        if let Self::Complex(complex_type) = self {
            Some(&**complex_type)
        } else {
            None
        }
    }

    pub fn as_ir_type(&self, isa: &dyn TargetIsa) -> ir::Type {
        match self {
            Self::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => unimplemented!(),
                PrimitiveType::I64 | PrimitiveType::U64 => ir::types::I64,
                PrimitiveType::I32 | PrimitiveType::U32 => ir::types::I32,
                PrimitiveType::I16 | PrimitiveType::U16 => ir::types::I16,
                PrimitiveType::I8 | PrimitiveType::U8 | PrimitiveType::Boolean => ir::types::I8,
                PrimitiveType::Float => ir::types::F32,
                PrimitiveType::ISize | PrimitiveType::USize | PrimitiveType::String | PrimitiveType::Component => isa.pointer_type(),
                PrimitiveType::Void => unimplemented!(),
                PrimitiveType::Null => unimplemented!(),
            },
            Self::This | Self::Generic(_) | Self::Trait(_) | Self::Complex(_) => isa.pointer_type(),
            Self::Ref { ty, .. } => ty.variant.as_ir_type(isa),
        }
    }

    pub fn as_abi_param(&self, isa: &dyn TargetIsa) -> ir::AbiParam {
        ir::AbiParam::new(self.as_ir_type(isa))
    }
}
