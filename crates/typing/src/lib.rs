mod array;
mod component;
mod enumeration;
mod function;
mod kind;
mod primitive;
mod structure;

use std::{
    fmt::{self, Write},
    sync::Arc,
};

use cranelift::{
    codegen::ir,
    prelude::{FunctionBuilder, InstBuilder, MemFlags, isa::TargetIsa},
};
use mollie_shared::{MaybePositioned, Span, SpanType, cranelift::stack_alloc, pretty_fmt::PrettyFmt};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub use self::{
    array::ArrayType,
    component::{ComponentChildren, ComponentType},
    enumeration::{EnumType, EnumVariant},
    function::FunctionType,
    kind::TypeKind,
    primitive::PrimitiveType,
    structure::{Struct, StructType},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitFunc {
    pub name: String,
    pub this: bool,
    pub args: Vec<Type>,
    pub returns: Type,
    pub signature: ir::SigRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub generics: Vec<String>,
    pub functions: Vec<TraitFunc>,
    pub declared_at: Option<Span>,
}

pub fn array_of<T: Into<Type>>(element: T, size: Option<usize>) -> TypeVariant {
    TypeVariant::complex(ComplexType::Array(ArrayType { element: element.into(), size }))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum ComplexType {
    Function(FunctionType),
    Component(ComponentType),
    Struct(StructType),
    EnumType(EnumType),
    Array(ArrayType),
    TraitInstance(Type, usize),
    OneOf(Vec<Type>),
}

impl fmt::Display for ComplexType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function(value) => value.fmt(f),
            Self::Component(value) => value.fmt(f),
            Self::Struct(value) => value.fmt(f),
            Self::EnumType(value) => value.fmt(f),
            Self::Array(value) => value.fmt(f),
            Self::TraitInstance(value, _) => value.fmt(f),
            Self::OneOf(v) => {
                let mut first = true;

                for v in v {
                    if first {
                        v.fmt(f)?;

                        first = false;
                    } else {
                        write!(f, " | {v}")?;
                    }
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum TypeVariant {
    Primitive(PrimitiveType),
    Complex(Arc<ComplexType>),
    Trait(usize),
    Generic(usize),
}

impl TypeVariant {
    pub fn of<T: TypeVariantOf>() -> Self {
        T::type_variant_of()
    }

    pub fn kind(&self) -> TypeKind {
        match self {
            Self::Primitive(PrimitiveType::Any) => TypeKind::Any,
            Self::Primitive(PrimitiveType::Float) => TypeKind::Float,
            Self::Primitive(PrimitiveType::Boolean) => TypeKind::Boolean,
            Self::Primitive(PrimitiveType::String) => TypeKind::String,
            Self::Primitive(PrimitiveType::Component) => TypeKind::Component,
            Self::Primitive(PrimitiveType::Void) => TypeKind::Void,
            Self::Primitive(PrimitiveType::Null) => TypeKind::Null,
            Self::Primitive(_) => TypeKind::Integer,
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
            Self::Trait(_) => unimplemented!(),
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

impl fmt::Display for TypeVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(primitive) => primitive.fmt(f),
            Self::Complex(complex) => complex.fmt(f),
            Self::Generic(_) => f.write_str("<generic>"),
            Self::Trait(_) => f.write_str("<trait>"),
        }
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

    pub fn one_of<T: IntoIterator<Item = R>, R: Into<Type>>(types: T) -> Self {
        Self::complex(ComplexType::OneOf(types.into_iter().map(Into::into).collect()))
    }

    pub fn function<T: IntoIterator<Item = Self>, R: Into<Type>>(have_self: bool, args: T, returns: R) -> Self {
        Self::complex(ComplexType::Function(FunctionType {
            is_native: true,
            have_self,
            args: args.into_iter().map(Into::into).collect(),
            returns: Box::new(returns.into()),
        }))
    }

    pub fn structure<K: Into<String>, V: Into<Type>, T: IntoIterator<Item = (K, V)>>(properties: T) -> Self {
        Self::complex(ComplexType::Struct(StructType {
            structure: Struct::default(),
            generics: Vec::new(),
            properties: properties.into_iter().map(|(key, value)| (key.into(), value.into())).collect(),
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
                (Self::Generic(a), Self::Generic(b)) => a == b,
                (me, Self::Complex(b)) => {
                    if let ComplexType::OneOf(types) = b.as_ref() {
                        if let Some(ComplexType::OneOf(a_types)) = me.as_complex()
                            && a_types == types
                        {
                            true
                        } else {
                            for ty in types {
                                if me.same_as(&ty.variant, applied_generics) {
                                    return true;
                                }
                            }

                            false
                        }
                    } else if let Self::Complex(a) = me {
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
                _ => false,
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
            Self::Trait(_) | Self::Complex(_) => isa.pointer_type(),
            Self::Generic(_) => unimplemented!(),
        }
    }
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
    fn set_first_generic(&mut self, value: Self) {
        if self.applied_generics.is_empty() {
            self.applied_generics.push(value);
        } else {
            self.applied_generics[0] = value;
        }
    }

    pub fn inherit_from(&mut self, other: &TypeVariant) {
        if let Some(array) = other.as_array() {
            match &self.variant {
                TypeVariant::Complex(complex_type) => match &**complex_type {
                    ComplexType::Function(function_type) => {
                        if function_type.args.iter().any(|arg| matches!(arg.variant, TypeVariant::Generic(0)))
                            || matches!(function_type.returns.variant, TypeVariant::Generic(0))
                        {
                            self.set_first_generic(array.element.clone());
                        }
                    }
                    ComplexType::Struct(struct_type) => {
                        if !struct_type.generics.is_empty() {
                            self.set_first_generic(array.element.clone());
                        }
                    }
                    ComplexType::Array(array_type) => {
                        if matches!(array_type.element.variant, TypeVariant::Generic(0)) {
                            self.set_first_generic(array.element.clone());
                        }
                    }
                    ComplexType::OneOf(items) => {
                        if items.iter().any(|item| matches!(item.variant, TypeVariant::Generic(0))) {
                            self.set_first_generic(array.element.clone());
                        }
                    }
                    _ => {
                        // if !component_type.generics.is_empty() {
                        //     self.applied_generics[0] = array.element.clone();
                        // }
                    }
                },
                TypeVariant::Generic(0) => self.set_first_generic(array.element.clone()),
                _ => {}
            }
        }
    }

    #[must_use]
    pub fn resolve_type(mut self, applied_generics: &[Self]) -> Self {
        if let TypeVariant::Generic(generic) = self.variant {
            applied_generics[generic].clone()
        } else if let TypeVariant::Complex(variant) = &self.variant {
            if let ComplexType::OneOf(types) = &**variant {
                let mut result_types = Vec::with_capacity(types.len());

                for ty in types {
                    result_types.push(ty.clone().resolve_type(applied_generics));
                }

                Self {
                    variant: TypeVariant::complex(ComplexType::OneOf(result_types)),
                    applied_generics: self.applied_generics,
                    declared_at: self.declared_at,
                }
            } else if self.variant.is_enum() || self.variant.is_struct() {
                self.applied_generics = applied_generics.to_vec();

                self
            } else {
                self
            }
        } else {
            self
        }
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
            applied_generics: Vec::new(),
            variant: value,
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

pub trait TypeVariantOf {
    fn type_variant_of() -> TypeVariant;
}

impl TypeVariantOf for () {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::void()
    }
}

impl TypeVariantOf for i8 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::int8()
    }
}

impl TypeVariantOf for u8 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::uint8()
    }
}

impl TypeVariantOf for i16 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::int16()
    }
}

impl TypeVariantOf for u16 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::uint16()
    }
}

impl TypeVariantOf for i32 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::int32()
    }
}

impl TypeVariantOf for u32 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::uint32()
    }
}

impl TypeVariantOf for i64 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::int64()
    }
}

impl TypeVariantOf for u64 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::uint64()
    }
}

impl TypeVariantOf for isize {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::isize()
    }
}

impl TypeVariantOf for usize {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::usize()
    }
}

impl TypeVariantOf for f32 {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::float()
    }
}

impl TypeVariantOf for bool {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::boolean()
    }
}

impl TypeVariantOf for &str {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::string()
    }
}

impl TypeVariantOf for String {
    fn type_variant_of() -> TypeVariant {
        TypeVariant::string()
    }
}

impl<T: TypeVariantOf> From<T> for Type {
    fn from(_: T) -> Self {
        T::type_variant_of().into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FatPtr;

impl FatPtr {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, ptr: ir::Value, metadata: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();
        let slot = stack_alloc(fn_builder, ptr_type.bytes() * 2);

        fn_builder.ins().stack_store(ptr, slot, 0);
        fn_builder.ins().stack_store(metadata, slot, ptr_type.bytes().cast_signed());

        fn_builder.ins().stack_addr(ptr_type, slot, 0)
    }

    pub fn get_ptr(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, fat_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder.ins().load(ptr_type, MemFlags::trusted(), fat_ptr, 0)
    }

    pub fn get_metadata(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, fat_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder.ins().load(ptr_type, MemFlags::trusted(), fat_ptr, ptr_type.bytes().cast_signed())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VTablePtr;

impl VTablePtr {
    pub fn get_type_idx(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, vtable_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder.ins().load(ptr_type, MemFlags::trusted(), vtable_ptr, 0)
    }

    pub fn get_func_ptr(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, vtable_ptr: ir::Value, func_idx: u32) -> ir::Value {
        let ptr_type = isa.pointer_type();
        let vtable_ptr = fn_builder.ins().load(ptr_type, MemFlags::trusted(), vtable_ptr, ptr_type.bytes().cast_signed());

        if ptr_type.bytes() * func_idx > 0 {
            let offset = fn_builder.ins().iconst(ptr_type, i64::from(ptr_type.bytes() * func_idx));

            fn_builder.ins().iadd(vtable_ptr, offset)
        } else {
            vtable_ptr
        }
    }
}
