use derive_more::Display;

mod array;
mod component;
mod enumeration;
mod function;
mod structure;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub use self::{
    array::ArrayType,
    component::{ComponentChildren, ComponentType},
    enumeration::{EnumType, EnumVariant},
    function::FunctionType,
    structure::StructType,
};
use crate::Type;

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum ComplexType {
    Function(FunctionType),
    Component(ComponentType),
    Struct(StructType),
    EnumType(EnumType),
    Array(ArrayType),
    #[display("{}", _0)]
    TraitInstance(Type, usize),
    #[display("{}", _0.iter().map(ToString::to_string).collect::<Vec<_>>().join(" | "))]
    OneOf(Vec<Type>),
}
