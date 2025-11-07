use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum PrimitiveType {
    Any,
    ISize,
    I64,
    I32,
    I16,
    I8,
    USize,
    U64,
    U32,
    U16,
    U8,
    Float,
    Boolean,
    String,
    Component,
    Void,
    Null,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("<any>"),
            Self::ISize => f.write_str("isize"),
            Self::I64 => f.write_str("i64"),
            Self::I32 => f.write_str("i32"),
            Self::I16 => f.write_str("i16"),
            Self::I8 => f.write_str("i8"),
            Self::USize => f.write_str("usize"),
            Self::U64 => f.write_str("u64"),
            Self::U32 => f.write_str("u32"),
            Self::U16 => f.write_str("u16"),
            Self::U8 => f.write_str("u8"),
            Self::Float => f.write_str("float"),
            Self::Boolean => f.write_str("boolean"),
            Self::String => f.write_str("string"),
            Self::Component => f.write_str("component"),
            Self::Void => f.write_str("void"),
            Self::Null => f.write_str("null"),
        }
    }
}
