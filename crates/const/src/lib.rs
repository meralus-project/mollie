use std::{
    hash::{Hash, Hasher},
    mem::discriminant,
};

use mollie_parser::{Expr, LiteralExpr};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ConstantValue {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    ISize(isize),
    USize(usize),
    Float(f32),
    Boolean(bool),
    Array(Box<[Self]>),
    String(String),
    Nothing,
}

impl Eq for ConstantValue {}

impl Hash for ConstantValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);
    }
}

impl From<i8> for ConstantValue {
    fn from(value: i8) -> Self {
        Self::I8(value)
    }
}

impl From<u8> for ConstantValue {
    fn from(value: u8) -> Self {
        Self::U8(value)
    }
}

impl From<i16> for ConstantValue {
    fn from(value: i16) -> Self {
        Self::I16(value)
    }
}

impl From<u16> for ConstantValue {
    fn from(value: u16) -> Self {
        Self::U16(value)
    }
}

impl From<i32> for ConstantValue {
    fn from(value: i32) -> Self {
        Self::I32(value)
    }
}

impl From<u32> for ConstantValue {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}

impl From<i64> for ConstantValue {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<u64> for ConstantValue {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}

impl From<isize> for ConstantValue {
    fn from(value: isize) -> Self {
        Self::ISize(value)
    }
}

impl From<usize> for ConstantValue {
    fn from(value: usize) -> Self {
        Self::USize(value)
    }
}

impl From<f32> for ConstantValue {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}

impl From<bool> for ConstantValue {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl ConstantValue {
    pub const fn is_constant(value: &Expr) -> bool {
        matches!(
            value,
            Expr::Literal(LiteralExpr::Boolean(_) | LiteralExpr::Number(_, _) | LiteralExpr::String(_))
        )
    }

    pub fn to_constant(value: &Expr) -> Option<Self> {
        use Expr::Literal;
        use LiteralExpr::{Boolean, Number, String};
        use mollie_parser::Number::{F32, I64};

        match value {
            &Literal(Boolean(value)) => Some(Self::Boolean(value)),
            Literal(Number(value, suffix)) => match *value {
                I64(value) => match suffix.as_deref() {
                    Some("int_size") => Some(Self::ISize(value.try_into().ok()?)),
                    Some("uint_size") => Some(Self::USize(value.try_into().ok()?)),
                    Some("int64") => Some(Self::I64(value)),
                    Some("uint64") => Some(Self::U64(value.try_into().ok()?)),
                    Some("uint32") => Some(Self::U32(value.try_into().ok()?)),
                    Some("int16") => Some(Self::I16(value.try_into().ok()?)),
                    Some("uint16") => Some(Self::U16(value.try_into().ok()?)),
                    Some("int8") => Some(Self::I8(value.try_into().ok()?)),
                    Some("uint8") => Some(Self::U8(value.try_into().ok()?)),
                    _ => Some(Self::I32(value.try_into().ok()?)),
                },
                F32(value) => Some(Self::Float(value)),
            },
            Literal(String(value)) => Some(Self::String(value.clone())),
            _ => None,
        }
    }
}
