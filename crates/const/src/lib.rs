use std::hash::Hash;

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
    String(String),
}

impl Eq for ConstantValue {}

impl Hash for ConstantValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
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
