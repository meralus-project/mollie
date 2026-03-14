mod maybe_positioned;
mod positioned;
pub mod pretty_fmt;
mod span;

use std::fmt;

use serde::Serialize;

pub use self::{
    maybe_positioned::{MaybePositioned, SpanType},
    positioned::Positioned,
    span::{Span, SpanRange},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LangItem {
    IntoIterator,
    IntoIteratorIntoIter,
    Iterator,
    IteratorNext,
    Option,
    OptionSome,
    OptionNone,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Assign,           // =
    Add,              // +
    AddAssign,        // +=
    Sub,              // -
    SubAssign,        // -=
    Mul,              // *
    MulAssign,        // *=
    Div,              // /
    DivAssign,        // /=
    Equal,            // ==
    NotEqual,         // !=
    LessThan,         // <
    LessThanEqual,    // <=
    GreaterThan,      // >
    GreaterThanEqual, // >=
    And,              // &&
    Or,               // ||
    BitAnd,           // &
    BitAndAssign,     // &=
    BitOr,            // |
    BitOrAssign,      // |=
    Is,               // is
}

impl Operator {
    pub const fn lower(self) -> Option<Self> {
        match self {
            Self::AddAssign => Some(Self::Add),
            Self::SubAssign => Some(Self::Sub),
            Self::MulAssign => Some(Self::Mul),
            Self::DivAssign => Some(Self::Div),
            Self::BitAndAssign => Some(Self::BitAnd),
            Self::BitOrAssign => Some(Self::BitOr),
            _ => None,
        }
    }

    pub const fn need_same_types(&self) -> bool {
        !matches!(self, Self::Assign | Self::Is)
    }

    pub const fn produce_same_type(&self) -> bool {
        matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Assign => "=",
            Self::Add => "+",
            Self::AddAssign => "+=",
            Self::Sub => "-",
            Self::SubAssign => "-=",
            Self::Mul => "*",
            Self::MulAssign => "*=",
            Self::Div => "/",
            Self::DivAssign => "/=",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::LessThanEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanEqual => ">=",
            Self::And => "&&",
            Self::Or => "||",
            Self::BitAnd => "&",
            Self::BitAndAssign => "&=",
            Self::BitOr => "|",
            Self::BitOrAssign => "|=",
            Self::Is => "is",
        })
    }
}
