use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanType {
    Simple,
    Definition,
}

impl fmt::Display for SpanType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple => f.write_str("at"),
            Self::Definition => f.write_str("defined at"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MaybePositioned<T> {
    pub value: T,
    pub span: Option<Span>,
}

impl<T> MaybePositioned<T> {
    pub const fn new(value: T, span: Option<Span>) -> Self {
        Self { value, span }
    }
}

impl<T> From<T> for MaybePositioned<T> {
    fn from(value: T) -> Self {
        Self { value, span: None }
    }
}
