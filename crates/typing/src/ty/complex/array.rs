use std::fmt;

use mollie_ir::Array;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ArrayType {
    pub element: Type,
    pub size: Option<usize>,
    #[cfg_attr(feature = "serde", serde(skip_deserializing, skip_serializing))]
    pub array: Array,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.element, self.size.as_ref().map_or_else(String::new, ToString::to_string))
    }
}
