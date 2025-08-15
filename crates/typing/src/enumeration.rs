use std::fmt::{self, Write};

use mollie_shared::pretty_fmt::PrettyFmt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct EnumVariant {
    pub properties: Option<Vec<(String, Type)>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct EnumType {
    pub generics: Vec<String>,
    pub variants: Vec<(String, EnumVariant)>,
}

impl fmt::Display for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("enum {\n")?;

        // indent_down();

        // f.write_array_like(self.variants.iter().map(|(key, value)| format!("{key}:
        // {value}")), true)?;

        // indent_up();

        f.write_char('\n')?;
        f.write_indent()?;
        f.write_char('}')
    }
}
