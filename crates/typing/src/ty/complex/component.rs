use std::fmt::{self, Write};

use mollie_ir::Struct;
use mollie_shared::pretty_fmt::{PrettyFmt, indent_down, indent_up};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum ComponentChildren {
    None,
    Single,
    MaybeSingle,
    Multiple(Option<usize>),
    MaybeMultiple(Option<usize>),
}

impl ComponentChildren {
    pub const fn validate(&self, got: &Self) -> bool {
        // check if valid
        match (self, got) {
            (Self::None, Self::Single | Self::Multiple(_))
            | (Self::Single | Self::Multiple(_), Self::None)
            | (Self::Single | Self::MaybeSingle, Self::Multiple(_)) => false,
            (Self::None | Self::MaybeSingle | Self::MaybeMultiple(_), Self::None)
            | (Self::Single | Self::MaybeSingle | Self::Multiple(_) | Self::MaybeMultiple(_), Self::Single)
            | (Self::Multiple(_) | Self::MaybeMultiple(_), Self::Multiple(_)) => true,
            (Self::None | Self::Single | Self::MaybeSingle | Self::Multiple(_) | Self::MaybeMultiple(_), Self::MaybeSingle | Self::MaybeMultiple(_)) => {
                unreachable!()
            }
        }
    }
}

impl From<usize> for ComponentChildren {
    fn from(value: usize) -> Self {
        if value == 0 {
            Self::None
        } else if value == 1 {
            Self::Single
        } else {
            Self::Multiple(None)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ComponentType {
    pub properties: Vec<(String, bool, Type)>,
    pub children: Option<Type>,
    #[cfg_attr(feature = "serde", serde(skip_deserializing, skip_serializing))]
    pub structure: Struct,
}

impl fmt::Display for ComponentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("component {\n")?;

        indent_down();

        f.write_array_like(
            self.properties
                .iter()
                .map(|(key, nullable, value)| format!("{key}{}: {value}", if *nullable { "?" } else { "" })),
            true,
        )?;

        indent_up();

        f.write_char('\n')?;
        f.write_indent()?;
        f.write_char('}')
    }
}
