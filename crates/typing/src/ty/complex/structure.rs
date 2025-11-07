use std::fmt::{self, Write};

use mollie_ir::Struct;
use mollie_shared::pretty_fmt::{PrettyFmt, indent_down, indent_up};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct StructType {
    pub generics: Vec<String>,
    pub properties: Vec<(String, Type)>,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub structure: Struct,
}

impl fmt::Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("struct {\n")?;

        indent_down();

        f.write_array_like(self.properties.iter().map(|(key, value)| format!("{key}: {value}")), true)?;

        indent_up();

        f.write_char('\n')?;
        f.write_indent()?;
        f.write_char('}')
    }
}

#[allow(dead_code)]
fn size_of_struct(fields: &[cranelift::prelude::Type]) -> u32 {
    let mut size = 0;

    // Go through all fields and incement size by each fields size and padding
    for &field in fields {
        size += field.bytes();

        // Add padding to ensure the field is aligned
        let align = field.bytes();
        let padding = (align - size % align) % align;
        size += padding;
    }

    // Add padding to the end of the struct to make the struct itself aligned
    let self_align = alignment_of_struct(fields.iter());
    let end_padding = (self_align - size % self_align) % self_align;

    size += end_padding;

    size
}

#[allow(dead_code)]
fn alignment_of_struct<'a, T: Iterator<Item = &'a cranelift::prelude::Type>>(fields: T) -> u32 {
    let mut alignment = 0;

    for field in fields {
        alignment = alignment.max(field.bytes());
    }

    alignment
}

#[allow(dead_code)]
fn offset_of_field(field: usize, fields: &[cranelift::prelude::Type]) -> i32 {
    let mut offset = 0;

    // Go through all fields prior to this one and increment offset by their size
    // and padding
    for &prior in fields.iter().take(field) {
        offset += prior.bytes().cast_signed();

        // Add padding to ensure the field is aligned
        let align = prior.bytes().cast_signed();
        let padding = (align - offset % align) % align;

        offset += padding;
    }

    offset
}
