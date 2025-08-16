use std::fmt::{self, Write};

use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, isa::TargetIsa};
use mollie_shared::{
    cranelift::stack_alloc,
    pretty_fmt::{PrettyFmt, indent_down, indent_up},
};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub size: u32,
}

impl Struct {
    pub fn new<T: IntoIterator<Item = cranelift::prelude::Type>>(fields: T) -> Self {
        let fields = fields.into_iter().collect::<Vec<_>>();
        let size = size_of_struct(&fields);

        Self {
            fields: fields
                .iter()
                .enumerate()
                .map(|(i, ty)| Field {
                    ty: *ty,
                    offset: offset_of_field(i, &fields),
                })
                .collect(),
            size,
        }
    }

    pub fn instance<T: IntoIterator<Item = Value>>(&self, isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, values: T) -> Value {
        let slot = stack_alloc(fn_builder, self.size);

        for (field, value) in self.fields.iter().zip(values) {
            fn_builder.ins().stack_store(value, slot, field.offset);
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub ty: cranelift::prelude::Type,
    pub offset: i32,
}

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
    let self_align = alignment_of_struct(fields);
    let end_padding = (self_align - size % self_align) % self_align;

    size += end_padding;

    size
}

fn alignment_of_struct(fields: &[cranelift::prelude::Type]) -> u32 {
    let mut alignment = 0;

    // Since we don't have nested structs, the allignment of a struct is simply its
    // largest field.
    for &field in fields {
        let field_alignment = field.bytes();

        alignment = alignment.max(field_alignment);
    }

    alignment
}

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
