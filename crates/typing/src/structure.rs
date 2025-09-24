use std::fmt::{self, Write};

use cranelift::{
    jit::JITModule,
    module::{DataDescription, Module},
    prelude::{FunctionBuilder, InstBuilder, Value, isa::TargetIsa, types},
};
use mollie_const::ConstantValue;
use mollie_shared::{
    cranelift::stack_alloc,
    pretty_fmt::{PrettyFmt, indent_down, indent_up},
};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{FatPtr, Type};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub size: u32,
}

impl Struct {
    pub fn new<T: IntoIterator<Item = (cranelift::prelude::Type, Option<ConstantValue>)>>(fields_iter: T) -> Self {
        let mut fields = Vec::new();
        let mut offset = 0;
        let mut size = 0;
        let mut self_align = 0;

        for (ty, default_value) in fields_iter {
            size += ty.bytes();

            let align = ty.bytes();
            let padding = (align - size % align) % align;

            size += padding;

            fields.push(Field { ty, offset, default_value });

            offset += ty.bytes().cast_signed();

            let align = ty.bytes().cast_signed();
            let padding = (align - offset % align) % align;

            offset += padding;
            self_align = self_align.max(ty.bytes());
        }

        Self {
            fields,
            size: size + (self_align - size % self_align) % self_align,
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn default_for(&self, module: &mut JITModule, data_desc: &mut DataDescription, fn_builder: &mut FunctionBuilder, field: usize) -> Option<Value> {
        if let Some(default) = &self.fields[field].default_value {
            Some(match default {
                &ConstantValue::I8(value) => fn_builder.ins().iconst(types::I8, i64::from(value)),
                &ConstantValue::U8(value) => fn_builder.ins().iconst(types::I8, i64::from(value)),
                &ConstantValue::I16(value) => fn_builder.ins().iconst(types::I16, i64::from(value)),
                &ConstantValue::U16(value) => fn_builder.ins().iconst(types::I16, i64::from(value)),
                &ConstantValue::I32(value) => fn_builder.ins().iconst(types::I32, i64::from(value)),
                &ConstantValue::U32(value) => fn_builder.ins().iconst(types::I32, i64::from(value)),
                &ConstantValue::I64(value) => fn_builder.ins().iconst(types::I64, value),
                &ConstantValue::U64(value) => fn_builder.ins().iconst(types::I64, i64::try_from(value).ok()?),
                &ConstantValue::ISize(value) => fn_builder.ins().iconst(module.isa().pointer_type(), i64::try_from(value).ok()?),
                &ConstantValue::USize(value) => fn_builder.ins().iconst(module.isa().pointer_type(), i64::try_from(value).ok()?),
                &ConstantValue::Float(value) => fn_builder.ins().f32const(value),
                &ConstantValue::Boolean(value) => fn_builder.ins().iconst(types::I8, i64::from(value)),
                ConstantValue::String(value) => {
                    let len = value.len();

                    data_desc.define(value.clone().into_bytes().into_boxed_slice());

                    let id = module.declare_anonymous_data(true, false).unwrap();

                    module.define_data(id, data_desc).unwrap();
                    data_desc.clear();
                    module.finalize_definitions().unwrap();

                    let data_id = module.declare_data_in_func(id, fn_builder.func);

                    let ptr = fn_builder.ins().symbol_value(module.isa().pointer_type(), data_id);
                    let size = fn_builder.ins().iconst(module.isa().pointer_type(), len.cast_signed() as i64);

                    FatPtr::new(module.isa(), fn_builder, ptr, size)
                }
            })
        } else {
            None
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
    pub default_value: Option<ConstantValue>,
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
