use cranelift::{
    codegen::ir,
    module::{DataDescription, Module},
    prelude::{FunctionBuilder, InstBuilder, isa::TargetIsa},
};
use mollie_const::ConstantValue;

use crate::{FatPtr, stack_alloc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub ty: ir::Type,
    pub offset: i32,
    pub default_value: Option<ConstantValue>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub size: u32,
}

impl Struct {
    pub fn new<T: IntoIterator<Item = (ir::Type, Option<ConstantValue>)>>(fields_iter: T) -> Self {
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
    pub fn default_for<T: Module>(&self, module: &mut T, data_desc: &mut DataDescription, fn_builder: &mut FunctionBuilder, field: usize) -> Option<ir::Value> {
        self.fields[field]
            .default_value
            .as_ref()
            .and_then(|default| compile_constant(default, module, data_desc, fn_builder))
    }

    pub fn instance<T: IntoIterator<Item = ir::Value>>(&self, isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, values: T) -> ir::Value {
        let slot = stack_alloc(fn_builder, self.size);

        for (field, value) in self.fields.iter().zip(values) {
            fn_builder.ins().stack_store(value, slot, field.offset);
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}

pub fn compile_constant<M: Module>(
    constant: &ConstantValue,
    module: &mut M,
    data_desc: &mut DataDescription,
    fn_builder: &mut FunctionBuilder,
) -> Option<ir::Value> {
    Some(match constant {
        &ConstantValue::I8(value) => fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
        &ConstantValue::U8(value) => fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
        &ConstantValue::I16(value) => fn_builder.ins().iconst(ir::types::I16, i64::from(value)),
        &ConstantValue::U16(value) => fn_builder.ins().iconst(ir::types::I16, i64::from(value)),
        &ConstantValue::I32(value) => fn_builder.ins().iconst(ir::types::I32, i64::from(value)),
        &ConstantValue::U32(value) => fn_builder.ins().iconst(ir::types::I32, i64::from(value)),
        &ConstantValue::I64(value) => fn_builder.ins().iconst(ir::types::I64, value),
        &ConstantValue::U64(value) => fn_builder.ins().iconst(ir::types::I64, i64::try_from(value).ok()?),
        &ConstantValue::ISize(value) => fn_builder.ins().iconst(module.isa().pointer_type(), i64::try_from(value).ok()?),
        &ConstantValue::USize(value) => fn_builder.ins().iconst(module.isa().pointer_type(), i64::try_from(value).ok()?),
        &ConstantValue::Float(value) => fn_builder.ins().f32const(value),
        &ConstantValue::Boolean(value) => fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
        ConstantValue::Array(values) => {
            let element = match values.first()? {
                ConstantValue::I8(_) | ConstantValue::U8(_) | ConstantValue::Boolean(_) => ir::types::I8,
                ConstantValue::I16(_) | ConstantValue::U16(_) => ir::types::I16,
                ConstantValue::I32(_) | ConstantValue::U32(_) => ir::types::I32,
                ConstantValue::I64(_) | ConstantValue::U64(_) => ir::types::I64,
                ConstantValue::ISize(_) | ConstantValue::USize(_) | ConstantValue::Array(_) | ConstantValue::String(_) => module.isa().pointer_type(),
                ConstantValue::Float(_) => ir::types::F32,
                ConstantValue::Nothing => ir::types::INVALID,
            };

            let values = values
                .iter()
                .filter_map(|constant| compile_constant(constant, module, data_desc, fn_builder))
                .collect::<Box<[_]>>();

            Array { element }.instance(module.isa(), fn_builder, values)
        }
        ConstantValue::String(value) => {
            let len = value.len();

            data_desc.define(value.clone().into_bytes().into_boxed_slice());

            let id = module.declare_anonymous_data(true, false).unwrap();

            module.define_data(id, data_desc).unwrap();
            data_desc.clear();

            let data_id = module.declare_data_in_func(id, fn_builder.func);

            let ptr = fn_builder.ins().symbol_value(module.isa().pointer_type(), data_id);
            let size = fn_builder.ins().iconst(module.isa().pointer_type(), len.cast_signed() as i64);

            FatPtr::new(module.isa(), fn_builder, ptr, size)
        }
        ConstantValue::Nothing => return None,
    })
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Array {
    pub element: ir::Type,
}

#[allow(clippy::cast_possible_truncation)]
impl Array {
    pub fn get_size(&self, values: usize) -> u32 {
        self.element.bytes() * values as u32
    }

    pub fn get_offset_of(&self, index: usize) -> i32 {
        self.element.bytes().cast_signed() * index.cast_signed() as i32
    }

    pub fn instance<II: ExactSizeIterator + Iterator<Item = ir::Value>, I: IntoIterator<Item = ir::Value, IntoIter = II>>(
        &self,
        isa: &dyn TargetIsa,
        fn_builder: &mut FunctionBuilder,
        values: I,
    ) -> ir::Value {
        let values = values.into_iter();
        let slot = stack_alloc(fn_builder, self.get_size(values.len()));

        for (index, value) in values.enumerate() {
            fn_builder.ins().stack_store(value, slot, self.get_offset_of(index));
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}
