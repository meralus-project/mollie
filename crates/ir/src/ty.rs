use cranelift::{
    codegen::ir,
    frontend::FuncInstBuilder,
    prelude::{FunctionBuilder, InstBuilder, isa::TargetIsa},
};
use itertools::Itertools;
use mollie_const::ConstantValue;

use crate::{MollieType, stack_alloc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub ty: MollieType,
    pub offset: i32,
    pub default_value: Option<ConstantValue>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub size: u32,
    pub align: u32,
}

impl Struct {
    pub fn new<T: IntoIterator<Item = (MollieType, Option<ConstantValue>)>>(fields_iter: T) -> Self {
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
            size: if size > 0 {
                size + (self_align - size % self_align) % self_align
            } else {
                size
            },
            align: self_align,
        }
    }
}

pub trait ConstantCompiler<'a> {
    fn construct(&mut self, ty: usize, variant: usize, values: Box<[Option<ConstValue>]>) -> Option<ir::Value>;
    fn alloc_string(&mut self, value: String) -> Option<ConstValue>;
    fn alloc_array(&mut self, element: usize, element_type: MollieType, values: Box<[ConstValue]>) -> Option<ConstValue>;
    fn get_array_element(&mut self, ty: usize) -> MollieType;
    fn isa(&self) -> &dyn TargetIsa;
    fn ins<'short>(&'short mut self) -> FuncInstBuilder<'short, 'a>;
}

pub enum ConstValue {
    Value(ir::Value),
    FatPtr(ir::Value, ir::Value),
}

impl IntoIterator for ConstValue {
    type IntoIter = IntoIter;
    type Item = ir::Value;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { inner: Some(self) }
    }
}

pub struct IntoIter {
    inner: Option<ConstValue>,
}

impl Iterator for IntoIter {
    type Item = ir::Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner {
            Some(ConstValue::Value(value)) => {
                self.inner = None;

                Some(value)
            }
            Some(ConstValue::FatPtr(value, metadata)) => {
                self.inner = Some(ConstValue::Value(metadata));

                Some(value)
            }
            None => None,
        }
    }
}

pub fn compile_constant<'a, C: ConstantCompiler<'a>>(element_type: usize, constant: ConstantValue, compiler: &mut C) -> Option<ConstValue> {
    let ptr_type = compiler.isa().pointer_type();

    Some(match constant {
        ConstantValue::I8(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I8, i64::from(value))),
        ConstantValue::U8(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I8, i64::from(value))),
        ConstantValue::I16(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I16, i64::from(value))),
        ConstantValue::U16(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I16, i64::from(value))),
        ConstantValue::I32(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I32, i64::from(value))),
        ConstantValue::U32(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I32, i64::from(value))),
        ConstantValue::I64(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I64, value)),
        ConstantValue::U64(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I64, i64::try_from(value).ok()?)),
        ConstantValue::ISize(value) => ConstValue::Value(compiler.ins().iconst(ptr_type, i64::try_from(value).ok()?)),
        ConstantValue::USize(value) => ConstValue::Value(compiler.ins().iconst(ptr_type, i64::try_from(value).ok()?)),
        ConstantValue::Float(value) => ConstValue::Value(compiler.ins().f32const(value)),
        ConstantValue::Boolean(value) => ConstValue::Value(compiler.ins().iconst(ir::types::I8, i64::from(value))),
        ConstantValue::Array(values) => {
            let element = compiler.get_array_element(element_type);
            let values = values
                .into_iter()
                .filter_map(|constant| compile_constant(element_type, constant, compiler))
                .collect::<Box<[_]>>();

            return compiler.alloc_array(element_type, element, values);
        }
        ConstantValue::String(value) => {
            return compiler.alloc_string(value);
            // let len = value.len();

            // data_desc.define(value.clone().into_bytes().into_boxed_slice());

            // let id = module.declare_anonymous_data(true, false).ok()?;

            // module.define_data(id, data_desc).ok()?;
            // data_desc.clear();

            // let data_id = module.declare_data_in_func(id, fn_builder.func);

            // let ptr = fn_builder.ins().symbol_value(ptr_type, data_id);
            // let size = fn_builder.ins().iconst(ptr_type, len.cast_signed() as
            // i64);

            // FatPtr::new(module.isa(), fn_builder, ptr, size)
        }
        ConstantValue::Construct { ty, variant, fields } => {
            let values = fields
                .into_iter()
                .map(|f| f.1.and_then(|v| compile_constant(element_type, v, compiler)))
                .collect::<Box<[_]>>();

            return compiler.construct(ty, variant, values).map(ConstValue::Value);
        }
        ConstantValue::Nothing => return None,
    })
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Array {
    pub element: MollieType,
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

        match self.element {
            MollieType::Regular(_) => {
                for (index, value) in values.enumerate() {
                    fn_builder.ins().stack_store(value, slot, self.get_offset_of(index));
                }
            }
            MollieType::Fat(ty, _) => {
                for (index, (value, metadata)) in values.tuples::<(ir::Value, ir::Value)>().enumerate() {
                    fn_builder.ins().stack_store(value, slot, self.get_offset_of(index));
                    fn_builder
                        .ins()
                        .stack_store(metadata, slot, self.get_offset_of(index) + ty.bytes().cast_signed());
                }
            }
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}
