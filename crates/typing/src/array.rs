use std::fmt;

use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, isa::TargetIsa};
use mollie_shared::cranelift::stack_alloc;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ArrayType {
    pub element: Type,
    pub size: Option<usize>,
}

#[allow(clippy::cast_possible_truncation)]
impl ArrayType {
    pub fn get_size(&self, isa: &dyn TargetIsa, values: usize) -> u32 {
        self.element.variant.as_ir_type(isa).bytes() * values as u32
    }

    pub fn get_offset_of(&self, isa: &dyn TargetIsa, index: usize) -> i32 {
        self.element.variant.as_ir_type(isa).bytes().cast_signed() * index.cast_signed() as i32
    }

    pub fn instance(&self, isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, values: Vec<Value>) -> Value {
        let slot = stack_alloc(fn_builder, self.get_size(isa, values.len()));

        for (index, value) in values.into_iter().enumerate() {
            fn_builder.ins().stack_store(value, slot, self.get_offset_of(isa, index));
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.element, self.size.as_ref().map_or_else(String::new, ToString::to_string))
    }
}
