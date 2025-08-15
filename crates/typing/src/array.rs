use std::fmt;

use cranelift::{
    jit::JITModule,
    module::Module,
    prelude::{InstBuilder, Value},
};
use mollie_shared::cranelift::stack_alloc;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct ArrayOfType {
    pub ty: cranelift::prelude::Type,
}

#[allow(clippy::cast_possible_truncation)]
impl ArrayOfType {
    pub const fn new(ty: cranelift::prelude::Type) -> Self {
        Self { ty }
    }

    pub fn get_offset_of(&self, index: usize) -> i32 {
        self.ty.bytes().cast_signed() * index.cast_signed() as i32
    }

    pub fn instance(&self, module: &JITModule, fn_builder: &mut cranelift::prelude::FunctionBuilder, values: Vec<Value>) -> Value {
        let size_t = module.isa().pointer_type();
        let slot = stack_alloc(fn_builder, self.ty.bytes() * values.len() as u32);

        for (index, value) in values.into_iter().enumerate() {
            fn_builder.ins().stack_store(value, slot, self.get_offset_of(index));
        }

        fn_builder.ins().stack_addr(size_t, slot, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ArrayType {
    pub element: Type,
    pub size: Option<usize>,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub arr: ArrayOfType,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.element, self.size.as_ref().map_or_else(String::new, ToString::to_string))
    }
}
