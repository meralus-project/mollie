mod adt;
mod field_type;
mod primitive_type;
mod solver;
mod type_info;

use mollie_index::new_idx_type;

pub use self::{
    adt::{Adt, AdtKind, AdtVariant},
    field_type::FieldType,
    primitive_type::{IntType, PrimitiveType, UIntType},
    solver::{TypeSolver, TypeStorage, TypeUnificationError, Variable},
    type_info::{FuncArg, TypeInfo},
};

new_idx_type!(TypeInfoRef);
new_idx_type!(AdtRef);
new_idx_type!(AdtVariantRef);
new_idx_type!(FieldRef);
new_idx_type!(TraitRef);
new_idx_type!(VTableRef);
new_idx_type!(VFuncRef);

#[derive(Debug, Clone, Copy)]
pub struct CoreTypes<T = TypeInfoRef> {
    pub void: T,
    pub any: T,
    pub boolean: T,
    pub int8: T,
    pub int16: T,
    pub int32: T,
    pub int64: T,
    pub int_size: T,
    pub uint8: T,
    pub uint16: T,
    pub uint32: T,
    pub uint64: T,
    pub uint_size: T,
    pub float: T,
    pub component: T,
    pub string: T,
}

impl<T: Copy> CoreTypes<T> {
    pub fn cast_primitive(&self, primitive: PrimitiveType) -> T {
        match primitive {
            PrimitiveType::Any => self.any,
            PrimitiveType::Int(int_type) => match int_type {
                IntType::ISize => self.int_size,
                IntType::I64 => self.int64,
                IntType::I32 => self.int32,
                IntType::I16 => self.int16,
                IntType::I8 => self.int8,
            },
            PrimitiveType::UInt(uint_type) => match uint_type {
                UIntType::USize => self.uint_size,
                UIntType::U64 => self.uint64,
                UIntType::U32 => self.uint32,
                UIntType::U16 => self.uint16,
                UIntType::U8 => self.uint8,
            },
            PrimitiveType::Float => self.float,
            PrimitiveType::Boolean => self.boolean,
            PrimitiveType::String => self.string,
            PrimitiveType::Component => self.component,
            PrimitiveType::Void => self.void,
            PrimitiveType::Null => unimplemented!(),
        }
    }
}
