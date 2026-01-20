mod adt;
mod field_type;
mod primitive_type;
mod solver;
mod type_info;

use mollie_index::new_idx_type;

pub use self::{
    adt::{Adt, AdtKind, AdtVariant, CompiledAdt, CompiledAdtVariant},
    field_type::FieldType,
    primitive_type::{IntType, PrimitiveType, UIntType},
    solver::{TypeSolver, TypeStorage, TypeUnificationError},
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
pub struct CoreTypes {
    pub void: TypeInfoRef,
    pub any: TypeInfoRef,
    pub boolean: TypeInfoRef,
    pub int8: TypeInfoRef,
    pub int16: TypeInfoRef,
    pub int32: TypeInfoRef,
    pub int64: TypeInfoRef,
    pub int_size: TypeInfoRef,
    pub uint8: TypeInfoRef,
    pub uint16: TypeInfoRef,
    pub uint32: TypeInfoRef,
    pub uint64: TypeInfoRef,
    pub uint_size: TypeInfoRef,
    pub float: TypeInfoRef,
    pub component: TypeInfoRef,
    pub string: TypeInfoRef,
}
