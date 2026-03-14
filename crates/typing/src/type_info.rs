use crate::{AdtRef, PrimitiveType, TraitRef};

mollie_index::new_idx_type!(TypeInfoRef);

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Primitive(PrimitiveType),
    Array(TypeInfoRef, Option<usize>),
    Func(Box<[TypeInfoRef]>, TypeInfoRef),
    Adt(AdtRef, Box<[TypeInfoRef]>),
    Trait(TraitRef, Box<[TypeInfoRef]>),
    Unknown(Option<TypeInfoRef>),
    Generic(usize),
    Ref(TypeInfoRef),
    Error,
}
