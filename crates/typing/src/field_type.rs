use crate::{AdtKind, AdtRef, CoreTypes, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver, UIntType, type_info::FuncArg};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldType {
    This,
    Generic(usize, Option<TypeInfoRef>),
    Unknown(Option<Box<Self>>),
    Primitive(PrimitiveType),
    Array(Box<Self>, Option<usize>),
    Func(Box<[FuncArg<Self>]>, Box<Self>),
    Trait(TraitRef, Box<[Self]>),
    Adt(AdtRef, AdtKind, Box<[Self]>),
}

impl FieldType {
    pub fn erase(&mut self) {
        match self {
            &mut Self::Generic(index, _) => *self = Self::Generic(index, None),
            Self::Unknown(fallback) => {
                if let Some(fallback) = fallback {
                    fallback.erase();
                }
            }
            Self::This | Self::Primitive(_) => (),
            Self::Array(field_type, _) => field_type.erase(),
            Self::Func(func_args, field_type) => {
                for arg in func_args {
                    arg.as_inner_mut().erase();
                }

                field_type.erase();
            }
            Self::Trait(_, type_args) | Self::Adt(.., type_args) => {
                for type_arg in type_args {
                    type_arg.erase();
                }
            }
        }
    }

    pub fn try_unify(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::This, Self::This) | (Self::Generic(..), _) | (Self::Unknown(None), Self::Unknown(Some(_))) => true,
            (Self::Primitive(primitive_type), Self::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (Self::Array(field_type, size), Self::Array(other_field_type, other_size)) => {
                field_type.try_unify(other_field_type)
                    && match (size, other_size) {
                        (None, None | Some(_)) => true,
                        (Some(_), None) => false,
                        (Some(size), Some(other_size)) => size == other_size,
                    }
            }
            (Self::Func(field_types, field_type), Self::Func(other_field_types, other_field_type)) => {
                field_types.len() == other_field_types.len()
                    && field_types
                        .iter()
                        .zip(other_field_types)
                        .all(|(field_type, other_field_type)| field_type.as_inner().try_unify(other_field_type.as_inner()))
                    && field_type.try_unify(other_field_type)
            }
            (Self::Trait(trait_ref, field_types), Self::Trait(other_trait_ref, other_field_types)) => {
                field_types.len() == other_field_types.len()
                    && field_types
                        .iter()
                        .zip(other_field_types)
                        .all(|(field_type, other_field_type)| field_type.try_unify(other_field_type))
                    && trait_ref == other_trait_ref
            }
            (Self::Adt(adt_ref, adt_kind, field_types), Self::Adt(other_adt_ref, other_adt_kind, other_field_types)) => {
                field_types.len() == other_field_types.len()
                    && field_types
                        .iter()
                        .zip(other_field_types)
                        .all(|(field_type, other_field_type)| field_type.try_unify(other_field_type))
                    && adt_ref == other_adt_ref
                    && adt_kind == other_adt_kind
            }
            _ => false,
        }
    }

    pub fn from_type_info_ref(info_ref: TypeInfoRef, solver: &TypeSolver) -> Self {
        Self::from_type_info(solver.get_info(info_ref), Some(info_ref), solver)
    }

    pub fn from_type_info(info: &TypeInfo, info_ref: Option<TypeInfoRef>, solver: &TypeSolver) -> Self {
        match info {
            &TypeInfo::Unknown(fallback) => Self::Unknown(fallback.map(|info| Box::new(Self::from_type_info(solver.get_info(info), Some(info), solver)))),
            &TypeInfo::Generic(idx, _) => Self::Generic(idx, info_ref),
            &TypeInfo::Primitive(primitive_type) => Self::Primitive(primitive_type),
            &TypeInfo::Ref(type_info_ref) => Self::from_type_info(solver.get_info(type_info_ref), Some(type_info_ref), solver),
            TypeInfo::Func(type_info_refs, type_info_ref) => Self::Func(
                type_info_refs
                    .iter()
                    .map(|ty| ty.map(|ty| Self::from_type_info(solver.get_info(ty), Some(ty), solver)))
                    .collect(),
                Box::new(Self::from_type_info(solver.get_info(*type_info_ref), Some(*type_info_ref), solver)),
            ),
            TypeInfo::Trait(trait_ref, type_info_refs) => Self::Trait(
                *trait_ref,
                type_info_refs
                    .iter()
                    .map(|ty| Self::from_type_info(solver.get_info(*ty), Some(*ty), solver))
                    .collect(),
            ),
            TypeInfo::Adt(adt_ref, adt_kind, type_info_refs) => Self::Adt(
                *adt_ref,
                *adt_kind,
                type_info_refs
                    .iter()
                    .map(|ty| Self::from_type_info(solver.get_info(*ty), Some(*ty), solver))
                    .collect(),
            ),
            &TypeInfo::Array(type_info_ref, size) => Self::Array(
                Box::new(Self::from_type_info(solver.get_info(type_info_ref), Some(type_info_ref), solver)),
                size,
            ),
        }
    }

    pub fn as_type_info(&self, this: Option<TypeInfoRef>, core_types: &CoreTypes, solver: &mut TypeSolver, args: &[TypeInfoRef]) -> TypeInfoRef {
        match self {
            Self::This => this.unwrap(),
            Self::Unknown(None) => panic!("what"),
            Self::Unknown(Some(ty)) => ty.as_type_info(this, core_types, solver, args),
            &Self::Generic(generic, fallback) => args
                .get(generic)
                .copied()
                .or(fallback)
                .unwrap_or_else(|| solver.add_info(TypeInfo::Generic(generic, None), None)),
            Self::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => core_types.any,
                PrimitiveType::Int(IntType::ISize) => core_types.int_size,
                PrimitiveType::Int(IntType::I64) => core_types.int64,
                PrimitiveType::Int(IntType::I32) => core_types.int32,
                PrimitiveType::Int(IntType::I16) => core_types.int16,
                PrimitiveType::Int(IntType::I8) => core_types.int8,
                PrimitiveType::UInt(UIntType::USize) => core_types.uint_size,
                PrimitiveType::UInt(UIntType::U64) => core_types.uint64,
                PrimitiveType::UInt(UIntType::U32) => core_types.uint32,
                PrimitiveType::UInt(UIntType::U16) => core_types.uint16,
                PrimitiveType::UInt(UIntType::U8) => core_types.uint8,
                PrimitiveType::Float => core_types.float,
                PrimitiveType::Boolean => core_types.boolean,
                PrimitiveType::String => core_types.string,
                PrimitiveType::Component => core_types.component,
                PrimitiveType::Void => core_types.void,
                PrimitiveType::Null => unimplemented!(),
            },
            Self::Array(element, size) => {
                let element = element.as_type_info(this, core_types, solver, args);

                solver.add_info(TypeInfo::Array(element, *size), None)
            }
            Self::Func(params, output) => {
                let params = params
                    .iter()
                    .map(|param| param.as_ref().map(|param| param.as_type_info(this, core_types, solver, args)))
                    .collect();
                let output = output.as_type_info(this, core_types, solver, args);

                solver.add_info(TypeInfo::Func(params, output), None)
            }
            Self::Trait(trait_ref, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| field_type.as_type_info(this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Trait(*trait_ref, args), None)
            }
            Self::Adt(adt_ref, kind, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| field_type.as_type_info(this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Adt(*adt_ref, *kind, args), None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{FieldType, PrimitiveType};

    #[test]
    fn test_unify() {
        assert!(
            FieldType::Array(Box::new(FieldType::Generic(0, None)), None)
                .try_unify(&FieldType::Array(Box::new(FieldType::Primitive(PrimitiveType::Boolean)), None))
        );
    }
}
