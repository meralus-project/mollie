use crate::{ComplexTypeKind, ComplexTypeRef, CoreTypes, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver};



#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FieldType {
    This,
    Generic(usize),
    Unknown(Option<Box<Self>>),
    Primitive(PrimitiveType),
    Array(Box<Self>, Option<usize>),
    Func(Box<[Self]>, Box<Self>),
    Trait(TraitRef, Box<[Self]>),
    Complex(ComplexTypeRef, ComplexTypeKind, Box<[Self]>),
}

impl FieldType {
    pub fn from_type_info(info: &TypeInfo, solver: &TypeSolver) -> Self {
        match info {
            &TypeInfo::Unknown(fallback) => Self::Unknown(fallback.map(|info| Box::new(Self::from_type_info(solver.get_info(info), solver)))),
            &TypeInfo::Primitive(primitive_type) => Self::Primitive(primitive_type),
            &TypeInfo::Ref(type_info_ref) => Self::from_type_info(solver.get_info(type_info_ref), solver),
            TypeInfo::Func(type_info_refs, type_info_ref) => Self::Func(
                type_info_refs.iter().map(|ty| Self::from_type_info(solver.get_info(*ty), solver)).collect(),
                Box::new(Self::from_type_info(solver.get_info(*type_info_ref), solver)),
            ),
            TypeInfo::Trait(trait_ref, type_info_refs) => Self::Trait(
                *trait_ref,
                type_info_refs.iter().map(|ty| Self::from_type_info(solver.get_info(*ty), solver)).collect(),
            ),
            TypeInfo::Complex(complex_type_ref, complex_type_kind, type_info_refs) => Self::Complex(
                *complex_type_ref,
                *complex_type_kind,
                type_info_refs.iter().map(|ty| Self::from_type_info(solver.get_info(*ty), solver)).collect(),
            ),
            &TypeInfo::Array(type_info_ref, size) => Self::Array(Box::new(Self::from_type_info(solver.get_info(type_info_ref), solver)), size),
        }
    }

    pub fn as_type_info(&self, this: Option<TypeInfoRef>, core_types: &CoreTypes, solver: &mut TypeSolver, args: &[TypeInfoRef]) -> TypeInfoRef {
        match self {
            Self::This => this.unwrap(),
            Self::Unknown(_) => panic!("what"),
            &Self::Generic(generic) => args.get(generic).copied().unwrap_or_else(|| solver.add_info(TypeInfo::Unknown(None))),
            Self::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => core_types.any,
                PrimitiveType::ISize => core_types.int_size,
                PrimitiveType::I64 => core_types.int64,
                PrimitiveType::I32 => core_types.int32,
                PrimitiveType::I16 => core_types.int16,
                PrimitiveType::I8 => core_types.int8,
                PrimitiveType::USize => core_types.uint_size,
                PrimitiveType::U64 => core_types.uint64,
                PrimitiveType::U32 => core_types.uint32,
                PrimitiveType::U16 => core_types.uint16,
                PrimitiveType::U8 => core_types.uint8,
                PrimitiveType::Float => core_types.float,
                PrimitiveType::Boolean => core_types.boolean,
                PrimitiveType::String => core_types.string,
                PrimitiveType::Component => core_types.component,
                PrimitiveType::Void => core_types.void,
                PrimitiveType::Null => unimplemented!(),
            },
            Self::Array(element, size) => {
                let element = element.as_type_info(this, core_types, solver, args);

                solver.add_info(TypeInfo::Array(element, *size))
            }
            Self::Func(params, output) => {
                let params = params.iter().map(|param| param.as_type_info(this, core_types, solver, args)).collect();
                let output = output.as_type_info(this, core_types, solver, args);

                solver.add_info(TypeInfo::Func(params, output))
            }
            Self::Trait(trait_ref, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| field_type.as_type_info(this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Trait(*trait_ref, args))
            }
            Self::Complex(complex_type, kind, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| field_type.as_type_info(this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Complex(*complex_type, *kind, args))
            }
        }
    }
}
