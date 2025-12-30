pub use mollie_compiler as compiler;
pub use mollie_const as constants;
pub use mollie_index as index;
pub use mollie_ir as ir;
pub use mollie_shared as shared;
pub use mollie_tast as tast;
pub use mollie_typing as typing;

use self::{
    constants::ConstantValue,
    index::{IndexBoxedSlice, IndexVec},
    tast::{TypeChecker, VTableFunc, VTableFuncKind},
    typing::{ComplexType, ComplexTypeKind, ComplexTypeVariant, FieldType, PrimitiveType, TypeInfo, TypeInfoRef, VFuncRef, VTableRef},
};

pub trait MollieTypeOf {
    fn generic_index() -> Option<usize> {
        None
    }

    fn mollie_type_of() -> FieldType;
}

macro_rules! mollie_types {
    ($($typo:ty => $variant:expr),*) => {
        $(
            impl MollieTypeOf for $typo {
                fn mollie_type_of() -> FieldType {
                    $variant
                }
            }
        )*
    };
}

mollie_types! {
    i8 => FieldType::Primitive(PrimitiveType::I8),
    u8 => FieldType::Primitive(PrimitiveType::U8),
    i16 => FieldType::Primitive(PrimitiveType::I16),
    u16 => FieldType::Primitive(PrimitiveType::U16),
    i32 => FieldType::Primitive(PrimitiveType::I32),
    u32 => FieldType::Primitive(PrimitiveType::U32),
    i64 => FieldType::Primitive(PrimitiveType::I64),
    u64 => FieldType::Primitive(PrimitiveType::U64)
}

pub struct Generic<const N: usize>;

impl<const N: usize> MollieTypeOf for Generic<N> {
    fn generic_index() -> Option<usize> {
        Some(N)
    }

    fn mollie_type_of() -> FieldType {
        FieldType::Generic(N)
    }
}

#[derive(Debug, Default)]
pub struct StructBuilder {
    name: Option<String>,
    fields: Vec<(String, FieldType, Option<ConstantValue>)>,
    generics: usize,
}

impl StructBuilder {
    pub fn with_name<T: Into<String>>(name: T) -> Self {
        Self {
            name: Some(name.into()),
            fields: Vec::new(),
            generics: 0,
        }
    }

    pub const fn add_generic(mut self) -> Self {
        self.generics += 1;

        self
    }

    pub fn field<T: MollieTypeOf>(mut self, name: impl Into<String>) -> Self {
        if let Some(index) = T::generic_index() {
            assert!(self.generics > index, "pls add generic param");
        }

        self.fields.push((name.into(), T::mollie_type_of(), None));

        self
    }

    pub fn field_ty<T: Into<String>>(mut self, name: T, ty: FieldType) -> Self {
        self.fields.push((name.into(), ty, None));

        self
    }

    pub fn finish(self) -> ComplexType {
        ComplexType {
            name: self.name,
            kind: ComplexTypeKind::Struct,
            generics: self.generics,
            variants: IndexBoxedSlice::from_iter([ComplexTypeVariant {
                name: None,
                discriminant: 0,
                fields: self.fields.into(),
            }]),
        }
    }
}

pub struct VTableBuilder {
    this: FieldType,
    functions: IndexVec<VFuncRef, VTableFunc<TypeInfo>>,
}

impl VTableBuilder {
    pub const fn new(this: FieldType) -> Self {
        Self {
            this,
            functions: IndexVec::new(),
        }
    }

    pub fn func<T: Into<String>, I: IntoIterator<Item = TypeInfoRef>>(mut self, name: T, external_name: &'static str, args: I, returns: TypeInfoRef) -> Self {
        self.functions.push(VTableFunc {
            trait_func: None,
            name: name.into(),
            arg_names: vec![],
            ty: TypeInfo::Func(args.into_iter().collect(), returns),
            kind: VTableFuncKind::External(external_name),
        });

        self
    }

    pub fn finish(self, checker: &mut TypeChecker) -> VTableRef {
        let vtable = checker.vtables.insert(
            self.functions
                .into_values()
                .map(
                    |VTableFunc {
                         trait_func,
                         name,
                         arg_names,
                         ty,
                         kind,
                     }| VTableFunc {
                        trait_func,
                        name,
                        arg_names,
                        ty: checker.solver.add_info(ty),
                        kind,
                    },
                )
                .collect(),
        );

        checker.solver.vtables.insert(self.this, std::iter::once((None, vtable)).collect());

        vtable
    }
}
