pub use mollie_compiler as compiler;
pub use mollie_const as constants;
pub use mollie_index as index;
pub use mollie_ir as ir;
pub use mollie_shared as shared;
pub use mollie_tast as tast;
use mollie_tast::VTableGenerator;
pub use mollie_typing as typing;
use mollie_typing::FuncArg;

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
    u64 => FieldType::Primitive(PrimitiveType::U64),
    f32 => FieldType::Primitive(PrimitiveType::Float),
    &str => FieldType::Primitive(PrimitiveType::String),
    String => FieldType::Primitive(PrimitiveType::String)
}

pub struct Generic<const N: usize>;

impl<const N: usize> MollieTypeOf for Generic<N> {
    fn generic_index() -> Option<usize> {
        Some(N)
    }

    fn mollie_type_of() -> FieldType {
        FieldType::Generic(N, None)
    }
}

#[derive(Debug)]
pub struct StructBuilder {
    name: Option<String>,
    variants: Vec<(Option<String>, Vec<(String, FieldType, Option<ConstantValue>)>)>,
    generics: usize,
    kind: ComplexTypeKind,
}

impl StructBuilder {
    pub fn with_name<T: Into<String>>(name: T) -> Self {
        Self {
            name: Some(name.into()),
            variants: vec![(None, vec![])],
            generics: 0,
            kind: ComplexTypeKind::Struct,
        }
    }

    pub fn enum_with_name<T: Into<String>>(name: T) -> Self {
        Self {
            name: Some(name.into()),
            variants: vec![],
            generics: 0,
            kind: ComplexTypeKind::Enum,
        }
    }

    pub fn variant<T: Into<String>>(mut self, name: T) -> Self {
        self.variants.push((Some(name.into()), Vec::new()));

        self
    }

    pub const fn add_generic(mut self) -> Self {
        self.generics += 1;

        self
    }

    pub fn field<T: MollieTypeOf>(mut self, name: impl Into<String>) -> Self {
        if let Some(index) = T::generic_index() {
            assert!(self.generics > index, "pls add generic param");
        }

        if let Some((_, variant)) = self.variants.last_mut() {
            variant.push((name.into(), T::mollie_type_of(), None));
        }

        self
    }

    pub fn field_ty<T: Into<String>>(mut self, name: T, ty: FieldType) -> Self {
        if let Some((_, variant)) = self.variants.last_mut() {
            variant.push((name.into(), ty, None));
        }

        self
    }

    pub fn finish(self) -> ComplexType {
        ComplexType {
            name: self.name,
            kind: self.kind,
            generics: self.generics,
            variants: IndexBoxedSlice::from_iter(self.variants.into_iter().enumerate().map(|(discriminant, (name, fields))| ComplexTypeVariant {
                name,
                discriminant,
                fields: fields.into(),
            })),
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
            ty: TypeInfo::Func(args.into_iter().map(FuncArg::Regular).collect(), returns),
            kind: VTableFuncKind::External(external_name),
        });

        self
    }

    pub fn finish(self, checker: &mut TypeChecker) -> VTableRef {
        let functions = self
            .functions
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
            .collect();

        let vtable = checker.vtables.insert(VTableGenerator {
            origin_trait: None,
            generics: Box::new([]),
            used_vtables: Vec::new(),
            used_complex_types: Vec::new(),
            functions,
        });

        checker.solver.vtables.insert(self.this, std::iter::once((None, vtable)).collect());

        vtable
    }
}
