use std::{fmt, ops::Deref};

pub use mollie_compiler as compiler;
use mollie_compiler::allocator::{alloc, unmark_root};
pub use mollie_const as constants;
pub use mollie_index as index;
pub use mollie_ir as ir;
pub use mollie_parser as parser;
pub use mollie_shared as shared;
pub use mollie_typed_ast as typed_ast;
pub use mollie_typing as typing;
use mollie_typing::{IntType, UIntType};

use self::{
    compiler::allocator::{GcValue, TypeLayout},
    constants::ConstantValue,
    index::{IndexBoxedSlice, IndexVec},
    typed_ast::{TypeChecker, VTableFunc, VTableFuncKind, VTableGenerator},
    typing::{Adt, AdtKind, AdtVariant, FieldType, FuncArg, PrimitiveType, TypeInfo, TypeInfoRef, VFuncRef, VTableRef},
};

#[repr(transparent)]
pub struct GcPtr<T>(*mut GcValue<T>);

impl<T> GcPtr<T> {
    pub fn from(value: T) -> Self {
        let layout = Box::leak::<'static>(Box::new(TypeLayout::of::<T>()));
        let result_value = unsafe { alloc(layout) };

        unsafe {
            *result_value.cast::<T>() = value;
        }

        Self(result_value.cast())
    }

    pub fn type_layout(&self) -> &'static TypeLayout {
        unsafe { self.0.byte_sub(std::mem::offset_of!(GcValue<()>, value)).read().layout }
    }
}

impl<T: fmt::Display> fmt::Display for GcPtr<T> {
    #[track_caller]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: fmt::Debug> fmt::Debug for GcPtr<T> {
    #[track_caller]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T> Deref for GcPtr<T> {
    type Target = T;

    #[track_caller]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0.cast::<T>() }
    }
}

impl<T> Drop for GcPtr<T> {
    fn drop(&mut self) {
        unsafe { unmark_root(self.0.cast()) };
    }
}

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
    i8     => FieldType::Primitive(PrimitiveType::Int (IntType ::   I8)),
    u8     => FieldType::Primitive(PrimitiveType::UInt(UIntType::   U8)),
    i16    => FieldType::Primitive(PrimitiveType::Int (IntType ::  I16)),
    u16    => FieldType::Primitive(PrimitiveType::UInt(UIntType::  U16)),
    i32    => FieldType::Primitive(PrimitiveType::Int (IntType ::  I32)),
    u32    => FieldType::Primitive(PrimitiveType::UInt(UIntType::  U32)),
    i64    => FieldType::Primitive(PrimitiveType::Int (IntType ::  I64)),
    u64    => FieldType::Primitive(PrimitiveType::UInt(UIntType::  U64)),
    isize  => FieldType::Primitive(PrimitiveType::Int (IntType ::ISize)),
    usize  => FieldType::Primitive(PrimitiveType::UInt(UIntType::USize)),
    f32    => FieldType::Primitive(PrimitiveType::Float                ),
    bool   => FieldType::Primitive(PrimitiveType::Boolean              ),
    &str   => FieldType::Primitive(PrimitiveType::String               ),
    String => FieldType::Primitive(PrimitiveType::String               )
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

type AdtBuilderVariant = Vec<(String, FieldType, Option<ConstantValue>)>;

#[derive(Debug)]
pub struct AdtBuilder {
    name: Option<String>,
    collectable: bool,
    variants: Vec<(Option<String>, AdtBuilderVariant)>,
    generics: usize,
    kind: AdtKind,
}

impl AdtBuilder {
    pub fn new_struct<T: Into<String>>(name: T) -> Self {
        Self {
            name: Some(name.into()),
            collectable: true,
            variants: vec![(None, vec![])],
            generics: 0,
            kind: AdtKind::Struct,
        }
    }

    pub fn new_enum<T: Into<String>>(name: T) -> Self {
        Self {
            name: Some(name.into()),
            collectable: true,
            variants: vec![],
            generics: 0,
            kind: AdtKind::Enum,
        }
    }

    pub fn non_gc_collectable(mut self) -> Self {
        self.collectable = false;

        self
    }

    pub fn variant<T: Into<String>>(mut self, name: T) -> Self {
        self.variants.push((Some(name.into()), vec![(
            String::from("<discriminant>"),
            FieldType::Primitive(PrimitiveType::UInt(UIntType::USize)),
            None,
        )]));

        self
    }

    pub const fn add_generic(mut self) -> Self {
        self.generics += 1;

        self
    }

    pub fn field_default<T: MollieTypeOf + Into<ConstantValue>>(mut self, name: impl Into<String>, default: T) -> Self {
        if let Some(index) = T::generic_index() {
            assert!(self.generics > index, "pls add generic param");
        }

        if let Some((_, variant)) = self.variants.last_mut() {
            variant.push((name.into(), T::mollie_type_of(), Some(default.into())));
        }

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

    pub fn finish(self) -> Adt {
        Adt {
            name: self.name,
            collectable: self.collectable,
            kind: self.kind,
            generics: self.generics,
            variants: IndexBoxedSlice::from_iter(self.variants.into_iter().enumerate().map(|(discriminant, (name, fields))| AdtVariant {
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
            used_adt_types: Vec::new(),
            functions,
        });

        checker.solver.vtables.insert(self.this, std::iter::once((None, vtable)).collect());

        vtable
    }
}
