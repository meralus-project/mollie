use std::{fmt, iter::empty, ops::Deref};

pub use mollie_compiler as compiler;
pub use mollie_const as constants;
pub use mollie_index as index;
pub use mollie_ir as ir;
pub use mollie_parser as parser;
pub use mollie_shared as shared;
pub use mollie_typed_ast as typed_ast;
pub use mollie_typing as typing;

use self::{
    compiler::{
        CompiledAdt,
        allocator::{GcValue, TypeLayout, alloc, unmark_root},
    },
    constants::ConstantValue,
    index::{Idx, IndexBoxedSlice, IndexVec},
    typed_ast::{FunctionBody, TypedASTContext},
    typing::{
        Adt, AdtKind, AdtRef, AdtVariant, AdtVariantField, AdtVariantRef, Arg, ArgType, FieldRef, IntType, ModuleId, PrimitiveType, Trait, TraitFunc,
        TraitFuncRef, TraitRef, Type, TypeContext, TypeRef, UIntType, VFuncRef, VTableFunc, VTableGenerator, VTableRef,
    },
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

    pub fn from_parts(value: T, layout: &'static TypeLayout) -> Self {
        let result_value = unsafe { alloc(layout) };

        unsafe {
            *result_value.cast::<T>() = value;
        }

        Self(result_value.cast())
    }

    pub fn ptr(&self) -> *const T {
        self.0.cast()
    }

    pub fn ptr_mut(&mut self) -> *mut T {
        self.0.cast()
    }

    pub fn type_layout(&self) -> &'static TypeLayout {
        unsafe { self.0.byte_sub(std::mem::offset_of!(GcValue<()>, value)).read().layout }
    }

    pub fn adt_variant(&self) -> AdtVariantRef {
        let type_layout = self.type_layout();

        match type_layout.kind {
            // SAFETY: discriminant is always placed at the beginning
            Some(AdtKind::Enum) => AdtVariantRef::new(unsafe { self.0.cast::<usize>().read() }),
            _ => AdtVariantRef::ZERO,
        }
    }

    pub fn get<F>(&self, adt: &CompiledAdt, field: FieldRef) -> Option<&F> {
        assert_eq!(self.type_layout(), adt.type_layout);

        let field = &adt.variants[self.adt_variant()].fields[field].0;

        assert_eq!(size_of::<F>(), field.ty.bytes() as usize);

        unsafe { self.0.byte_add(field.offset as usize).cast::<F>().as_ref() }
    }

    pub fn get_mut<F>(&mut self, adt: &CompiledAdt, field: FieldRef) -> Option<&mut F> {
        assert_eq!(self.type_layout(), adt.type_layout);

        let field = &adt.variants[self.adt_variant()].fields[field].0;

        assert_eq!(size_of::<F>(), field.ty.bytes() as usize);

        unsafe { self.0.byte_add(field.offset as usize).cast::<F>().as_mut() }
    }

    pub fn get_slice<F>(&self, adt: &CompiledAdt, field: FieldRef) -> Option<&[F]> {
        assert_eq!(self.type_layout(), adt.type_layout);

        let field = &adt.variants[self.adt_variant()].fields[field].0;

        assert_eq!(size_of::<&[F]>(), field.ty.bytes() as usize);

        unsafe { Some(self.0.byte_add(field.offset as usize).cast::<&[F]>().read()) }
    }

    pub fn get_slice_mut<F>(&mut self, adt: &CompiledAdt, field: FieldRef) -> Option<&mut [F]> {
        assert_eq!(self.type_layout(), adt.type_layout);

        let field = &adt.variants[self.adt_variant()].fields[field].0;

        assert_eq!(size_of::<&mut [F]>(), field.ty.bytes() as usize);

        unsafe { Some(self.0.byte_add(field.offset as usize).cast::<&mut [F]>().read()) }
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

    fn mollie_type_of(context: &mut TypeContext) -> TypeRef;
}

pub trait MollieMultipleTypeOf {
    fn generic_index() -> Option<usize> {
        None
    }

    fn mollie_type_of(context: &mut TypeContext) -> impl Iterator<Item = TypeRef>;
}

macro_rules! mollie_types {
    ($($typo:ty => $variant:expr),*) => {
        $(
            impl MollieTypeOf for $typo {
                fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
                    context.types.get_or_add($variant)
                }
            }
        )*
    };
}

macro_rules! mollie_arg_types {
    ($($name:ident),*) => {
        impl<$($name: MollieTypeOf),*> MollieMultipleTypeOf for ($($name),*,) {
            fn mollie_type_of(context: &mut TypeContext) ->impl Iterator<Item = TypeRef> {
                [$($name::mollie_type_of(context)),*].into_iter()
            }
        }
    };
}

mollie_types! {
    i8     => Type::Primitive(PrimitiveType::Int (IntType ::   I8)),
    u8     => Type::Primitive(PrimitiveType::UInt(UIntType::   U8)),
    i16    => Type::Primitive(PrimitiveType::Int (IntType ::  I16)),
    u16    => Type::Primitive(PrimitiveType::UInt(UIntType::  U16)),
    i32    => Type::Primitive(PrimitiveType::Int (IntType ::  I32)),
    u32    => Type::Primitive(PrimitiveType::UInt(UIntType::  U32)),
    i64    => Type::Primitive(PrimitiveType::Int (IntType ::  I64)),
    u64    => Type::Primitive(PrimitiveType::UInt(UIntType::  U64)),
    isize  => Type::Primitive(PrimitiveType::Int (IntType ::ISize)),
    usize  => Type::Primitive(PrimitiveType::UInt(UIntType::USize)),
    f32    => Type::Primitive(PrimitiveType::Float                ),
    bool   => Type::Primitive(PrimitiveType::Boolean              ),
    &str   => Type::Primitive(PrimitiveType::String               ),
    String => Type::Primitive(PrimitiveType::String               )
}

pub struct Generic<const N: usize>;

impl<const N: usize> MollieTypeOf for Generic<N> {
    fn generic_index() -> Option<usize> {
        Some(N)
    }

    fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
        context.types.get_or_add(Type::Generic(N))
    }
}

type AdtBuilderVariant = Vec<(String, TypeRef, Option<ConstantValue>)>;

pub struct AnyType;

impl MollieTypeOf for () {
    fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
        context.types.get_or_add(Type::Primitive(PrimitiveType::Void))
    }
}

impl MollieTypeOf for AnyType {
    fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
        context.types.get_or_add(Type::Primitive(PrimitiveType::Any))
    }
}

impl MollieMultipleTypeOf for () {
    fn mollie_type_of(_: &mut TypeContext) -> impl Iterator<Item = TypeRef> {
        empty()
    }
}

impl<T: MollieTypeOf> MollieTypeOf for &[T] {
    fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
        let element = T::mollie_type_of(context);

        context.types.get_or_add(Type::Array(element, None))
    }
}

impl<T: MollieTypeOf, const U: usize> MollieTypeOf for [T; U] {
    fn mollie_type_of(context: &mut TypeContext) -> TypeRef {
        let element = T::mollie_type_of(context);

        context.types.get_or_add(Type::Array(element, Some(U)))
    }
}

mollie_arg_types![A];
mollie_arg_types![A, B];
mollie_arg_types![A, B, C];
mollie_arg_types![A, B, C, D];
mollie_arg_types![A, B, C, D, E];
mollie_arg_types![A, B, C, D, E, F];
mollie_arg_types![A, B, C, D, E, F, G];

pub fn func<Args: MollieMultipleTypeOf, Returns: MollieTypeOf>(context: &mut TypeContext) -> TypeRef {
    let args = Args::mollie_type_of(context).collect();
    let returns = Returns::mollie_type_of(context);

    context.types.get_or_add(Type::Func(args, returns))
}

#[derive(Debug)]
pub struct AdtBuilder<'a> {
    context: &'a mut TypeContext,
    name: Option<String>,
    collectable: bool,
    variants: Vec<(Option<String>, AdtBuilderVariant)>,
    generics: usize,
    kind: AdtKind,
}

impl<'a> AdtBuilder<'a> {
    pub fn new_struct<T: Into<String>>(context: &'a mut TypeContext, name: T) -> Self {
        Self {
            context,
            name: Some(name.into()),
            collectable: true,
            variants: vec![(None, vec![])],
            generics: 0,
            kind: AdtKind::Struct,
        }
    }

    pub fn new_enum<T: Into<String>>(context: &'a mut TypeContext, name: T) -> Self {
        Self {
            context,
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
            self.context.types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize))),
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
            variant.push((name.into(), T::mollie_type_of(self.context), Some(default.into())));
        }

        self
    }

    pub fn field<T: MollieTypeOf>(mut self, name: impl Into<String>) -> Self {
        if let Some(index) = T::generic_index() {
            assert!(self.generics > index, "pls add generic param");
        }

        if let Some((_, variant)) = self.variants.last_mut() {
            variant.push((name.into(), T::mollie_type_of(self.context), None));
        }

        self
    }

    pub fn field_ty<T: Into<String>>(mut self, name: T, ty: TypeRef) -> Self {
        if let Some((_, variant)) = self.variants.last_mut() {
            variant.push((name.into(), ty, None));
        }

        self
    }

    pub fn finish_in_module(self, module: ModuleId) -> AdtRef {
        let adt = Adt {
            name: self.name,
            collectable: self.collectable,
            kind: self.kind,
            generics: self.generics,
            variants: IndexBoxedSlice::from_iter(self.variants.into_iter().enumerate().map(|(discriminant, (name, fields))| {
                AdtVariant {
                    name,
                    discriminant,
                    fields: fields
                        .into_iter()
                        .map(|(name, ty, default_value)| AdtVariantField { name, ty, default_value })
                        .collect(),
                }
            })),
        };

        self.context.register_adt_in_module(module, adt)
    }

    pub fn finish(self) -> AdtRef {
        self.finish_in_module(ModuleId::ZERO)
    }
}

#[derive(Debug)]
pub struct TraitBuilder<'a> {
    context: &'a mut TypeContext,
    name: String,
    generics: usize,
    functions: IndexVec<TraitFuncRef, (String, Vec<Arg<TypeRef>>, TypeRef)>,
}

impl<'a> TraitBuilder<'a> {
    pub fn new<T: Into<String>>(context: &'a mut TypeContext, name: T) -> Self {
        Self {
            context,
            name: name.into(),
            functions: IndexVec::new(),
            generics: 1,
        }
    }

    pub fn func<T: Into<String>, I: IntoIterator<Item = (T, TypeRef)>>(mut self, name: T, params: I, returns: TypeRef) -> Self {
        let mut args = vec![Arg {
            name: "self".into(),
            kind: ArgType::This,
            ty: self.context.types.get_or_add(Type::Generic(0)),
        }];

        args.extend(params.into_iter().map(|(name, ty)| Arg {
            name: name.into(),
            kind: ArgType::Regular,
            ty,
        }));

        self.functions.push((name.into(), args, returns));

        self
    }

    pub fn static_func<T: Into<String>, I: IntoIterator<Item = (T, TypeRef)>>(mut self, name: T, params: I, returns: TypeRef) -> Self {
        self.functions.push((
            name.into(),
            params
                .into_iter()
                .map(|(name, ty)| Arg {
                    name: name.into(),
                    kind: ArgType::Regular,
                    ty,
                })
                .collect(),
            returns,
        ));

        self
    }

    pub const fn add_generic(mut self) -> Self {
        self.generics += 1;

        self
    }

    pub fn finish_in_module(self, module: ModuleId) -> TraitRef {
        let r#trait = Trait {
            name: self.name,
            generics: self.generics,
            functions: self
                .functions
                .into_iter()
                .map(|(_, (name, args, returns))| TraitFunc {
                    name,
                    args: args.into_boxed_slice(),
                    returns,
                })
                .collect(),
        };

        self.context.register_trait_in_module(module, r#trait)
    }

    pub fn finish(self) -> TraitRef {
        self.finish_in_module(ModuleId::ZERO)
    }
}

pub struct VTableBuilder<'a, E> {
    context: &'a mut TypedASTContext<E>,
    target: TypeRef,
    generics: usize,
    functions: IndexVec<VFuncRef, VTableFunc>,
    external_functions: IndexVec<VFuncRef, FunctionBody<E>>,
}

impl<'a, E> VTableBuilder<'a, E> {
    pub const fn new(context: &'a mut TypedASTContext<E>, target: TypeRef) -> Self {
        Self {
            context,
            target,
            generics: 0,
            functions: IndexVec::new(),
            external_functions: IndexVec::new(),
        }
    }

    pub fn add_generic(mut self) -> Self {
        self.generics += 1;

        self
    }

    pub fn func<T: Into<String>, I: IntoIterator<Item = TypeRef>>(mut self, name: T, external_name: &'static str, args: I, returns: TypeRef) -> Self {
        let ty = self.context.type_context.types.get_or_add(Type::Func(args.into_iter().collect(), returns));

        self.external_functions.push(FunctionBody::Import(external_name));
        self.functions.push(VTableFunc {
            trait_func: None,
            name: name.into(),
            arg_names: Vec::new(),
            ty,
        });

        self
    }

    pub fn finish(self) -> VTableRef {
        self.context.vtables.insert(self.external_functions);
        self.context.type_context.vtables.insert(VTableGenerator {
            ty: self.target,
            origin_trait: None,
            generics: (0..self.generics)
                .map(|generic| self.context.type_context.types.get_or_add(Type::Generic(generic)))
                .collect(),
            functions: self.functions,
        })
    }
}
