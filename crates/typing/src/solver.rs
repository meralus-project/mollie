use std::{
    fmt::{self, Display, Write},
    hash::{DefaultHasher, Hash, Hasher},
};

use indexmap::IndexMap;
use mollie_index::{Idx, IndexVec};

use crate::{ComplexType, ComplexTypeKind, ComplexTypeRef, ComplexTypeVariantRef, FieldType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, VTableRef};

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub id: usize,
    pub ty: TypeInfoRef,
}

#[derive(Debug)]
pub enum TypeUnificationError {
    TypeMismatch(TypeInfoRef, TypeInfoRef),
    UnimplementedTrait(TraitRef, TypeInfoRef),
    ArraySizeMismatch(usize, usize),
    UnknownType(TypeInfoRef),
}

#[derive(Debug, Default)]
pub struct TypeSolver {
    name_to_info: IndexMap<String, TypeInfoRef>,
    pub type_infos: IndexVec<TypeInfoRef, TypeInfo>,
    frames: Vec<IndexMap<String, Variable>>,
    pub vtables: IndexMap<FieldType, IndexMap<Option<TraitRef>, VTableRef>>,
    pub errors: Vec<TypeUnificationError>,
}

impl TypeSolver {
    pub fn finalize(&mut self) {
        for (info_ref, info) in self.type_infos.iter_mut() {
            if let TypeInfo::Unknown(fallback) = info {
                if let Some(fallback) = fallback {
                    *info = TypeInfo::Ref(*fallback);
                } else {
                    self.errors.push(TypeUnificationError::UnknownType(info_ref));
                }
            }
        }
    }

    pub fn hash_value_into<T: Hasher>(&self, state: &mut T, info: &TypeInfo) {
        match info {
            TypeInfo::Unknown(None) => (),
            TypeInfo::Primitive(primitive_type) => primitive_type.hash(state),
            TypeInfo::Func(args, returns) => {
                for &arg in args {
                    self.hash_into(state, arg);
                }

                self.hash_into(state, *returns);
            }
            TypeInfo::Trait(trait_ref, type_args) => {
                trait_ref.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            TypeInfo::Complex(complex_type_ref, complex_type_kind, type_args) => {
                complex_type_ref.hash(state);
                complex_type_kind.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            &TypeInfo::Array(element, size) => {
                self.hash_into(state, element);

                size.hash(state);
            }
            _ => unreachable!(),
        }
    }

    pub fn hash_into<T: Hasher>(&self, state: &mut T, info: TypeInfoRef) {
        self.hash_value_into(state, self.get_info2(info));
    }

    pub fn hash_of_type_info(&self, info: &TypeInfo) -> u64 {
        let mut state = DefaultHasher::new();

        self.hash_value_into(&mut state, info);

        state.finish()
    }

    pub fn hash_of(&self, info: TypeInfoRef) -> u64 {
        let mut state = DefaultHasher::new();

        self.hash_into(&mut state, info);

        state.finish()
    }

    pub fn push_frame(&mut self) {
        self.frames.push(IndexMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn get_var<T: AsRef<str>>(&self, name: T) -> Option<Variable> {
        let expected_name = name.as_ref();

        for frame in self.frames.iter().rev() {
            if let Some(var) = frame.get(expected_name) {
                return Some(*var);
            }
        }

        None
    }

    pub fn add_var<T: Into<String>>(&mut self, name: T, ty: TypeInfoRef) -> Variable {
        let Some(frame) = self.frames.last_mut() else { unreachable!() };

        let var = Variable { id: frame.len(), ty };

        frame.insert(name.into(), var);

        var
    }

    pub fn type_info_refs(&self) -> impl Iterator<Item = TypeInfoRef> {
        (0..self.type_infos.len()).map(TypeInfoRef::new)
    }

    pub fn type_infos(&self) -> impl Iterator<Item = &TypeInfo> {
        self.type_infos.values()
    }

    pub fn get_named_info<T: AsRef<str>>(&self, name: T) -> Option<TypeInfoRef> {
        self.name_to_info.get(name.as_ref()).copied()
    }

    pub fn remove_named_info<T: AsRef<str>>(&mut self, name: T) -> Option<TypeInfoRef> {
        self.name_to_info.swap_remove(name.as_ref())
    }

    pub fn add_name_for_info<T: Into<String>>(&mut self, name: T, info_ref: TypeInfoRef) {
        self.name_to_info.insert(name.into(), info_ref);
    }

    pub fn add_named_info<T: Into<String>>(&mut self, name: T, info: TypeInfo) -> TypeInfoRef {
        let result = TypeInfoRef(self.type_infos.len());

        self.type_infos.push(info);
        self.name_to_info.insert(name.into(), result);

        result
    }

    pub fn add_info(&mut self, info: TypeInfo) -> TypeInfoRef {
        let result = TypeInfoRef(self.type_infos.len());

        self.type_infos.push(info);

        result
    }

    pub fn get_info(&self, info_ref: TypeInfoRef) -> &TypeInfo {
        match &self.type_infos[info_ref] {
            &TypeInfo::Ref(info_ref) => self.get_info(info_ref),
            info => info,
        }
    }

    pub fn get_info2(&self, info_ref: TypeInfoRef) -> &TypeInfo {
        match &self.type_infos[info_ref] {
            &TypeInfo::Unknown(Some(info_ref)) | &TypeInfo::Ref(info_ref) => self.get_info2(info_ref),
            info => info,
        }
    }

    pub fn get_maybe_info(&self, info_ref: TypeInfoRef) -> Option<&TypeInfo> {
        match self.type_infos.get(info_ref) {
            Some(&TypeInfo::Ref(info_ref)) => self.get_maybe_info(info_ref),
            info => info,
        }
    }

    pub fn get_field_type(&self, info_ref: TypeInfoRef) -> FieldType {
        match self.get_info(info_ref) {
            TypeInfo::Unknown(_) => unimplemented!(),
            &TypeInfo::Primitive(ty) => FieldType::Primitive(ty),
            TypeInfo::Ref(_) => unreachable!(),
            TypeInfo::Func(..) => todo!(),
            TypeInfo::Trait(trait_ref, args) => FieldType::Trait(*trait_ref, args.iter().map(|&arg| self.get_field_type(arg)).collect()),
            TypeInfo::Complex(complex_type_ref, kind, args) => {
                FieldType::Complex(*complex_type_ref, *kind, args.iter().map(|&arg| self.get_field_type(arg)).collect())
            }
            &TypeInfo::Array(info_ref, size) => FieldType::Array(Box::new(self.get_field_type(info_ref)), size),
        }
    }

    pub fn unify(&mut self, got: TypeInfoRef, expected: TypeInfoRef) {
        if got == expected {
            return;
        }

        match (self.get_info(got).clone(), self.get_maybe_info(expected).cloned()) {
            (_, None)
            | (TypeInfo::Unknown(None), Some(TypeInfo::Unknown(None)))
            | (TypeInfo::Unknown(Some(_)), Some(TypeInfo::Unknown(Some(_))))
            | (TypeInfo::Primitive(PrimitiveType::Component), Some(TypeInfo::Complex(_, ComplexTypeKind::Component, _)))
            | (TypeInfo::Complex(_, ComplexTypeKind::Component, _), Some(TypeInfo::Primitive(PrimitiveType::Component))) => (),
            (_, Some(TypeInfo::Unknown(None))) => self.type_infos[expected] = TypeInfo::Ref(got),
            (TypeInfo::Unknown(_), _) => self.type_infos[got] = TypeInfo::Ref(expected),
            (_, Some(TypeInfo::Ref(b))) => self.unify(got, b),
            (TypeInfo::Ref(a), _) => self.unify(a, expected),
            (TypeInfo::Func(a_args, a_out), Some(TypeInfo::Func(b_args, b_out))) => {
                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a, b);
                }

                self.unify(a_out, b_out);
            }
            (TypeInfo::Array(a, a_size), Some(TypeInfo::Array(b, b_size))) => {
                self.unify(a, b);

                match (a_size, b_size) {
                    (None, None) => (),
                    (None, Some(_)) => self.type_infos[b] = TypeInfo::Array(b, None),
                    (Some(_), None) => self.type_infos[a] = TypeInfo::Array(a, None),
                    (Some(a_size), Some(b_size)) => {
                        if a_size != b_size {
                            self.errors.push(TypeUnificationError::ArraySizeMismatch(a_size, b_size));
                        }
                    }
                }
            }
            (TypeInfo::Complex(a_ty, _, a_args), Some(TypeInfo::Complex(b_ty, _, b_args))) => {
                if a_ty != b_ty {
                    self.errors.push(TypeUnificationError::TypeMismatch(got, expected));
                }

                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a, b);
                }
            }
            (TypeInfo::Trait(trait_ref, _), Some(_)) => {
                if self
                    .vtables
                    .get(&self.get_field_type(expected))
                    .is_none_or(|vtables| !vtables.contains_key(&Some(trait_ref)))
                {
                    self.errors.push(TypeUnificationError::UnimplementedTrait(trait_ref, expected));
                }
            }
            (_, Some(TypeInfo::Trait(trait_ref, _))) => {
                if self
                    .vtables
                    .get(&self.get_field_type(got))
                    .is_none_or(|vtables| !vtables.contains_key(&Some(trait_ref)))
                {
                    self.errors.push(TypeUnificationError::UnimplementedTrait(trait_ref, got));
                }
            }
            (a_info, Some(b_info)) => {
                if a_info != b_info {
                    self.errors.push(TypeUnificationError::TypeMismatch(got, expected));
                }
            }
        }
    }

    pub const fn len(&self) -> usize {
        self.type_infos.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.type_infos.is_empty()
    }

    pub fn contains_unknown(&self, ty: TypeInfoRef) -> bool {
        match &self.type_infos[ty] {
            TypeInfo::Unknown(fallback) => fallback.is_none(),
            &TypeInfo::Ref(ty) => self.contains_unknown(ty),
            TypeInfo::Func(args, output) => args.iter().any(|&arg| self.contains_unknown(arg)) || self.contains_unknown(*output),
            TypeInfo::Complex(_, _, args) => args.iter().any(|&arg| self.contains_unknown(arg)),
            &TypeInfo::Array(element, _) => self.contains_unknown(element),
            _ => false,
        }
    }

    fn generic_fmt(
        &self,
        f: &mut fmt::Formatter,
        generics: &GenericFmt,
        generic: usize,
        this: Option<TypeInfoRef>,
        storage: &IndexVec<ComplexTypeRef, ComplexType>,
    ) -> fmt::Result {
        match generics {
            GenericFmt::Field(type_args, other_generics) => match type_args.get(generic) {
                Some(arg) => self.fmt_field(f, arg, this, storage, other_generics),
                None => write!(f, "<generic #{generic}>"),
            },
            GenericFmt::TypeInfo(type_args) => match type_args.get(generic) {
                Some(&arg) => self.fmt_type(f, arg, this, storage),
                None => write!(f, "<generic #{generic}>"),
            },
        }
    }

    fn fmt_field(
        &self,
        f: &mut fmt::Formatter,
        field_type: &FieldType,
        this: Option<TypeInfoRef>,
        storage: &IndexVec<ComplexTypeRef, ComplexType>,
        generics: &GenericFmt,
    ) -> fmt::Result {
        match field_type {
            FieldType::This => match this {
                Some(ty) => self.fmt_type(f, ty, this, storage),
                None => f.write_str("<Self>"),
            },
            FieldType::Unknown(fallback) => match fallback.as_deref() {
                Some(field_type) => self.fmt_field(f, field_type, this, storage, generics),
                None => f.write_str("<unknown>"),
            },
            &FieldType::Generic(idx) => self.generic_fmt(f, generics, idx, this, storage),
            FieldType::Primitive(primitive_type) => primitive_type.fmt(f),
            FieldType::Array(element_type, size) => {
                self.fmt_field(f, element_type, this, storage, generics)?;

                f.write_char('[')?;

                if let Some(size) = size {
                    size.fmt(f)?;
                }

                f.write_char(']')
            }
            FieldType::Func(params, output) => {
                f.write_str("fn(")?;

                let mut first = true;

                for ty in params {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }

                    self.fmt_field(f, ty, this, storage, generics)?;
                }

                f.write_str(") -> ")?;

                self.fmt_field(f, output.as_ref(), this, storage, generics)
            }
            FieldType::Trait(..) => todo!(),
            FieldType::Complex(complex_type, kind, type_args) => {
                let complex_type = &storage[*complex_type];

                if let Some(name) = &complex_type.name {
                    name.fmt(f)?;
                }

                f.write_char('{')?;

                match kind {
                    ComplexTypeKind::Struct | ComplexTypeKind::Component => {
                        let mut first = true;

                        for (name, ty, _) in complex_type.variants[ComplexTypeVariantRef::new(0)].fields.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            name.fmt(f)?;

                            f.write_str(": ")?;

                            self.fmt_field(f, ty, this, storage, &GenericFmt::Field(type_args.as_ref(), generics))?;
                        }
                    }
                    ComplexTypeKind::Enum => {
                        let mut first = true;

                        for variant in complex_type.variants.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            if let Some(name) = &variant.name {
                                name.fmt(f)?;
                            }

                            f.write_char('{')?;

                            let mut first = true;

                            for (name, ty, _) in variant.fields.values() {
                                if first {
                                    first = false;
                                } else {
                                    f.write_str(", ")?;
                                }

                                name.fmt(f)?;

                                f.write_str(": ")?;

                                self.fmt_field(f, ty, this, storage, generics)?;
                            }

                            f.write_char('}')?;
                        }
                    }
                }

                f.write_char('}')
            }
        }
    }

    pub fn fmt_type(&self, f: &mut fmt::Formatter, ty: TypeInfoRef, this: Option<TypeInfoRef>, storage: &IndexVec<ComplexTypeRef, ComplexType>) -> fmt::Result {
        match &self.type_infos[ty] {
            TypeInfo::Unknown(fallback) => match fallback {
                &Some(ty) => self.fmt_type(f, ty, this, storage),
                None => f.write_str("<unknown>"),
            },
            TypeInfo::Primitive(primitive_type) => primitive_type.fmt(f),
            &TypeInfo::Ref(ty) => self.fmt_type(f, ty, this, storage),
            TypeInfo::Func(args, returns) => {
                f.write_str("fn(")?;

                let mut first = true;

                for &ty in args {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }

                    self.fmt_type(f, ty, this, storage)?;
                }

                f.write_str(") -> ")?;

                self.fmt_type(f, *returns, this, storage)
            }
            TypeInfo::Trait(..) => todo!(),
            TypeInfo::Complex(complex_type, kind, type_args) => {
                let complex_type = &storage[*complex_type];

                if let Some(name) = &complex_type.name {
                    name.fmt(f)?;
                }

                f.write_char('{')?;

                match kind {
                    ComplexTypeKind::Struct | ComplexTypeKind::Component => {
                        let mut first = true;

                        for (name, ty, _) in complex_type.variants[ComplexTypeVariantRef::new(0)].fields.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            name.fmt(f)?;

                            f.write_str(": ")?;

                            self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()))?;
                        }
                    }
                    ComplexTypeKind::Enum => {
                        let mut first = true;

                        for variant in complex_type.variants.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            if let Some(name) = &variant.name {
                                name.fmt(f)?;
                            }

                            f.write_char('{')?;

                            let mut first = true;

                            for (name, ty, _) in variant.fields.values() {
                                if first {
                                    first = false;
                                } else {
                                    f.write_str(", ")?;
                                }

                                name.fmt(f)?;

                                f.write_str(": ")?;

                                self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()))?;
                            }

                            f.write_char('}')?;
                        }
                    }
                }

                f.write_char('}')
            }
            &TypeInfo::Array(element_type, size) => {
                self.fmt_type(f, element_type, this, storage)?;

                f.write_char('[')?;

                if let Some(size) = size {
                    size.fmt(f)?;
                }

                f.write_char(']')
            }
        }
    }
}

enum GenericFmt<'a> {
    Field(&'a [FieldType], &'a Self),
    TypeInfo(&'a [TypeInfoRef]),
}
