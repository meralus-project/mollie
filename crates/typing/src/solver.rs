use std::{
    fmt::{self, Display, Write},
    hash::{DefaultHasher, Hash, Hasher},
};

use indexmap::IndexMap;
use mollie_index::{Idx, IndexVec};
use serde::Serialize;

use crate::{Adt, AdtKind, AdtRef, AdtVariantRef, FieldType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, VTableRef};

#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Variable {
    pub id: usize,
    pub frame: usize,
    pub ty: TypeInfoRef,
}

#[derive(Debug)]
pub enum TypeUnificationError {
    TypeMismatch(TypeInfoRef, TypeInfoRef),
    UnimplementedTrait(TraitRef, TypeInfoRef),
    ArraySizeMismatch(usize, usize),
    UnknownType(TypeInfoRef),
}

pub trait TypeStorage {
    fn get_adt(&self, adt_ref: AdtRef) -> &Adt;
    fn get_trait_name(&self, trait_ref: TraitRef) -> Option<&str>;
}

#[derive(Debug, Default)]
pub struct TypeSolver {
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

    pub fn find_vtable(&self, field_type: &FieldType) -> Option<&IndexMap<Option<TraitRef>, VTableRef>> {
        if let Some(v) = self.vtables.get(field_type) {
            Some(v)
        } else {
            for (ty, v) in &self.vtables {
                if ty.try_unify(field_type) {
                    return Some(v);
                }
            }

            None
        }
    }

    pub fn hash_value_into<T: Hasher>(&self, state: &mut T, info: &TypeInfo) {
        match info {
            TypeInfo::Unknown(None) => (),
            TypeInfo::Primitive(primitive_type) => primitive_type.hash(state),
            TypeInfo::Func(args, returns) => {
                "fn".hash(state);

                for &arg in args {
                    self.hash_into(state, arg.inner());
                }

                self.hash_into(state, *returns);
            }
            TypeInfo::Trait(trait_ref, type_args) => {
                "trait".hash(state);

                trait_ref.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            TypeInfo::Adt(adt_ref, adt_kind, type_args) => {
                "adt".hash(state);

                adt_ref.hash(state);
                adt_kind.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            &TypeInfo::Array(element, size) => {
                "array".hash(state);

                self.hash_into(state, element);

                size.hash(state);
            }
            v => unreachable!("can't hash {v:?}"),
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

    pub const fn current_frame(&self) -> usize {
        self.frames.len() - 1
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
        let frame = self.current_frame();
        let vars = &mut self.frames[frame];

        let var = Variable { id: vars.len(), frame, ty };

        vars.insert(name.into(), var);

        var
    }

    pub fn type_info_refs(&self) -> impl Iterator<Item = TypeInfoRef> {
        (0..self.type_infos.len()).map(TypeInfoRef::new)
    }

    pub fn type_infos(&self) -> impl Iterator<Item = &TypeInfo> {
        self.type_infos.values()
    }

    fn is_constant_info(&self, info: &TypeInfo) -> bool {
        match info {
            TypeInfo::Ref(_) | TypeInfo::Generic(..) | TypeInfo::Unknown(_) => false,
            TypeInfo::Primitive(_) => true,
            TypeInfo::Func(func_args, returns) => {
                func_args.iter().all(|arg| self.is_constant_info(&self.type_infos[arg.inner()])) && self.is_constant_info(&self.type_infos[*returns])
            }
            TypeInfo::Trait(_, type_args) => type_args.iter().all(|type_arg| self.is_constant_info(&self.type_infos[*type_arg])),
            TypeInfo::Adt(.., type_args) => type_args.iter().all(|type_arg| self.is_constant_info(&self.type_infos[*type_arg])),
            &TypeInfo::Array(element, _) => self.is_constant_info(&self.type_infos[element]),
        }
    }

    pub fn add_info(&mut self, info: TypeInfo) -> TypeInfoRef {
        if self.is_constant_info(&info)
            && let Some((result, _)) = self.type_infos.iter().find(|(_, i)| i == &&info)
        {
            result
        } else {
            let result = TypeInfoRef(self.type_infos.len());

            self.type_infos.push(info);

            result
        }
    }

    pub fn get_info3(&self, info_ref: TypeInfoRef) -> (TypeInfoRef, &TypeInfo) {
        match &self.type_infos[info_ref] {
            &TypeInfo::Ref(info_ref) | &TypeInfo::Generic(_, Some(info_ref)) => self.get_info3(info_ref),
            info => (info_ref, info),
        }
    }

    pub fn get_info(&self, info_ref: TypeInfoRef) -> &TypeInfo {
        match &self.type_infos[info_ref] {
            &TypeInfo::Ref(info_ref) | &TypeInfo::Generic(_, Some(info_ref)) => self.get_info(info_ref),
            info => info,
        }
    }

    pub fn get_info2(&self, info_ref: TypeInfoRef) -> &TypeInfo {
        match &self.type_infos[info_ref] {
            &TypeInfo::Unknown(Some(info_ref)) | &TypeInfo::Generic(_, Some(info_ref)) | &TypeInfo::Ref(info_ref) => self.get_info2(info_ref),
            info => info,
        }
    }

    pub fn get_maybe_info(&self, info_ref: TypeInfoRef) -> Option<&TypeInfo> {
        match self.type_infos.get(info_ref) {
            Some(&TypeInfo::Ref(info_ref)) => self.get_maybe_info(info_ref),
            info => info,
        }
    }

    pub fn get_maybe_info2(&self, info_ref: TypeInfoRef) -> (TypeInfoRef, Option<&TypeInfo>) {
        match self.type_infos.get(info_ref) {
            Some(&TypeInfo::Ref(info_ref)) => self.get_maybe_info2(info_ref),
            info => (info_ref, info),
        }
    }

    pub fn get_field_type(&self, info_ref: TypeInfoRef) -> FieldType {
        match self.get_info(info_ref) {
            TypeInfo::Unknown(_) => unimplemented!(),
            &TypeInfo::Generic(idx, _) => FieldType::Generic(idx, Some(info_ref)),
            &TypeInfo::Primitive(ty) => FieldType::Primitive(ty),
            TypeInfo::Ref(_) => unreachable!(),
            TypeInfo::Func(..) => todo!(),
            TypeInfo::Trait(trait_ref, args) => FieldType::Trait(*trait_ref, args.iter().map(|&arg| self.get_field_type(arg)).collect()),
            TypeInfo::Adt(adt_ref, adt_kind, args) => FieldType::Adt(*adt_ref, *adt_kind, args.iter().map(|&arg| self.get_field_type(arg)).collect()),
            &TypeInfo::Array(info_ref, size) => FieldType::Array(Box::new(self.get_field_type(info_ref)), size),
        }
    }

    pub fn solve_generic_args(&mut self, ty: TypeInfoRef, args: &[TypeInfoRef]) {
        let (ty, ty_val) = self.get_info3(ty);

        match ty_val.clone() {
            TypeInfo::Generic(idx, None) => self.type_infos[ty] = TypeInfo::Generic(idx, args.get(idx).copied()),
            TypeInfo::Ref(type_info_ref) | TypeInfo::Array(type_info_ref, _) => self.solve_generic_args(type_info_ref, args),
            TypeInfo::Func(func_args, type_info_ref) => {
                for arg in func_args {
                    self.solve_generic_args(arg.inner(), args);
                }

                self.solve_generic_args(type_info_ref, args);
            }
            TypeInfo::Trait(_, type_info_refs) | TypeInfo::Adt(.., type_info_refs) => {
                for arg in type_info_refs {
                    self.solve_generic_args(arg, args);
                }
            }
            i => println!("can't solve {i:?} with {args:?}"),
        }
    }

    #[track_caller]
    pub fn unify(&mut self, got: TypeInfoRef, expected: TypeInfoRef) {
        let (got, got_val) = self.get_info3(got);
        let (expected, expected_val) = self.get_maybe_info2(expected);

        if got == expected {
            return;
        }

        match (got_val.clone(), expected_val.cloned()) {
            (TypeInfo::Generic(_, Some(got)), Some(TypeInfo::Generic(idx, None))) => self.type_infos[expected] = TypeInfo::Generic(idx, Some(got)),
            (TypeInfo::Generic(idx, None), Some(TypeInfo::Generic(_, Some(expected)))) => self.type_infos[got] = TypeInfo::Generic(idx, Some(expected)),
            (_, None | Some(TypeInfo::Generic(_, Some(_))))
            | (TypeInfo::Generic(_, Some(_)), _)
            | (TypeInfo::Unknown(None), Some(TypeInfo::Unknown(None)))
            | (TypeInfo::Unknown(Some(_)), Some(TypeInfo::Unknown(Some(_))))
            | (TypeInfo::Primitive(PrimitiveType::Component), Some(TypeInfo::Adt(_, AdtKind::Component, _)))
            | (TypeInfo::Adt(_, AdtKind::Component, _), Some(TypeInfo::Primitive(PrimitiveType::Component))) => (),
            (_, Some(TypeInfo::Generic(idx, None))) => self.type_infos[expected] = TypeInfo::Generic(idx, Some(got)),
            (TypeInfo::Generic(idx, None), _) => self.type_infos[got] = TypeInfo::Generic(idx, Some(expected)),
            (_, Some(TypeInfo::Unknown(None))) => self.type_infos[expected] = TypeInfo::Ref(got),
            (TypeInfo::Unknown(_), _) => self.type_infos[got] = TypeInfo::Ref(expected),
            (_, Some(TypeInfo::Ref(b))) => self.unify(got, b),
            (TypeInfo::Ref(a), _) => self.unify(a, expected),
            (TypeInfo::Func(a_args, a_out), Some(TypeInfo::Func(b_args, b_out))) => {
                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a.inner(), b.inner());
                }

                self.unify(a_out, b_out);
            }
            (TypeInfo::Array(a, a_size), Some(TypeInfo::Array(b, b_size))) => {
                self.unify(a, b);

                match (a_size, b_size) {
                    (None, None) => (),
                    (None, Some(_)) => self.type_infos[expected] = TypeInfo::Array(b, None),
                    (Some(_), None) => self.type_infos[got] = TypeInfo::Array(a, None),
                    (Some(a_size), Some(b_size)) => {
                        if a_size != b_size {
                            self.errors.push(TypeUnificationError::ArraySizeMismatch(a_size, b_size));
                        }
                    }
                }
            }
            (TypeInfo::Adt(a_ty, _, a_args), Some(TypeInfo::Adt(b_ty, _, b_args))) => {
                if a_ty != b_ty {
                    self.errors.push(TypeUnificationError::TypeMismatch(got, expected));
                }

                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a, b);
                }
            }
            (TypeInfo::Trait(trait_ref, _), Some(_)) => {
                if self
                    .find_vtable(&self.get_field_type(expected))
                    .is_none_or(|vtables| !vtables.contains_key(&Some(trait_ref)))
                {
                    self.errors.push(TypeUnificationError::UnimplementedTrait(trait_ref, expected));
                }
            }
            (_, Some(TypeInfo::Trait(trait_ref, _))) => {
                if self
                    .find_vtable(&self.get_field_type(got))
                    .is_none_or(|vtables| !vtables.contains_key(&Some(trait_ref)))
                {
                    self.errors.push(TypeUnificationError::UnimplementedTrait(trait_ref, got));
                }
            }
            (a_info, Some(b_info)) => {
                if a_info != b_info {
                    self.errors.push(TypeUnificationError::TypeMismatch(expected, got));
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
            TypeInfo::Func(args, output) => args.iter().any(|&arg| self.contains_unknown(arg.inner())) || self.contains_unknown(*output),
            TypeInfo::Adt(_, _, args) => args.iter().any(|&arg| self.contains_unknown(arg)),
            &TypeInfo::Array(element, _) => self.contains_unknown(element),
            _ => false,
        }
    }

    fn generic_fmt<S: TypeStorage>(
        &self,
        f: &mut fmt::Formatter,
        generics: &GenericFmt,
        generic: usize,
        this: Option<TypeInfoRef>,
        storage: &S,
        short: bool,
    ) -> fmt::Result {
        match generics {
            GenericFmt::Field(type_args, other_generics) => match type_args.get(generic) {
                Some(arg) => self.fmt_field(f, arg, this, storage, other_generics, short),
                None => write!(f, "<generic #{generic}>"),
            },
            GenericFmt::TypeInfo(type_args) => match type_args.get(generic) {
                Some(&arg) => self.fmt_type(f, arg, this, storage, short),
                None => write!(f, "<generic #{generic}>"),
            },
        }
    }

    fn fmt_field<S: TypeStorage>(
        &self,
        f: &mut fmt::Formatter,
        field_type: &FieldType,
        this: Option<TypeInfoRef>,
        storage: &S,
        generics: &GenericFmt,
        short: bool,
    ) -> fmt::Result {
        match field_type {
            FieldType::This => match this {
                Some(ty) => self.fmt_type(f, ty, this, storage, short),
                None => f.write_str("<Self>"),
            },
            FieldType::Unknown(fallback) => match fallback.as_deref() {
                Some(field_type) => self.fmt_field(f, field_type, this, storage, generics, short),
                None => f.write_str("<unknown>"),
            },
            &FieldType::Generic(idx, _) => self.generic_fmt(f, generics, idx, this, storage, short),
            FieldType::Primitive(primitive_type) => primitive_type.fmt(f),
            FieldType::Array(element_type, size) => {
                self.fmt_field(f, element_type, this, storage, generics, short)?;

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

                    self.fmt_field(f, ty.as_inner(), this, storage, generics, short)?;
                }

                f.write_str(") -> ")?;

                self.fmt_field(f, output.as_ref(), this, storage, generics, short)
            }
            FieldType::Trait(t, _) => match storage.get_trait_name(*t) {
                Some(name) => name.fmt(f),
                None => write!(f, "<trait: {}>", t.index()),
            },
            FieldType::Adt(adt_ref, adt_kind, type_args) => {
                let adt = storage.get_adt(*adt_ref);

                if let Some(name) = &adt.name {
                    name.fmt(f)?;

                    if !type_args.is_empty() {
                        f.write_char('<')?;

                        let mut first = true;

                        for arg in type_args {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            self.fmt_field(f, arg, this, storage, generics, short)?;
                        }

                        f.write_char('>')?;
                    }

                    if short {
                        return Ok(());
                    }

                    f.write_char(' ')?;
                } else if !type_args.is_empty() {
                    f.write_char('<')?;

                    let mut first = true;

                    for arg in type_args {
                        if first {
                            first = false;
                        } else {
                            f.write_str(", ")?;
                        }

                        self.fmt_field(f, arg, this, storage, generics, short)?;
                    }

                    f.write_char('>')?;
                }

                if short {
                    return Ok(());
                }

                f.write_char('{')?;

                match adt_kind {
                    AdtKind::Struct | AdtKind::Component => {
                        let mut first = true;

                        f.write_char(' ')?;

                        if !adt.variants[AdtVariantRef::new(0)].fields.is_empty() {
                            for (name, ty, _) in adt.variants[AdtVariantRef::new(0)].fields.values() {
                                if first {
                                    first = false;
                                } else {
                                    f.write_str(", ")?;
                                }

                                name.fmt(f)?;

                                f.write_str(": ")?;

                                self.fmt_field(f, ty, this, storage, &GenericFmt::Field(type_args.as_ref(), generics), short)?;
                            }

                            f.write_char(' ')?;
                        }
                    }
                    AdtKind::Enum => {
                        let mut first = true;

                        f.write_char(' ')?;

                        for variant in adt.variants.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            if let Some(name) = &variant.name {
                                name.fmt(f)?;
                            }

                            if !variant.fields.is_empty() {
                                if variant.name.is_some() {
                                    f.write_char(' ')?;
                                }

                                f.write_char('{')?;
                                f.write_char(' ')?;

                                let mut first = true;

                                for (name, ty, _) in variant.fields.values() {
                                    if first {
                                        first = false;
                                    } else {
                                        f.write_str(", ")?;
                                    }

                                    name.fmt(f)?;

                                    f.write_str(": ")?;

                                    self.fmt_field(f, ty, this, storage, generics, short)?;
                                }

                                f.write_char(' ')?;
                                f.write_char('}')?;
                            }
                        }

                        if !adt.variants.is_empty() {
                            f.write_char(' ')?;
                        }
                    }
                }

                f.write_char('}')
            }
        }
    }

    pub fn fmt_type<S: TypeStorage>(&self, f: &mut fmt::Formatter, ty: TypeInfoRef, this: Option<TypeInfoRef>, storage: &S, short: bool) -> fmt::Result {
        match &self.type_infos[ty] {
            TypeInfo::Unknown(fallback) => match fallback {
                &Some(ty) => self.fmt_type(f, ty, this, storage, short),
                None => write!(f, "<unknown: {ty:?}>"),
            },
            TypeInfo::Generic(idx, fallback) => match fallback {
                &Some(ty) => self.fmt_type(f, ty, this, storage, short),
                None => write!(f, "<generic({idx}): {ty:?}>"),
            },
            TypeInfo::Primitive(primitive_type) => primitive_type.fmt(f),
            &TypeInfo::Ref(ty) => self.fmt_type(f, ty, this, storage, short),
            TypeInfo::Func(args, returns) => {
                f.write_str("fn(")?;

                let mut first = true;

                for &ty in args {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }

                    self.fmt_type(f, ty.inner(), this, storage, short)?;
                }

                f.write_str(") -> ")?;

                self.fmt_type(f, *returns, this, storage, short)
            }
            TypeInfo::Trait(t, _args) => match storage.get_trait_name(*t) {
                Some(name) => name.fmt(f),
                None => write!(f, "<trait: {}>", t.index()),
            },
            TypeInfo::Adt(adt_ref, kind, type_args) => {
                let adt = storage.get_adt(*adt_ref);

                if let Some(name) = &adt.name {
                    name.fmt(f)?;

                    if !type_args.is_empty() {
                        f.write_char('<')?;

                        let mut first = true;

                        for arg in type_args {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            self.fmt_type(f, *arg, this, storage, short)?;
                        }

                        f.write_char('>')?;
                    }

                    if short {
                        return Ok(());
                    }

                    f.write_char(' ')?;
                } else if !type_args.is_empty() {
                    f.write_char('<')?;

                    let mut first = true;

                    for arg in type_args {
                        if first {
                            first = false;
                        } else {
                            f.write_str(", ")?;
                        }

                        self.fmt_type(f, *arg, this, storage, short)?;
                    }

                    f.write_char('>')?;
                }

                if short {
                    return Ok(());
                }

                f.write_char('{')?;

                match kind {
                    AdtKind::Struct | AdtKind::Component => {
                        let mut first = true;

                        f.write_char(' ')?;

                        if !adt.variants[AdtVariantRef::ZERO].fields.is_empty() {
                            for (name, ty, _) in adt.variants[AdtVariantRef::ZERO].fields.values() {
                                if first {
                                    first = false;
                                } else {
                                    f.write_str(", ")?;
                                }

                                name.fmt(f)?;

                                f.write_str(": ")?;

                                self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()), short)?;
                            }

                            f.write_char(' ')?;
                        }
                    }
                    AdtKind::Enum => {
                        let mut first = true;

                        f.write_char(' ')?;

                        for variant in adt.variants.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            if let Some(name) = &variant.name {
                                name.fmt(f)?;
                            }

                            if !variant.fields.is_empty() {
                                if variant.name.is_some() {
                                    f.write_char(' ')?;
                                }

                                f.write_char('{')?;
                                f.write_char(' ')?;

                                let mut first = true;

                                for (name, ty, _) in variant.fields.values() {
                                    if first {
                                        first = false;
                                    } else {
                                        f.write_str(", ")?;
                                    }

                                    name.fmt(f)?;

                                    f.write_str(": ")?;

                                    self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()), short)?;
                                }

                                f.write_char(' ')?;
                                f.write_char('}')?;
                            }
                        }

                        if !adt.variants.is_empty() {
                            f.write_char(' ')?;
                        }
                    }
                }

                f.write_char('}')
            }
            &TypeInfo::Array(element_type, size) => {
                self.fmt_type(f, element_type, this, storage, short)?;

                f.write_char('[')?;

                if let Some(size) = size {
                    size.fmt(f)?;
                }

                f.write_char(']')
            }
        }
    }

    pub fn fmt_type_info<S: TypeStorage>(&self, f: &mut fmt::Formatter, ty: &TypeInfo, this: Option<TypeInfoRef>, storage: &S, short: bool) -> fmt::Result {
        match ty {
            TypeInfo::Unknown(fallback) => match fallback {
                &Some(ty) => self.fmt_type(f, ty, this, storage, short),
                None => write!(f, "<unknown>"),
            },
            TypeInfo::Generic(idx, fallback) => match fallback {
                &Some(ty) => self.fmt_type(f, ty, this, storage, short),
                None => write!(f, "<generic({idx})>"),
            },
            TypeInfo::Primitive(primitive_type) => primitive_type.fmt(f),
            &TypeInfo::Ref(ty) => self.fmt_type(f, ty, this, storage, short),
            TypeInfo::Func(args, returns) => {
                f.write_str("fn(")?;

                let mut first = true;

                for &ty in args {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }

                    self.fmt_type(f, ty.inner(), this, storage, short)?;
                }

                f.write_str(") -> ")?;

                self.fmt_type(f, *returns, this, storage, short)
            }
            TypeInfo::Trait(t, _) => match storage.get_trait_name(*t) {
                Some(name) => name.fmt(f),
                None => write!(f, "<trait: {}>", t.index()),
            },
            TypeInfo::Adt(adt_ref, adt_kind, type_args) => {
                let adt = storage.get_adt(*adt_ref);

                if let Some(name) = &adt.name {
                    name.fmt(f)?;

                    if !type_args.is_empty() {
                        f.write_char('<')?;

                        let mut first = true;

                        for arg in type_args {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            self.fmt_type(f, *arg, this, storage, short)?;
                        }

                        f.write_char('>')?;
                    }

                    if short {
                        return Ok(());
                    }

                    f.write_char(' ')?;
                } else if !type_args.is_empty() {
                    f.write_char('<')?;

                    let mut first = true;

                    for arg in type_args {
                        if first {
                            first = false;
                        } else {
                            f.write_str(", ")?;
                        }

                        self.fmt_type(f, *arg, this, storage, short)?;
                    }

                    f.write_char('>')?;
                }

                if short {
                    return Ok(());
                }

                f.write_char('{')?;

                match adt_kind {
                    AdtKind::Struct | AdtKind::Component => {
                        let mut first = true;

                        f.write_char(' ')?;

                        if !adt.variants[AdtVariantRef::new(0)].fields.is_empty() {
                            for (name, ty, _) in adt.variants[AdtVariantRef::new(0)].fields.values() {
                                if first {
                                    first = false;
                                } else {
                                    f.write_str(", ")?;
                                }

                                name.fmt(f)?;

                                f.write_str(": ")?;

                                self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()), short)?;
                            }

                            f.write_char(' ')?;
                        }
                    }
                    AdtKind::Enum => {
                        let mut first = true;

                        f.write_char(' ')?;

                        for variant in adt.variants.values() {
                            if first {
                                first = false;
                            } else {
                                f.write_str(", ")?;
                            }

                            if let Some(name) = &variant.name {
                                name.fmt(f)?;
                            }

                            if !variant.fields.is_empty() {
                                if variant.name.is_some() {
                                    f.write_char(' ')?;
                                }

                                f.write_char('{')?;
                                f.write_char(' ')?;

                                let mut first = true;

                                for (name, ty, _) in variant.fields.values() {
                                    if first {
                                        first = false;
                                    } else {
                                        f.write_str(", ")?;
                                    }

                                    name.fmt(f)?;

                                    f.write_str(": ")?;

                                    self.fmt_field(f, ty, this, storage, &GenericFmt::TypeInfo(type_args.as_ref()), short)?;
                                }

                                f.write_char(' ')?;
                                f.write_char('}')?;
                            }
                        }

                        if !adt.variants.is_empty() {
                            f.write_char(' ')?;
                        }
                    }
                }

                f.write_char('}')
            }
            &TypeInfo::Array(element_type, size) => {
                self.fmt_type(f, element_type, this, storage, short)?;

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
