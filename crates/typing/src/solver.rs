use std::collections::HashMap;

use mollie_index::IndexVec;
use mollie_shared::{MaybePositioned, Span};

use crate::{AdtRef, AdtVariantRef, FieldRef, IntType, PrimitiveType, Type, TypeInfo, TypeInfoRef, ty::TypeRef, type_context::TypeContext};

mollie_index::new_idx_type!(TypeFrameRef);

#[derive(Debug)]
pub struct TypeSolver<'a> {
    pub context: &'a mut TypeContext,
    pub type_infos: IndexVec<TypeInfoRef, MaybePositioned<TypeInfo>>,
    pub available_generics: HashMap<String, (TypeInfoRef, TypeRef)>,
    frames: IndexVec<TypeFrameRef, TypeFrame>,
}

#[derive(Debug, Default)]
struct TypeFrame(HashMap<String, TypeInfoRef>);

impl<'a> TypeSolver<'a> {
    pub fn from_context(context: &'a mut TypeContext) -> Self {
        Self {
            context,
            type_infos: IndexVec::new(),
            available_generics: HashMap::new(),
            frames: IndexVec::from_iter([TypeFrame::default()]),
        }
    }

    pub fn fork(&mut self) -> TypeSolver<'_> {
        TypeSolver::from_context(self.context)
    }

    pub fn finalize(&mut self) {
        for type_info in self.type_infos.values_mut() {
            if let TypeInfo::Unknown(Some(fallback)) = type_info.value {
                type_info.value = TypeInfo::Ref(fallback);
            }
        }
    }

    pub fn push_frame(&mut self) -> TypeFrameRef {
        self.frames.insert(TypeFrame::default())
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn set_var<T: Into<String>>(&mut self, name: T, ty: TypeInfoRef) {
        if let Some(frame) = self.frames.last_mut() {
            frame.0.insert(name.into(), ty);
        }
    }

    pub fn get_var(&self, name: impl AsRef<str>) -> Option<(TypeFrameRef, TypeInfoRef)> {
        let name = name.as_ref();

        for (frame_ref, frame) in self.frames.iter().rev() {
            if let Some(&var) = frame.0.get(name) {
                return Some((frame_ref, var));
            }
        }

        None
    }

    pub fn add_info(&mut self, info: TypeInfo, span: Option<Span>) -> TypeInfoRef {
        self.type_infos.insert(MaybePositioned::new(info, span))
    }

    pub fn add_unknown(&mut self, fallback: Option<TypeInfo>, span: Option<Span>) -> TypeInfoRef {
        let fallback = fallback.map(|fallback| self.add_info(fallback, span));

        self.type_infos.insert(MaybePositioned::new(TypeInfo::Unknown(fallback), span))
    }

    pub fn get_info(&self, info_ref: TypeInfoRef) -> TypeInfoRef {
        if let &TypeInfo::Ref(info_ref) = &self.type_infos[info_ref].value {
            self.get_info(info_ref)
        } else {
            info_ref
        }
    }

    pub fn unify(&mut self, a: TypeInfoRef, b: TypeInfoRef) {
        let a = self.get_info(a);
        let b = self.get_info(b);

        if a == b {
            return;
        }

        match (self.type_infos[a].value.clone(), self.type_infos[b].value.clone()) {
            (TypeInfo::Unknown(None), _)
            | (TypeInfo::Unknown(Some(_)), TypeInfo::Unknown(Some(_)))
            | (TypeInfo::Integer, TypeInfo::Primitive(PrimitiveType::Int(_) | PrimitiveType::UInt(_)) | TypeInfo::Integer) => {
                self.type_infos[a].value = TypeInfo::Ref(b);
            }
            (_, TypeInfo::Unknown(None)) | (TypeInfo::Primitive(PrimitiveType::Int(_) | PrimitiveType::UInt(_)), TypeInfo::Integer) => {
                self.type_infos[b].value = TypeInfo::Ref(a);
            }
            (TypeInfo::Unknown(Some(_)), _) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Unknown(Some(_))) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),
            (TypeInfo::Generic(_), _) => self.type_infos[a].value = TypeInfo::Ref(b),
            (_, TypeInfo::Generic(_)) => self.type_infos[b].value = TypeInfo::Ref(a),
            (TypeInfo::Array(a_element, a_size), TypeInfo::Array(b_element, b_size)) => {
                self.unify(a_element, b_element);

                match (a_size, b_size) {
                    (None, None) => (),
                    (None, Some(_)) => self.type_infos[b].value = TypeInfo::Array(b_element, None),
                    (Some(_), None) => self.type_infos[a].value = TypeInfo::Array(a_element, None),
                    (Some(a_size), Some(b_size)) => {
                        if a_size != b_size {
                            // if let Some(span) = self.type_infos[got].span {
                            //     self.errors.push(span.
                            // wrap(TypeUnificationError::ArraySizeMismatch(a_size,
                            // b_size))); }
                        }
                    }
                }
            }
            (TypeInfo::Func(a_args, a_returns), TypeInfo::Func(b_args, b_returns)) => {
                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a, b);
                }

                self.unify(a_returns, b_returns);
            }
            (TypeInfo::Adt(_, a_type_args), TypeInfo::Adt(_, b_type_args)) => {
                for (a, b) in a_type_args.into_iter().zip(b_type_args) {
                    self.unify(a, b);
                }
            }
            (TypeInfo::Trait(trait_ref, t_args), b_info) => {
                let b = self.solve(b);

                if let Some(vtable) = self.context.find_vtable(b, Some(trait_ref)) {
                    let type_args = if let TypeInfo::Adt(_, type_args) = b_info {
                        type_args
                    } else {
                        Box::default()
                    };

                    let generics: Box<[_]> = self.context.vtables[vtable]
                        .generics
                        .iter()
                        .map(|g| Self::type_to_info(&mut self.type_infos, self.context, *g, &type_args))
                        .collect();

                    for (arg, generic) in t_args.into_iter().zip(generics) {
                        self.unify(arg, generic);
                    }
                } else {
                    // panic!("unimplemented")
                }
            }
            (..) => (),
        }
    }

    pub fn solve(&mut self, info: TypeInfoRef) -> TypeRef {
        match &self.type_infos[info].value {
            &TypeInfo::Primitive(primitive_type) => self.context.types.get_or_add(Type::Primitive(primitive_type)),
            &TypeInfo::Array(element, size) => {
                let element = self.solve(element);

                self.context.types.get_or_add(Type::Array(element, size))
            }
            TypeInfo::Func(args, returns) => {
                let args = args.clone();
                let returns = self.solve(*returns);
                let args = args.into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Func(args, returns))
            }
            &TypeInfo::Unknown(Some(info)) | &TypeInfo::Ref(info) => self.solve(info),
            &TypeInfo::Unknown(None) | TypeInfo::Error => self.context.types.get_or_add(Type::Error),
            &TypeInfo::Integer => self.context.types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::I32))),
            TypeInfo::Adt(adt_ref, type_args) => {
                let adt_ref = *adt_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Adt(adt_ref, type_args))
            }
            TypeInfo::Trait(trait_ref, type_args) => {
                let trait_ref = *trait_ref;
                let type_args = type_args.clone().into_iter().map(|arg| self.solve(arg)).collect();

                self.context.types.get_or_add(Type::Trait(trait_ref, type_args))
            }
            &TypeInfo::Generic(i) => self.context.types.get_or_add(Type::Generic(i)),
        }
    }

    pub fn type_to_info(
        infos: &mut IndexVec<TypeInfoRef, MaybePositioned<TypeInfo>>,
        storage: &TypeContext,
        ty: TypeRef,
        type_args: &[TypeInfoRef],
    ) -> TypeInfoRef {
        match storage.types[ty].clone() {
            Type::Primitive(primitive_type) => infos.insert(MaybePositioned::new(TypeInfo::Primitive(primitive_type), None)),
            Type::Array(element, size) => {
                let element = Self::type_to_info(infos, storage, element, type_args);

                infos.insert(MaybePositioned::new(TypeInfo::Array(element, size), None))
            }
            Type::Adt(adt_ref, adt_type_args) => {
                let adt_type_args = adt_type_args
                    .into_iter()
                    .map(|type_arg| Self::type_to_info(infos, storage, type_arg, type_args))
                    .collect();

                infos.insert(MaybePositioned::new(TypeInfo::Adt(adt_ref, adt_type_args), None))
            }
            Type::Trait(trait_ref, trait_type_args) => {
                let trait_type_args = trait_type_args
                    .into_iter()
                    .map(|type_arg| Self::type_to_info(infos, storage, type_arg, type_args))
                    .collect();

                infos.insert(MaybePositioned::new(TypeInfo::Trait(trait_ref, trait_type_args), None))
            }
            Type::Func(args, returns) => {
                let args = args.into_iter().map(|arg| Self::type_to_info(infos, storage, arg, type_args)).collect();
                let returns = Self::type_to_info(infos, storage, returns, type_args);

                infos.insert(MaybePositioned::new(TypeInfo::Func(args, returns), None))
            }
            Type::Generic(i) => type_args
                .get(i)
                .copied()
                .unwrap_or_else(|| infos.insert(MaybePositioned::new(TypeInfo::Generic(i), None))),
            Type::Error => infos.insert(MaybePositioned::new(TypeInfo::Error, None)),
        }
    }

    pub fn instantiate_adt(&mut self, adt: AdtRef, variant: AdtVariantRef, type_args: &[TypeInfoRef]) -> impl Iterator<Item = (FieldRef, TypeInfoRef)> {
        self.context.adt_types[adt].variants[variant]
            .fields
            .iter()
            .map(|(field_ref, field)| (field_ref, Self::type_to_info(&mut self.type_infos, self.context, field.ty, type_args)))
    }
}
