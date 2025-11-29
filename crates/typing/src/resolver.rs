use std::hash::{Hash, Hasher};

use indexmap::{IndexMap, IndexSet};
use mollie_shared::Span;
#[cfg(feature = "serde")] use serde::Serialize;

use crate::{PrimitiveType, Type as OldType, TypeVariant};

#[derive(Debug, Default)]
pub struct TypeVTable {
    functions: IndexMap<String, OldType>,
}

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub id: usize,
    pub ty: TypeInfoRef,
}

#[derive(Debug, Default)]
pub struct TypeStorage {
    name_to_type: IndexMap<String, TypeRef>,
    types: IndexSet<OldType>,
    vtables: IndexMap<TypeRef, IndexMap<Option<usize>, TypeVTable>>,
}

#[derive(Debug)]
pub struct CoreTypes {
    pub void: TypeInfoRef,
    pub any: TypeInfoRef,
    pub boolean: TypeInfoRef,
    pub int8: TypeInfoRef,
    pub int16: TypeInfoRef,
    pub int32: TypeInfoRef,
    pub int64: TypeInfoRef,
    pub int_size: TypeInfoRef,
    pub uint8: TypeInfoRef,
    pub uint16: TypeInfoRef,
    pub uint32: TypeInfoRef,
    pub uint64: TypeInfoRef,
    pub uint_size: TypeInfoRef,
    pub float: TypeInfoRef,
    pub component: TypeInfoRef,
    pub string: TypeInfoRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeRef(usize, Option<usize>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VTableRef(TypeRef, Option<usize>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VFuncRef(VTableRef, usize);

impl VFuncRef {
    pub const fn vtable_ref(&self) -> VTableRef {
        self.0
    }
}

impl TypeStorage {
    pub fn add_named_type<T: Into<String>>(&mut self, name: T, ty: TypeVariant) -> TypeRef {
        self.add_named_full_type(name, ty.into())
    }

    pub fn add_named_full_type<T: Into<String>>(&mut self, name: T, ty: OldType) -> TypeRef {
        let mut ty = self.add_full_type(ty);

        ty.1 = Some(self.name_to_type.len());

        self.name_to_type.insert_full(name.into(), ty);

        ty
    }

    pub fn add_type(&mut self, ty: TypeVariant) -> TypeRef {
        TypeRef(self.types.insert_full(ty.into()).0, None)
    }

    pub fn add_full_type(&mut self, ty: OldType) -> TypeRef {
        TypeRef(self.types.insert_full(ty).0, None)
    }

    pub fn remove_named_type<T: AsRef<str>>(&mut self, name: T) {
        self.name_to_type.swap_remove(name.as_ref());
    }

    pub fn ref_of_type(&self, ty: &OldType) -> Option<TypeRef> {
        self.types
            .get_index_of(ty)
            .map(|ty| TypeRef(ty, self.name_to_type.binary_search_by(|_, value| value.0.cmp(&ty)).ok()))
    }

    pub fn get_named_type_ref<T: AsRef<str>>(&self, name: T) -> Option<TypeRef> {
        self.name_to_type.get(name.as_ref()).copied()
    }

    pub fn get_named_type<T: AsRef<str>>(&self, name: T) -> Option<&OldType> {
        self.name_to_type.get(name.as_ref()).and_then(|ty| self.get_type(*ty))
    }

    pub fn get_type_name(&self, ty: TypeRef) -> Option<&str> {
        ty.1.and_then(|name| self.name_to_type.get_index(name)).map(|(name, _)| name.as_str())
    }

    pub fn get_type(&self, ty: TypeRef) -> Option<&OldType> {
        self.types.get_index(ty.0)
    }

    pub fn create_vtable(&mut self, ty: TypeRef, trait_index: Option<usize>) {
        self.vtables.insert(ty, IndexMap::from_iter([(trait_index, TypeVTable::default())]));
    }

    pub fn create_vtable_func<T: Into<String>>(&mut self, vtable_ty: TypeRef, trait_index: Option<usize>, name: T, ty: OldType) -> Option<VFuncRef> {
        if let Some(vtable) = self.vtables.get_mut(&vtable_ty).and_then(|vtables| vtables.get_mut(&trait_index)) {
            let func_idx = vtable.functions.insert_full(name.into(), ty).0;

            Some(VFuncRef(VTableRef(vtable_ty, trait_index), func_idx))
        } else {
            None
        }
    }

    pub fn get_vfunc_type(&self, func: VFuncRef) -> Option<&OldType> {
        self.vtables
            .get(&func.0.0)
            .and_then(|vtable| vtable.get(&func.0.1))
            .and_then(|vtable| vtable.functions.get_index(func.1))
            .map(|(_, func)| func)
    }

    pub fn find_vtable_func<T: AsRef<str>>(&self, ty: &TypeVariant, function_name: T) -> Option<VFuncRef> {
        let ty = if let TypeVariant::Ref { ty, .. } = ty { &ty.variant } else { ty };
        let function_name = function_name.as_ref();

        for (vtable_ty, vtable) in self.vtables.iter().rev() {
            if self.get_type(*vtable_ty).is_some_and(|vtable_ty| ty.same_as(&vtable_ty.variant, &[])) {
                for (trait_idx, vtable) in vtable.iter().rev() {
                    if let Some(func_idx) = vtable.functions.get_index_of(function_name) {
                        return Some(VFuncRef(VTableRef(*vtable_ty, *trait_idx), func_idx));
                    }
                }
            }
        }

        None
    }
}

#[derive(Debug)]
pub enum Type<'a> {
    Ref(TypeRef),
    Primitive(PrimitiveType),
    Func(Vec<Self>, Box<Self>),
    Struct(Option<&'a str>, Vec<Self>, Vec<(&'a str, Self)>),
    Array(Box<Self>, Option<usize>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Unknown(Option<TypeInfoRef>),
    Primitive(PrimitiveType),
    Ref(TypeInfoRef),
    Type(TypeRef),
    Func(Box<[TypeInfoRef]>, TypeInfoRef),
    Struct(TypeRef, Box<[TypeInfoRef]>),
    Array(TypeInfoRef, Option<usize>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeInfoRef(usize);

#[derive(Debug)]
pub enum TypeUnificationError {
    TypeMismatch(TypeInfoRef, TypeInfoRef),
    ArraySizeMismatch(usize, usize),
}

#[derive(Debug, Default)]
pub struct TypeSolver {
    name_to_info: IndexMap<String, TypeInfoRef>,
    type_infos: Vec<TypeInfo>,
    frames: Vec<IndexMap<String, Variable>>,
    associated_types: IndexMap<u64, TypeInfoRef>,
    pub errors: Vec<TypeUnificationError>,
}

impl TypeSolver {
    pub fn push_frame(&mut self) {
        self.frames.push(IndexMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn get_type_of<T: Hash>(&self, value: &T, span: Span) -> Option<TypeInfoRef> {
        let mut hasher = std::hash::DefaultHasher::default();

        value.hash(&mut hasher);
        span.hash(&mut hasher);

        self.associated_types.get(&hasher.finish()).copied()
    }

    pub fn associate_type<T: Hash>(&mut self, value: &T, span: Span, ty: TypeInfoRef) {
        let mut hasher = std::hash::DefaultHasher::default();

        value.hash(&mut hasher);
        span.hash(&mut hasher);

        self.associated_types.insert(hasher.finish(), ty);
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

    pub fn get_named_info<T: AsRef<str>>(&self, name: T) -> Option<TypeInfoRef> {
        self.name_to_info.get(name.as_ref()).copied()
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
        &self.type_infos[info_ref.0]
    }

    pub fn unify(&mut self, a: TypeInfoRef, b: TypeInfoRef) {
        match (self.type_infos[a.0].clone(), self.type_infos.get(b.0).cloned()) {
            (_, None) => (),
            (TypeInfo::Unknown(_), _) => self.type_infos[a.0] = TypeInfo::Ref(b),
            (_, Some(TypeInfo::Unknown(_))) => self.type_infos[b.0] = TypeInfo::Ref(a),
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, Some(TypeInfo::Ref(b))) => self.unify(a, b),
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
                    (None, Some(_)) => self.type_infos[b.0] = TypeInfo::Array(b, None),
                    (Some(_), None) => self.type_infos[a.0] = TypeInfo::Array(a, None),
                    (Some(a_size), Some(b_size)) => {
                        if a_size != b_size {
                            self.errors.push(TypeUnificationError::ArraySizeMismatch(a_size, b_size));
                        }
                    }
                }
            }
            (TypeInfo::Struct(a_ty, a_args), Some(TypeInfo::Struct(b_ty, b_args))) => {
                if a_ty != b_ty {
                    self.errors.push(TypeUnificationError::TypeMismatch(a, b));
                }

                for (a, b) in a_args.into_iter().zip(b_args) {
                    self.unify(a, b);
                }
            }
            (a_info, Some(b_info)) => {
                if a_info != b_info {
                    self.errors.push(TypeUnificationError::TypeMismatch(a, b));
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

    fn format_generic_type(&self, ty: Type, storage: &TypeStorage) -> String {
        match ty {
            Type::Ref(type_ref) => storage.get_type(type_ref).unwrap().to_string(),
            Type::Primitive(primitive) => primitive.to_string(),
            Type::Func(args, output) => {
                let mut result = "fn(".to_string();
                let mut first = true;

                for ty in args {
                    if first {
                        first = false;
                    } else {
                        result += ", ";
                    }

                    result += self.format_generic_type(ty, storage).as_str();
                }

                result += ") -> ";
                result += self.format_generic_type(*output, storage).as_str();

                result
            }
            Type::Struct(ty, args, fields) => {
                let mut result = ty.map(ToString::to_string).unwrap_or_default();

                if !args.is_empty() {
                    let mut first = true;

                    result += "<";

                    for arg in args {
                        if first {
                            first = false;
                        } else {
                            result += ", ";
                        }

                        result += self.format_generic_type(arg, storage).as_str();
                    }

                    result += ">";
                }

                result
            }
            Type::Array(typee, size) => {
                format!(
                    "{}[{}]",
                    self.format_generic_type(*typee, storage),
                    size.as_ref().map(ToString::to_string).unwrap_or_default()
                )
            }
        }
    }

    pub fn format_type(&self, ty: Type, storage: &TypeStorage) -> String {
        match ty {
            Type::Ref(type_ref) => storage.get_type(type_ref).unwrap().to_string(),
            Type::Primitive(primitive) => primitive.to_string(),
            Type::Func(args, output) => {
                let mut result = "fn(".to_string();
                let mut first = true;

                for ty in args {
                    if first {
                        first = false;
                    } else {
                        result += ", ";
                    }

                    result += self.format_type(ty, storage).as_str();
                }

                result += ") -> ";
                result += self.format_type(*output, storage).as_str();

                result
            }
            Type::Struct(ty, args, fields) => {
                let mut result = ty.map(ToString::to_string).unwrap_or_default();
                let mut first = true;

                if ty.is_none() && args.is_empty() {
                    result += "{ ";
                } else {
                    if !args.is_empty() {
                        result += "<";

                        for arg in args {
                            if first {
                                first = false;
                            } else {
                                result += ", ";
                            }

                            result += self.format_generic_type(arg, storage).as_str();
                        }

                        result += ">";
                        first = true;
                    }

                    result += " { ";
                }

                for (name, ty) in fields {
                    if first {
                        first = false;
                    } else {
                        result += ", ";
                    }

                    result += name;
                    result += ": ";
                    result += self.format_type(ty, storage).as_str();
                }

                result += " }";

                result
            }
            Type::Array(typee, size) => {
                format!(
                    "{}[{}]",
                    self.format_type(*typee, storage),
                    size.as_ref().map(ToString::to_string).unwrap_or_default()
                )
            }
        }
    }

    pub fn contains_unknown(&self, ty: TypeInfoRef) -> bool {
        match &self.type_infos[ty.0] {
            TypeInfo::Unknown(fallback) => fallback.is_none(),
            &TypeInfo::Ref(ty) => self.contains_unknown(ty),
            TypeInfo::Func(args, output) => args.iter().any(|&arg| self.contains_unknown(arg)) || self.contains_unknown(*output),
            TypeInfo::Struct(_, args) => args.iter().any(|&arg| self.contains_unknown(arg)),
            &TypeInfo::Array(element, _) => self.contains_unknown(element),
            _ => false,
        }
    }

    pub fn get_as_type(&self, var: TypeInfoRef, storage: &TypeStorage) -> Option<OldType> {
        match self.type_infos[var.0].clone() {
            TypeInfo::Unknown(fallback) => fallback.and_then(|ty| self.get_as_type(ty, storage)),
            TypeInfo::Ref(var) => self.get_as_type(var, storage),
            TypeInfo::Type(ty) => storage.get_type(ty).cloned(),
            TypeInfo::Func(args, output) => {
                let args = args.into_iter().map(|arg| self.get_as_type(arg, storage)).collect::<Option<Vec<_>>>()?;
                let returns = Box::new(self.get_as_type(output, storage)?);

                Some(
                    TypeVariant::complex(crate::ComplexType::Function(crate::FunctionType {
                        is_native: false,
                        this: None,
                        args,
                        returns,
                    }))
                    .into(),
                )
            }
            TypeInfo::Struct(..) => todo!(),
            TypeInfo::Array(..) => todo!(),
            TypeInfo::Primitive(_) => todo!(),
        }
    }

    pub fn get_actual_type<'a>(&self, var: TypeInfoRef, storage: &'a TypeStorage) -> Option<Type<'a>> {
        match &self.type_infos[var.0] {
            TypeInfo::Unknown(fallback) => fallback.and_then(|ty| self.get_actual_type(ty, storage)),
            &TypeInfo::Ref(var) => self.get_actual_type(var, storage),
            &TypeInfo::Type(ty) => Some(Type::Ref(ty)),
            &TypeInfo::Primitive(primitive) => Some(Type::Primitive(primitive)),
            TypeInfo::Func(args, output) => Some(Type::Func(
                args.into_iter().map(|arg| self.get_actual_type(*arg, storage)).collect::<Option<Vec<_>>>()?,
                Box::new(self.get_actual_type(*output, storage)?),
            )),
            TypeInfo::Struct(ty, args) => storage.get_type(*ty).and_then(|ty| ty.variant.as_struct()).and_then(|structure| {
                Some(Type::Struct(
                    storage.get_type_name(*ty),
                    args.iter().map(|&arg| self.get_actual_type(arg, storage)).collect::<Option<_>>()?,
                    structure
                        .properties
                        .iter()
                        .map(|(name, ty)| {
                            match &ty.variant {
                                &TypeVariant::Primitive(primitive_type) => Some(Type::Primitive(primitive_type)),
                                TypeVariant::Complex(complex_type) => todo!(),
                                TypeVariant::Trait(_) => todo!(),
                                &TypeVariant::Generic(idx) => args.get(idx).and_then(|&arg| self.get_actual_type(arg, storage)),
                                TypeVariant::Ref { ty, mutable } => todo!(),
                                TypeVariant::This => todo!(),
                                TypeVariant::Unknown => todo!(),
                            }
                            .map(|ty| (name.as_str(), ty))
                        })
                        .collect::<Option<_>>()?,
                ))
            }),
            &TypeInfo::Array(element, size) => Some(Type::Array(Box::new(self.get_actual_type(element, storage)?), size)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        TypeVariant,
        resolver::{TypeRef, TypeStorage},
    };

    #[test]
    fn test_basic_type_resolving() {
        #[allow(dead_code)]
        struct CoreTypes {
            unknown: TypeRef,
            void: TypeRef,
            any: TypeRef,
            boolean: TypeRef,
            int8: TypeRef,
            int16: TypeRef,
            int32: TypeRef,
            int64: TypeRef,
            int_size: TypeRef,
            uint8: TypeRef,
            uint16: TypeRef,
            uint32: TypeRef,
            uint64: TypeRef,
            uint_size: TypeRef,
            float: TypeRef,
        }

        let mut resolver = TypeStorage::default();

        let types = CoreTypes {
            unknown: resolver.add_type(TypeVariant::Unknown),
            void: resolver.add_type(TypeVariant::void()),
            any: resolver.add_type(TypeVariant::any()),
            boolean: resolver.add_named_type("boolean", TypeVariant::boolean()),
            int8: resolver.add_named_type("int8", TypeVariant::int8()),
            int16: resolver.add_named_type("int16", TypeVariant::int16()),
            int32: resolver.add_named_type("int32", TypeVariant::int32()),
            int64: resolver.add_named_type("int64", TypeVariant::int64()),
            int_size: resolver.add_named_type("int_size", TypeVariant::isize()),
            uint8: resolver.add_named_type("uint8", TypeVariant::uint8()),
            uint16: resolver.add_named_type("uint16", TypeVariant::uint16()),
            uint32: resolver.add_named_type("uint32", TypeVariant::uint32()),
            uint64: resolver.add_named_type("uint64", TypeVariant::uint64()),
            uint_size: resolver.add_named_type("uint_size", TypeVariant::usize()),
            float: resolver.add_named_type("float", TypeVariant::float()),
        };

        resolver.create_vtable(types.any, None);
        resolver.create_vtable(types.boolean, None);

        let lmao = resolver.create_vtable_func(
            types.any,
            None,
            "lmao",
            TypeVariant::function_with_self(TypeVariant::any(), [], TypeVariant::void()).into(),
        );

        let test = resolver.create_vtable_func(
            types.boolean,
            None,
            "test",
            TypeVariant::function_with_self(TypeVariant::boolean(), [], TypeVariant::void()).into(),
        );

        let test2 = resolver.create_vtable_func(
            types.boolean,
            None,
            "test2",
            TypeVariant::function_with_self(TypeVariant::boolean(), [], TypeVariant::void()).into(),
        );

        assert_eq!(resolver.find_vtable_func(&TypeVariant::boolean(), "test"), test); // bool::test
        assert_eq!(resolver.find_vtable_func(&TypeVariant::boolean(), "test2"), test2); // bool::test2
        assert_eq!(resolver.find_vtable_func(&TypeVariant::boolean(), "lmao"), lmao); // <any>::lmao
        assert_eq!(resolver.find_vtable_func(&TypeVariant::borrow(TypeVariant::boolean()), "test"), test); // &bool => bool::test

        assert_eq!(resolver.get_type_name(types.boolean), Some("boolean"));

        // REAL type resolving for next program:
        // let hi = 20; // `i32`
        //
        // println(hi); // `println(i64)`, so `hi` should become `i64`
        #[derive(Debug, Clone, Copy)]
        enum TypeInfo {
            Unknown(Option<TypeRef>),
            Ref(usize),
            Type(TypeRef),
        }

        fn unify(vars: &mut [TypeInfo], a: usize, b: usize) {
            match (vars[a], vars.get(b).copied()) {
                (_, None) => (),
                (TypeInfo::Unknown(_), _) => vars[0] = TypeInfo::Ref(1),
                (_, Some(TypeInfo::Unknown(_))) => vars[1] = TypeInfo::Ref(0),
                (TypeInfo::Ref(a), _) => unify(vars, a, b),
                (_, Some(TypeInfo::Ref(b))) => unify(vars, a, b),
                (TypeInfo::Type(a), Some(TypeInfo::Type(b))) => assert_eq!(a, b),
            }
        }

        fn get_actual_type(vars: &[TypeInfo], var: usize) -> TypeRef {
            match vars[var] {
                TypeInfo::Unknown(None) => panic!("{var} have <unknown> type"),
                TypeInfo::Unknown(Some(fallback)) => fallback,
                TypeInfo::Ref(var) => get_actual_type(vars, var),
                TypeInfo::Type(ty) => ty,
            }
        }

        let mut vars = [TypeInfo::Unknown(Some(types.int32))];

        unify(&mut vars, 0, 1);

        assert_eq!(get_actual_type(&vars, 0), types.int32);

        let mut vars = [TypeInfo::Unknown(Some(types.int32)), TypeInfo::Type(types.int64)];

        unify(&mut vars, 0, 1);

        assert_eq!(get_actual_type(&vars, 0), types.int64);
    }
}
