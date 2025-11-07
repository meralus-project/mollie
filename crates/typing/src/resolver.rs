use indexmap::{IndexMap, IndexSet};

use crate::{Type, TypeVariant};

#[derive(Debug, Default)]
pub struct TypeVTable {
    functions: IndexMap<String, Type>,
}

#[derive(Debug, Default)]
pub struct TypeResolver {
    name_to_type: IndexMap<String, TypeRef>,
    types: IndexSet<Type>,
    vtables: IndexMap<TypeRef, IndexMap<Option<usize>, TypeVTable>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef(usize, Option<usize>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VTableRef(TypeRef, Option<usize>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VFuncRef(VTableRef, usize);

impl VFuncRef {
    pub const fn vtable_ref(&self) -> VTableRef {
        self.0
    }
}

impl TypeResolver {
    pub fn add_named_type<T: Into<String>>(&mut self, name: T, ty: TypeVariant) -> TypeRef {
        let mut ty = self.add_type(ty);

        ty.1 = Some(self.name_to_type.insert_full(name.into(), ty).0);

        ty
    }

    pub fn add_named_full_type<T: Into<String>>(&mut self, name: T, ty: Type) -> TypeRef {
        let mut ty = self.add_full_type(ty);

        ty.1 = Some(self.name_to_type.insert_full(name.into(), ty).0);

        ty
    }

    pub fn add_type(&mut self, ty: TypeVariant) -> TypeRef {
        TypeRef(self.types.insert_full(ty.into()).0, None)
    }

    pub fn add_full_type(&mut self, ty: Type) -> TypeRef {
        TypeRef(self.types.insert_full(ty).0, None)
    }

    pub fn get_named_type<T: AsRef<str>>(&self, name: T) -> Option<&Type> {
        self.name_to_type.get(name.as_ref()).and_then(|ty| self.get_type(*ty))
    }

    pub fn get_type_name(&self, ty: TypeRef) -> Option<&str> {
        ty.1.and_then(|name| self.name_to_type.get_index(name)).map(|(name, _)| name.as_str())
    }

    pub fn get_type(&self, ty: TypeRef) -> Option<&Type> {
        self.types.get_index(ty.0)
    }

    pub fn create_vtable(&mut self, ty: TypeRef, trait_index: Option<usize>) {
        self.vtables.insert(ty, IndexMap::from_iter([(trait_index, TypeVTable::default())]));
    }

    pub fn create_vtable_func<T: Into<String>>(&mut self, vtable_ty: TypeRef, trait_index: Option<usize>, name: T, ty: Type) -> Option<VFuncRef> {
        if let Some(vtable) = self.vtables.get_mut(&vtable_ty).and_then(|vtables| vtables.get_mut(&trait_index)) {
            let func_idx = vtable.functions.insert_full(name.into(), ty).0;

            Some(VFuncRef(VTableRef(vtable_ty, trait_index), func_idx))
        } else {
            None
        }
    }

    pub fn get_vfunc_type(&self, func: VFuncRef) -> Option<&Type> {
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

#[cfg(test)]
mod tests {
    use crate::{TypeRef, TypeResolver, TypeVariant};

    #[test]
    fn test_basic_type_resolving() {
        #[allow(dead_code)]
        struct CoreTypes {
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

        let mut resolver = TypeResolver::default();

        let types = CoreTypes {
            void: resolver.add_named_type("void", TypeVariant::void()),
            any: resolver.add_named_type("any", TypeVariant::any()),
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
    }
}
