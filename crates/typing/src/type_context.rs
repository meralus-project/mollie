use std::{
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Index, IndexMut},
};

use indexmap::IndexMap;
use mollie_index::{Idx, IndexVec};
use mollie_shared::{LangItem, Positioned, Span, pretty_fmt::FmtIteratorExt};

use crate::{
    Adt, AdtRef, AdtVariantRef, Arg, CoreTypes, FuncRef, IntType, ModuleId, PrimitiveType, TraitFuncRef, TraitRef, TypeErrorRef, TypeSolver, UIntType,
    VFuncRef, VTableRef,
    error::TypeError,
    ty::{Type, TypeRef},
};

#[derive(Debug, Default)]
pub struct TypeStorage {
    types: IndexVec<TypeRef, Type>,
    pub generics: Box<[TypeRef]>,
}

impl TypeStorage {
    pub fn is_likely_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        match (&self.types[ty], &self.types[other]) {
            (Type::Generic(..), _) | (_, Type::Generic(..)) => true,
            (Type::Primitive(primitive_type), Type::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (&Type::Array(element, size), &Type::Array(other_element, other_size)) => {
                self.is_likely_same(element, other_element)
                    && match (size, other_size) {
                        (None, None | Some(_)) => true,
                        (Some(_), None) => false,
                        (Some(size), Some(other_size)) => size == other_size,
                    }
            }
            (Type::Func(args, returns), Type::Func(other_args, other_returns)) => {
                args.len() == other_args.len()
                    && args
                        .iter()
                        .zip(other_args)
                        .all(|(&field_type, &other_field_type)| self.is_likely_same(field_type, other_field_type))
                    && self.is_likely_same(*returns, *other_returns)
            }
            (Type::Adt(adt, type_args), Type::Adt(other_adt, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args
                        .iter()
                        .zip(other_type_args)
                        .all(|(&field_type, &other_field_type)| self.is_likely_same(field_type, other_field_type))
                    && adt == other_adt
            }
            (Type::Trait(trait_ref, type_args), Type::Trait(other_trait_ref, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args
                        .iter()
                        .zip(other_type_args)
                        .all(|(&arg, &other_arg)| self.is_likely_same(arg, other_arg))
                    && trait_ref == other_trait_ref
            }
            _ => false,
        }
    }

    pub fn is_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        match (&self.types[ty], &self.types[other]) {
            (Type::Primitive(PrimitiveType::Any), _) | (_, Type::Primitive(PrimitiveType::Any)) => true,
            (Type::Generic(index), Type::Generic(other_index)) => index == other_index,
            (Type::Primitive(primitive_type), Type::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (&Type::Array(element, size), &Type::Array(other_element, other_size)) => self.is_same(element, other_element) && size == other_size,
            (Type::Func(args, returns), Type::Func(other_args, other_returns)) => {
                args.len() == other_args.len()
                    && args.iter().zip(other_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && self.is_same(*returns, *other_returns)
            }
            (Type::Adt(adt, type_args), Type::Adt(other_adt, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && adt == other_adt
            }
            (Type::Trait(trait_ref, type_args), Type::Trait(other_trait_ref, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && trait_ref == other_trait_ref
            }
            _ => false,
        }
    }

    pub fn get_or_add(&mut self, typo: Type) -> TypeRef {
        for (key, ty) in self.types.iter() {
            if ty == &typo {
                return key;
            }
        }

        self.types.insert(typo)
    }

    pub fn apply_type_args(&mut self, ty: TypeRef, type_args: &[TypeRef]) -> TypeRef {
        match self.types[ty].clone() {
            Type::Error | Type::Primitive(_) => ty,
            Type::Array(element, size) => {
                let element = self.apply_type_args(element, type_args);

                self.get_or_add(Type::Array(element, size))
            }
            Type::Adt(adt_ref, mut adt_type_args) => {
                for arg in &mut adt_type_args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                self.get_or_add(Type::Adt(adt_ref, adt_type_args))
            }
            Type::Trait(trait_ref, mut adt_type_args) => {
                for arg in &mut adt_type_args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                self.get_or_add(Type::Trait(trait_ref, adt_type_args))
            }
            Type::Func(mut args, returns) => {
                for arg in &mut args {
                    *arg = self.apply_type_args(*arg, type_args);
                }

                let returns = self.apply_type_args(returns, type_args);

                self.get_or_add(Type::Func(args, returns))
            }
            Type::Generic(i) => type_args.get(i).copied().unwrap_or_else(|| self.get_or_add(Type::Error)),
        }
    }

    pub fn hash_value_into<T: Hasher>(&self, state: &mut T, ty: &Type) {
        match ty {
            Type::Primitive(primitive_type) => primitive_type.hash(state),
            Type::Func(args, returns) => {
                "fn".hash(state);

                for &arg in args {
                    self.hash_into(state, arg);
                }

                self.hash_into(state, *returns);
            }
            Type::Trait(trait_ref, type_args) => {
                "trait".hash(state);

                trait_ref.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            Type::Adt(adt_ref, type_args) => {
                "adt".hash(state);

                adt_ref.hash(state);

                for &type_arg in type_args {
                    self.hash_into(state, type_arg);
                }
            }
            &Type::Array(element, size) => {
                "array".hash(state);

                self.hash_into(state, element);

                size.hash(state);
            }
            &Type::Generic(i) => match self.generics.get(i) {
                Some(&ty) => self.hash_into(state, ty),
                None => unimplemented!("can't hash <generic({i})>"),
            },
            Type::Error => unimplemented!("can't hash error"),
        }
    }

    pub fn hash_into<T: Hasher>(&self, state: &mut T, info: TypeRef) {
        self.hash_value_into(state, &self.types[info]);
    }

    pub fn hash_of(&self, info: TypeRef) -> u64 {
        let mut state = DefaultHasher::new();

        self.hash_into(&mut state, info);

        state.finish()
    }
}

impl Index<TypeRef> for TypeStorage {
    type Output = Type;

    fn index(&self, index: TypeRef) -> &Self::Output {
        &self.types[index]
    }
}

impl IndexMut<TypeRef> for TypeStorage {
    fn index_mut(&mut self, index: TypeRef) -> &mut Self::Output {
        &mut self.types[index]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicKind {
    SizeOf,
    AlignOf,
    SizeOfValue,
    AlignOfValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    SubModule(ModuleId),
    Adt(AdtRef),
    Trait(TraitRef),
    Func(FuncRef),
    Intrinsic(IntrinsicKind, TypeRef),
}

#[derive(Debug)]
pub struct Module {
    pub parent: Option<ModuleId>,
    pub name: String,
    pub id: ModuleId,
    pub items: IndexMap<String, ModuleItem>,
}

impl Module {
    pub fn new<T: Into<String>>(id: ModuleId, name: T) -> Self {
        Self {
            parent: None,
            name: name.into(),
            id,
            items: IndexMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LangItemValue {
    Adt(AdtRef),
    AdtVariant(AdtRef, AdtVariantRef),
    Trait(TraitRef),
    TraitFunc(TraitRef, TraitFuncRef),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub postfix: bool,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: TypeRef,
    // pub kind: VTableFuncKind,
}

#[derive(Debug)]
pub struct TraitFunc {
    pub name: String,
    pub args: Box<[Arg<TypeRef>]>,
    pub returns: TypeRef,
}

#[derive(Debug)]
pub struct Trait {
    pub name: String,
    pub generics: usize,
    pub functions: IndexVec<TraitFuncRef, TraitFunc>,
}

impl Trait {
    pub fn get_func_offset<T: AsRef<str>>(&self, name: T) -> usize {
        let mut offset = 0;
        let name = name.as_ref();

        for func in self.functions.values() {
            if func.name == name {
                break;
            }

            offset += size_of::<usize>();
        }

        offset
    }
}

// #[derive(Debug, Clone)]
// pub enum VTableFuncKind<S = ()> {
//     Local { ast: TypedAST<SolvedPass>, entry: BlockRef },
//     External(&'static str),
//     Special(S),
// }

#[derive(Debug, Clone)]
pub struct VTableFunc<T = TypeRef> {
    pub trait_func: Option<TraitFuncRef>,
    pub name: String,
    pub arg_names: Vec<String>,
    pub ty: T,
    // pub kind: VTableFuncKind,
}

#[derive(Debug)]
pub struct VTableGenerator {
    pub ty: TypeRef,
    pub origin_trait: Option<TraitRef>,
    pub generics: Box<[TypeRef]>,
    pub functions: IndexVec<VFuncRef, VTableFunc>,
}

#[derive(Debug)]
pub struct TypeContext {
    pub types: TypeStorage,
    pub core_types: CoreTypes<TypeRef>,

    pub language_items: HashMap<LangItem, LangItemValue>,

    pub modules: IndexVec<ModuleId, Module>,
    pub adt_types: IndexVec<AdtRef, Adt>,
    pub functions: IndexVec<FuncRef, Func>,
    pub traits: IndexVec<TraitRef, Trait>,
    pub vtables: IndexVec<VTableRef, VTableGenerator>,

    pub errors: IndexVec<TypeErrorRef, Positioned<TypeError>>,
}

pub struct TypeDisplay<'a> {
    ty: TypeRef,
    context: &'a TypeContext,
}

impl TypeDisplay<'_> {
    const fn with(&self, ty: TypeRef) -> Self {
        Self { ty, context: self.context }
    }
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.context.types[self.ty] {
            Type::Primitive(primitive_type) => primitive_type.fmt(f),
            &Type::Array(element, size) => match size {
                Some(size) => write!(f, "{}[{size}]", self.with(element)),
                None => write!(f, "{}[]", self.with(element)),
            },
            Type::Adt(adt_ref, type_args) => {
                if let Some(name) = &self.context.adt_types[*adt_ref].name {
                    name.fmt(f)?;
                }

                if type_args.is_empty() {
                    Ok(())
                } else {
                    write!(f, "<{}>", type_args.iter().map(|&ty| self.with(ty)).join(", "))
                }
            }
            Type::Trait(trait_ref, type_args) => {
                self.context.traits[*trait_ref].name.fmt(f)?;

                if type_args.is_empty() {
                    Ok(())
                } else {
                    write!(f, "<{}>", type_args.iter().map(|&ty| self.with(ty)).join(", "))
                }
            }
            Type::Func(arg_types, returns) => {
                write!(f, "fn({}) -> {}", arg_types.iter().map(|&ty| self.with(ty)).join(", "), self.with(*returns))
            }
            Type::Generic(i) => write!(f, "<generic({i})>"),
            Type::Error => f.write_str("<error>"),
        }
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContext {
    pub fn new() -> Self {
        let mut types = TypeStorage::default();

        let core_types = CoreTypes {
            void: types.get_or_add(Type::Primitive(PrimitiveType::Void)),
            any: types.get_or_add(Type::Primitive(PrimitiveType::Any)),
            bool: types.get_or_add(Type::Primitive(PrimitiveType::Bool)),
            i8: types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::I8))),
            i16: types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::I16))),
            i32: types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::I32))),
            i64: types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::I64))),
            isize: types.get_or_add(Type::Primitive(PrimitiveType::Int(IntType::ISize))),
            u8: types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::U8))),
            u16: types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::U16))),
            u32: types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::U32))),
            u64: types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::U64))),
            usize: types.get_or_add(Type::Primitive(PrimitiveType::UInt(UIntType::USize))),
            f32: types.get_or_add(Type::Primitive(PrimitiveType::F32)),
            string: types.get_or_add(Type::Primitive(PrimitiveType::String)),
        };

        Self {
            types,
            core_types,

            language_items: HashMap::new(),

            modules: IndexVec::from_iter([Module::new(ModuleId::ZERO, "<anonymous>")]),
            adt_types: IndexVec::new(),
            functions: IndexVec::new(),
            traits: IndexVec::new(),
            vtables: IndexVec::new(),

            errors: IndexVec::new(),
        }
    }

    pub fn is_same(&self, ty: TypeRef, other: TypeRef) -> bool {
        match (&self.types[ty], &self.types[other]) {
            (Type::Primitive(PrimitiveType::Any), _) | (_, Type::Primitive(PrimitiveType::Any)) => true,
            (Type::Generic(index), Type::Generic(other_index)) => index == other_index,
            (Type::Primitive(primitive_type), Type::Primitive(other_primitive_type)) => primitive_type == other_primitive_type,
            (&Type::Array(element, size), &Type::Array(other_element, other_size)) => self.is_same(element, other_element) && size == other_size,
            (Type::Func(args, returns), Type::Func(other_args, other_returns)) => {
                args.len() == other_args.len()
                    && args.iter().zip(other_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && self.is_same(*returns, *other_returns)
            }
            (Type::Adt(adt, type_args), Type::Adt(other_adt, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && adt == other_adt
            }
            (Type::Trait(trait_ref, type_args), Type::Trait(other_trait_ref, other_type_args)) => {
                type_args.len() == other_type_args.len()
                    && type_args.iter().zip(other_type_args).all(|(&arg, &other_arg)| self.is_same(arg, other_arg))
                    && trait_ref == other_trait_ref
            }
            (Type::Trait(trait_ref, _), _) => self.find_vtable(other, Some(*trait_ref)).is_some(),
            (_, Type::Trait(other_trait_ref, _)) => self.find_vtable(ty, Some(*other_trait_ref)).is_some(),
            _ => false,
        }
    }

    pub const fn display_of(&self, ty: TypeRef) -> TypeDisplay<'_> {
        TypeDisplay { ty, context: self }
    }

    pub fn error(&mut self, error: TypeError, span: Span) -> TypeErrorRef {
        self.errors.insert(span.wrap(error))
    }

    pub fn get_adt_item(&self, item: LangItem) -> Option<AdtRef> {
        self.language_items
            .get(&item)
            .and_then(|v| if let &LangItemValue::Adt(adt_ref) = v { Some(adt_ref) } else { None })
    }

    pub fn get_adt_variant_item(&self, item: LangItem) -> Option<(AdtRef, AdtVariantRef)> {
        self.language_items.get(&item).and_then(|v| {
            if let &LangItemValue::AdtVariant(adt_ref, adt_variant) = v {
                Some((adt_ref, adt_variant))
            } else {
                None
            }
        })
    }

    pub fn get_trait_item(&self, item: LangItem) -> Option<TraitRef> {
        self.language_items
            .get(&item)
            .and_then(|v| if let &LangItemValue::Trait(trait_ref) = v { Some(trait_ref) } else { None })
    }

    pub fn get_trait_func_item(&self, trait_ref: TraitRef, item: LangItem) -> Option<TraitFuncRef> {
        self.language_items.get(&item).and_then(|v| {
            if let &LangItemValue::TraitFunc(func_trait_ref, trait_func) = v
                && func_trait_ref == trait_ref
            {
                Some(trait_func)
            } else {
                None
            }
        })
    }

    pub fn find_vtable(&self, ty: TypeRef, origin_trait: Option<TraitRef>) -> Option<VTableRef> {
        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_likely_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
    }

    pub fn find_vtable_by_func<T: AsRef<str>>(&self, ty: TypeRef, name: T) -> Option<(VTableRef, VFuncRef)> {
        let name = name.as_ref();

        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_likely_same(ty, vtable.ty)
                && let Some(func) = vtable
                    .functions
                    .iter()
                    .find_map(|(func_ref, func)| if func.name == name { Some(func_ref) } else { None })
            {
                return Some((vtable_ref, func));
            }
        }

        None
    }

    pub fn get_vtable(&self, ty: TypeRef, origin_trait: Option<TraitRef>) -> Option<VTableRef> {
        for (vtable_ref, vtable) in self.vtables.iter() {
            if self.types.is_same(ty, vtable.ty) && vtable.origin_trait == origin_trait {
                return Some(vtable_ref);
            }
        }

        None
    }

    pub fn solver(&mut self) -> TypeSolver<'_> {
        TypeSolver::from_context(self)
    }

    pub fn register_module<T: Into<String>>(&mut self, name: T) -> ModuleId {
        self.register_module_in_module(ModuleId::ZERO, name)
    }

    pub fn register_module_in_module<T: Into<String>>(&mut self, parent: ModuleId, name: T) -> ModuleId {
        let name = name.into();
        let id = self.modules.next_index();
        let id = self.modules.insert(Module {
            parent: Some(parent),
            name: name.clone(),
            id,
            items: IndexMap::new(),
        });

        self.modules[parent].items.insert(name, ModuleItem::SubModule(id));
        self.modules[id].items.insert(String::from("super"), ModuleItem::SubModule(parent));

        id
    }

    pub fn register_adt(&mut self, ty: Adt) -> AdtRef {
        self.register_adt_in_module(ModuleId::ZERO, ty)
    }

    pub fn register_adt_in_module(&mut self, module: ModuleId, ty: Adt) -> AdtRef {
        let type_ref = self.adt_types.insert(ty);

        if let Some(name) = self.adt_types[type_ref].name.clone() {
            self.modules[module].items.insert(name, ModuleItem::Adt(type_ref));
        }

        type_ref
    }

    pub fn register_trait(&mut self, r#trait: Trait) -> TraitRef {
        self.register_trait_in_module(ModuleId::ZERO, r#trait)
    }

    pub fn register_trait_in_module(&mut self, module: ModuleId, r#trait: Trait) -> TraitRef {
        let trait_ref = self.traits.insert(r#trait);

        self.modules[module]
            .items
            .insert(self.traits[trait_ref].name.clone(), ModuleItem::Trait(trait_ref));

        trait_ref
    }

    pub fn register_func_in_module(&mut self, module: ModuleId, func: Func) -> FuncRef {
        let func = self.functions.insert(func);

        self.modules[module].items.insert(self.functions[func].name.clone(), ModuleItem::Func(func));

        func
    }

    pub fn inst_adt(&mut self, ty: AdtRef, type_args: &[TypeRef]) -> TypeRef {
        self.types.get_or_add(Type::Adt(ty, type_args.iter().copied().collect()))
    }

    // pub fn register_intrinsic_in_module<T: Into<String>>(&mut self, module:
    // ModuleId, name: T, kind: IntrinsicKind, ty: TypeInfo) {
    //     self.modules[module]
    //         .items
    //         .insert(name.into(), ModuleItem::Intrinsic(kind,
    // self.solver.add_info(ty, None))); }

    // pub fn instantiate_adt(&mut self, ty: AdtRef, generic_args: &[TypeInfoRef])
    // -> (TypeInfoRef, FieldType) {     let kind = self.adt_types[ty].kind;
    //     let generics = self.adt_types[ty].generics;
    //     let args = generic_args
    //         .iter()
    //         .copied()
    //         .map(Some)
    //         .chain(iter::repeat(None))
    //         .take(generics)
    //         .map(|arg| arg.unwrap_or_else(||
    // self.solver.add_info(TypeInfo::Unknown(None), None)))         .collect();

    //     let type_ref = self.solver.add_info(TypeInfo::Adt(ty, kind, args), None);

    //     (type_ref, FieldType::from_type_info_ref(type_ref, &self.solver))
    // }

    pub fn name_of_trait(&self, trait_ref: TraitRef) -> &str {
        self.traits[trait_ref].name.as_str()
    }

    pub fn find_trait<T: AsRef<str>>(&self, trait_name: T) -> Option<TraitRef> {
        let name = trait_name.as_ref();

        for (trait_ref, trait_data) in self.traits.iter() {
            if trait_data.name == name {
                return Some(trait_ref);
            }
        }

        None
    }

    pub const fn display_of_module(&self, module: ModuleId) -> ModuleDisplay<'_> {
        ModuleDisplay(module, &self.modules)
    }
}

pub struct ModuleDisplay<'a>(ModuleId, &'a IndexVec<ModuleId, Module>);

impl fmt::Display for ModuleDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.1[self.0].parent.map_or(Ok(()), |parent| {
            if parent == ModuleId::ZERO {
                write!(f, "{}", self.1[self.0].name)
            } else {
                write!(f, "{}::{}", Self(parent, self.1), self.1[self.0].name)
            }
        })
    }
}
