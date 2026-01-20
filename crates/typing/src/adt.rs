use cranelift::{
    codegen::ir,
    module::{Module, ModuleResult},
    prelude::{FunctionBuilder, InstBuilder as _},
};
use mollie_const::ConstantValue;
use mollie_index::{Idx, IndexBoxedSlice};
use mollie_ir::{CodeGenerator, Field, MollieType};

use crate::{AdtVariantRef, CoreTypes, FieldRef, FieldType, IntType, PrimitiveType, TypeInfo, TypeInfoRef, TypeSolver, UIntType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AdtKind {
    Struct,
    Component,
    Enum,
}

#[derive(Debug)]
pub struct CompiledAdtVariant {
    pub fields: IndexBoxedSlice<FieldRef, (Field, TypeInfoRef)>,
}

impl CompiledAdt {
    pub fn instance_struct_like<M: Module, T: IntoIterator<Item = ir::Value>>(
        &self,
        codegen: &mut CodeGenerator<M>,
        fn_builder: &mut FunctionBuilder,
        values: T,
    ) -> ModuleResult<ir::Value> {
        self.instance(AdtVariantRef::ZERO, codegen, fn_builder, values)
    }

    pub fn instance<M: Module, T: IntoIterator<Item = ir::Value>>(
        &self,
        variant: AdtVariantRef,
        codegen: &mut CodeGenerator<M>,
        fn_builder: &mut FunctionBuilder,
        values: T,
    ) -> ModuleResult<ir::Value> {
        codegen.data_desc.define_zeroinit(self.size as usize);

        let id = codegen.module.declare_anonymous_data(true, false)?;

        codegen.module.define_data(id, &codegen.data_desc)?;
        codegen.data_desc.clear();

        let data_id = codegen.module.declare_data_in_func(id, fn_builder.func);
        let ptr = fn_builder.ins().global_value(codegen.module.isa().pointer_type(), data_id);

        let mut values = values.into_iter();

        for (field, _) in self.variants[variant].fields.values() {
            if let MollieType::Fat(ty, _) = field.ty
                && let Some(value) = values.next()
                && let Some(metadata) = values.next()
            {
                fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);
                fn_builder
                    .ins()
                    .store(ir::MemFlags::trusted(), metadata, ptr, field.offset + ty.bytes().cast_signed());
            } else if let Some(value) = values.next() {
                fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);
            }
        }

        Ok(ptr)
    }
}

#[derive(Debug)]
pub struct CompiledAdt {
    pub variants: IndexBoxedSlice<AdtVariantRef, CompiledAdtVariant>,
    pub size: u32,
}

impl CompiledAdt {
    pub fn main_variant(&self) -> &CompiledAdtVariant {
        &self.variants[AdtVariantRef::ZERO]
    }
}

#[derive(Debug)]
pub struct AdtVariant {
    pub name: Option<String>,
    pub discriminant: usize,
    pub fields: IndexBoxedSlice<FieldRef, (String, FieldType, Option<ConstantValue>)>,
}

#[derive(Debug)]
pub struct Adt {
    pub name: Option<String>,
    pub kind: AdtKind,
    pub generics: usize,
    pub variants: IndexBoxedSlice<AdtVariantRef, AdtVariant>,
}

impl Adt {
    fn apply_args(field_type: &FieldType, this: Option<TypeInfoRef>, core_types: &CoreTypes, solver: &mut TypeSolver, args: &[TypeInfoRef]) -> TypeInfoRef {
        match field_type {
            FieldType::This => this.unwrap(),
            FieldType::Unknown(_) => panic!("what"),
            &FieldType::Generic(generic, fallback) => args
                .get(generic)
                .copied()
                .or(fallback)
                .unwrap_or_else(|| solver.add_info(TypeInfo::Unknown(None))),
            FieldType::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => core_types.any,
                PrimitiveType::Int(IntType::ISize) => core_types.int_size,
                PrimitiveType::Int(IntType::I64) => core_types.int64,
                PrimitiveType::Int(IntType::I32) => core_types.int32,
                PrimitiveType::Int(IntType::I16) => core_types.int16,
                PrimitiveType::Int(IntType::I8) => core_types.int8,
                PrimitiveType::UInt(UIntType::USize) => core_types.uint_size,
                PrimitiveType::UInt(UIntType::U64) => core_types.uint64,
                PrimitiveType::UInt(UIntType::U32) => core_types.uint32,
                PrimitiveType::UInt(UIntType::U16) => core_types.uint16,
                PrimitiveType::UInt(UIntType::U8) => core_types.uint8,
                PrimitiveType::Float => core_types.float,
                PrimitiveType::Boolean => core_types.boolean,
                PrimitiveType::String => core_types.string,
                PrimitiveType::Component => core_types.component,
                PrimitiveType::Void => core_types.void,
                PrimitiveType::Null => unimplemented!(),
            },
            FieldType::Array(element, size) => {
                let element = Self::apply_args(element.as_ref(), this, core_types, solver, args);

                solver.add_info(TypeInfo::Array(element, *size))
            }
            FieldType::Func(params, output) => {
                let params = params
                    .iter()
                    .map(|param| param.as_ref().map(|param| Self::apply_args(param, this, core_types, solver, args)))
                    .collect();

                let output = Self::apply_args(output.as_ref(), this, core_types, solver, args);

                solver.add_info(TypeInfo::Func(params, output))
            }
            FieldType::Trait(trait_ref, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| Self::apply_args(field_type, this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Trait(*trait_ref, args))
            }
            FieldType::Adt(adt_ref, kind, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| Self::apply_args(field_type, this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Adt(*adt_ref, *kind, args))
            }
        }
    }

    pub fn instantiate(
        &self,
        variant: AdtVariantRef,
        this: Option<TypeInfoRef>,
        core_types: &CoreTypes,
        solver: &mut TypeSolver,
        args: &[TypeInfoRef],
    ) -> impl Iterator<Item = (FieldRef, &str, TypeInfoRef)> {
        self.variants[variant]
            .fields
            .iter()
            .map(move |(field, (name, field_type, _))| (field, name.as_str(), Self::apply_args(field_type, this, core_types, solver, args)))
    }
}
