use cranelift::{
    codegen::ir,
    prelude::{FunctionBuilder, InstBuilder as _, isa::TargetIsa},
};
use mollie_const::ConstantValue;
use mollie_index::IndexBoxedSlice;
use mollie_ir::{Field, stack_alloc};

use crate::{ComplexTypeVariantRef, CoreTypes, FieldRef, FieldType, PrimitiveType, TypeInfo, TypeInfoRef, TypeSolver};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComplexTypeKind {
    Struct,
    Component,
    Enum,
}

#[derive(Debug)]
pub struct CompiledComplexTypeVariant {
    pub fields: IndexBoxedSlice<FieldRef, (Field, TypeInfoRef)>,
}

impl CompiledComplexType {
    pub fn instance<T: IntoIterator<Item = ir::Value>>(
        &self,
        variant: ComplexTypeVariantRef,
        isa: &dyn TargetIsa,
        fn_builder: &mut FunctionBuilder,
        values: T,
    ) -> ir::Value {
        let slot = stack_alloc(fn_builder, self.size);

        for ((field, _), value) in self.variants[variant].fields.values().zip(values) {
            fn_builder.ins().stack_store(value, slot, field.offset);
        }

        fn_builder.ins().stack_addr(isa.pointer_type(), slot, 0)
    }
}

#[derive(Debug)]
pub struct CompiledComplexType {
    pub variants: IndexBoxedSlice<ComplexTypeVariantRef, CompiledComplexTypeVariant>,
    pub size: u32,
}

#[derive(Debug)]
pub struct ComplexTypeVariant {
    pub name: Option<String>,
    pub discriminant: usize,
    pub fields: IndexBoxedSlice<FieldRef, (String, FieldType, Option<ConstantValue>)>,
}

#[derive(Debug)]
pub struct ComplexType {
    pub name: Option<String>,
    pub kind: ComplexTypeKind,
    pub generics: usize,
    pub variants: IndexBoxedSlice<ComplexTypeVariantRef, ComplexTypeVariant>,
}

impl ComplexType {
    fn apply_args(field_type: &FieldType, this: Option<TypeInfoRef>, core_types: &CoreTypes, solver: &mut TypeSolver, args: &[TypeInfoRef]) -> TypeInfoRef {
        match field_type {
            FieldType::This => this.unwrap(),
            FieldType::Unknown(_) => panic!("what"),
            &FieldType::Generic(generic) => args.get(generic).copied().unwrap_or_else(|| solver.add_info(TypeInfo::Unknown(None))),
            FieldType::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => core_types.any,
                PrimitiveType::ISize => core_types.int_size,
                PrimitiveType::I64 => core_types.int64,
                PrimitiveType::I32 => core_types.int32,
                PrimitiveType::I16 => core_types.int16,
                PrimitiveType::I8 => core_types.int8,
                PrimitiveType::USize => core_types.uint_size,
                PrimitiveType::U64 => core_types.uint64,
                PrimitiveType::U32 => core_types.uint32,
                PrimitiveType::U16 => core_types.uint16,
                PrimitiveType::U8 => core_types.uint8,
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
                let params = params.iter().map(|param| Self::apply_args(param, this, core_types, solver, args)).collect();
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
            FieldType::Complex(complex_type, kind, field_types) => {
                let args = field_types
                    .iter()
                    .map(|field_type| Self::apply_args(field_type, this, core_types, solver, args))
                    .collect();

                solver.add_info(TypeInfo::Complex(*complex_type, *kind, args))
            }
        }
    }

    pub fn instantiate(
        &self,
        variant: ComplexTypeVariantRef,
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
