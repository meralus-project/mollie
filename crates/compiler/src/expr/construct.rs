use std::iter;

use cranelift::{
    codegen::ir,
    frontend::FuncInstBuilder,
    module::Module,
    prelude::{InstBuilder, isa::TargetIsa},
};
use itertools::Itertools;
use mollie_index::Idx;
use mollie_ir::{Array, ConstValue, ConstantCompiler, MollieType, compile_constant};
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{AdtKind, AdtVariantRef, FieldRef, Type, TypeRef};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<'a, S, M: Module> ConstantCompiler<'a> for FunctionCompiler<'a, S, M> {
    fn construct(&mut self, ty: usize, variant: usize, values: Box<[Option<ConstValue>]>) -> Option<ir::Value> {
        let ty = TypeRef::new(ty);
        let hash = self.hash_of(ty);
        let Type::Adt(adt_ref, _) = self.type_context.type_context.types[ty] else {
            unreachable!()
        };

        let adt_kind = self.type_context.type_context.adt_types[adt_ref].kind;
        let is_enum = matches!(adt_kind, AdtKind::Enum);
        let variant = AdtVariantRef::new(variant);

        let values = if is_enum {
            let i = self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index().cast_signed() as i64);

            iter::once(i)
                .chain(values.into_iter().enumerate().flat_map(|(field, value)| {
                    value.unwrap_or_else(|| {
                        let (field, field_type) = &self.compiler.get_adt_variant(hash, variant).fields[FieldRef::new(field)];
                        let field_type = field_type.index();

                        field.default_value.clone().map_or_else(
                            || unimplemented!("can't compile: no value"),
                            |value| compile_constant(field_type, value, self).unwrap_or_else(|| unimplemented!("can't compile: no value")),
                        )
                    })
                }))
                .collect::<Box<[_]>>()
        } else {
            values
                .into_iter()
                .enumerate()
                .flat_map(|(field, value)| {
                    value.unwrap_or_else(|| {
                        let (field, field_type) = &self.compiler.get_adt_variant(hash, variant).fields[FieldRef::new(field)];
                        let field_type = field_type.index();

                        field.default_value.clone().map_or_else(
                            || unimplemented!("can't compile: no value"),
                            |value| compile_constant(field_type, value, self).unwrap_or_else(|| unimplemented!("can't compile: no value")),
                        )
                    })
                })
                .collect::<Box<[_]>>()
        };

        Some(self.construct(ty, variant, values.as_ref()).unwrap())
    }

    fn get_array_element(&mut self, ty: usize) -> MollieType {
        let ty = TypeRef::new(ty);
        let element = if let Type::Array(element, _) = self.type_context.type_context.types[ty] {
            element
        } else {
            ty
        };

        element.as_ir_type(&self.type_context.type_context.types, self.compiler.isa())
    }

    fn alloc_string(&mut self, value: String) -> Option<ConstValue> {
        let data_id = self.compiler.codegen.static_data(value.as_bytes()).ok()?;
        let data_id = self.compiler.codegen.module.declare_data_in_func(data_id, self.fn_builder.func);
        let ptr_type = self.compiler.ptr_type();
        let ptr = self.fn_builder.ins().global_value(ptr_type, data_id);
        let size = self.fn_builder.ins().iconst(ptr_type, value.len().cast_signed() as i64);

        Some(ConstValue::FatPtr(ptr, size))
    }

    fn alloc_array(&mut self, element: usize, element_type: MollieType, elements: Box<[ConstValue]>) -> Option<ConstValue> {
        let size = self.fn_builder.ins().iconst(self.compiler.ptr_type(), elements.len().cast_signed() as i64);
        let mut values = Vec::with_capacity(elements.len());

        for element in elements {
            match (element_type, element) {
                (MollieType::Regular(_), ConstValue::Value(value)) => values.push(value),
                (MollieType::Fat(..), ConstValue::FatPtr(value, metadata)) => {
                    values.push(value);
                    values.push(metadata);
                }
                _ => (),
            }
        }

        let element = if let Type::Array(element, _) = self.type_context.type_context.types[TypeRef::new(element)] {
            element
        } else {
            TypeRef::new(element)
        };

        let type_layout_ptr = &raw const *self.type_layout_of(element);
        let ptr_type = self.compiler.ptr_type();
        let type_layout_ptr = self.fn_builder.ins().iconst(ptr_type, type_layout_ptr as i64);
        let ptr = self.fn_builder.ins().call(self.context.alloc_array, &[type_layout_ptr, size]);
        let ptr = self.fn_builder.inst_results(ptr)[0];
        let array_ptr = self
            .fn_builder
            .ins()
            .load(ptr_type, ir::MemFlags::trusted(), ptr, size_of::<usize>().cast_signed() as i32 * 2);

        let arr = Array { element: element_type };

        match element_type {
            MollieType::Regular(_) => {
                for (index, value) in values.into_iter().enumerate() {
                    self.fn_builder.ins().store(ir::MemFlags::trusted(), value, array_ptr, arr.get_offset_of(index));
                }
            }
            MollieType::Fat(ty, _) => {
                for (index, (value, metadata)) in values.into_iter().tuples::<(ir::Value, ir::Value)>().enumerate() {
                    self.fn_builder.ins().store(ir::MemFlags::trusted(), value, array_ptr, arr.get_offset_of(index));
                    self.fn_builder.ins().store(
                        ir::MemFlags::trusted(),
                        metadata,
                        array_ptr,
                        arr.get_offset_of(index) + ty.bytes().cast_signed(),
                    );
                }
            }
        }

        Some(ConstValue::Value(ptr))
    }

    fn isa(&self) -> &dyn TargetIsa {
        self.compiler.isa()
    }

    fn ins<'short>(&'short mut self) -> FuncInstBuilder<'short, 'a> {
        self.fn_builder.ins()
    }
}

impl<S, M: Module> FunctionCompiler<'_, S, M> {
    pub fn compile_construct(
        &mut self,
        ast: &TypedAST,
        ty: TypeRef,
        variant: AdtVariantRef,
        fields: &[(FieldRef, TypeRef, ExprRef)],
    ) -> CompileResult<MolValue> {
        let mut values = Vec::new();
        let hash = self.hash_of(ty);
        let Type::Adt(adt_ref, _) = self.type_context.type_context.types[ty] else {
            unreachable!()
        };

        let adt_kind = self.type_context.type_context.adt_types[adt_ref].kind;

        let is_enum = matches!(adt_kind, AdtKind::Enum);

        if is_enum {
            values.push(self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index().cast_signed() as i64));
        }

        for (field_ref, (field, field_type)) in self
            .compiler
            .try_get_adt_variant(hash, variant)
            .unwrap_or_else(|| panic!("can't construct {}: it wasn't compiled", self.type_context.type_context.display_of(ty)))
            .fields
            .clone()
            .into_iter()
        {
            if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                if is_enum && field_ref.index() == 0 {
                    continue;
                }

                let (got_ty, got) = match property.2 {
                    ExprRef::INVALID => match &field.default_value {
                        Some(value) => (
                            None,
                            compile_constant(field_type.index(), value.clone(), self).map_or(MolValue::Nothing, |v| match v {
                                ConstValue::Value(value) => MolValue::Value(value),
                                ConstValue::FatPtr(value, metadata) => MolValue::FatPtr(value, metadata),
                            }),
                        ),
                        None => unimplemented!(
                            "can't compile {}: {field:?} => no value",
                            self.type_context.type_context.adt_types[adt_ref].variants[variant].fields[field_ref].name
                        ),
                    },
                    expr => (Some(ast[expr].ty), expr.compile(ast, self)?),
                };

                match (field.ty, &got) {
                    (MollieType::Regular(ty), &MolValue::Value(v)) => {
                        debug_assert_eq!(
                            ty,
                            self.fn_builder.func.dfg.value_type(v),
                            "got incorrect type for {}::{}",
                            self.type_context.type_context.adt_types[adt_ref].name.as_deref().unwrap_or_default(),
                            self.type_context.type_context.display_of(property.1)
                        );

                        values.push(v);
                    }
                    (MollieType::Fat(ty, meta), got) => match *got {
                        MolValue::Value(v) => match (&self.type_context.type_context.types[field_type], got_ty) {
                            (&Type::Trait(t, ..), Some(got_ty)) => {
                                let hash = self.type_context.type_context.types.hash_of(got_ty);
                                let data_id = self
                                    .compiler
                                    .codegen
                                    .module
                                    .declare_data_in_func(self.compiler.trait_to_vtable[&(hash, Some(t))], self.fn_builder.func);

                                let metadata = self.fn_builder.ins().global_value(self.compiler.ptr_type(), data_id);

                                values.push(v);
                                values.push(metadata);
                            }
                            (_, Some(got_ty)) => {
                                debug_assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                let hash = self.type_context.type_context.types.hash_of(got_ty);
                                let metadata = self.fn_builder.ins().iconst(meta, hash.cast_signed());

                                values.push(v);
                                values.push(metadata);
                            }
                            _ => (),
                        },
                        MolValue::FatPtr(v, m) => {
                            debug_assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                            debug_assert_eq!(meta, self.fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                            values.push(v);
                            values.push(m);
                        }
                        _ => (),
                    },
                    (ty, got) => println!("warning: expected {ty:?}, got {got:?}"),
                }
            }
        }

        Ok(MolValue::Value(self.construct(ty, variant, &values)?))
    }
}
