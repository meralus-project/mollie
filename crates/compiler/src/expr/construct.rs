use cranelift::{
    codegen::ir,
    frontend::FuncInstBuilder,
    module::Module,
    prelude::{InstBuilder, isa::TargetIsa},
};
use itertools::Itertools;
use mollie_index::Idx;
use mollie_ir::{Array, ConstValue, ConstantCompiler, MollieType, compile_constant};
use mollie_shared::Span;
use mollie_typed_ast::{ExprRef, TypedAST};
use mollie_typing::{AdtKind, AdtVariantRef, FieldRef, TypeInfo, TypeInfoRef};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

impl<'a, S, M: Module> ConstantCompiler<'a> for FunctionCompiler<'a, S, M> {
    fn construct(&mut self, ty: usize, variant: usize, values: Box<[Option<ConstValue>]>) -> Option<ir::Value> {
        let ty = TypeInfoRef::new(ty);
        let hash = self.hash_of(ty);
        let variant = AdtVariantRef::new(variant);

        let values = values
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
            .collect::<Box<[_]>>();

        self.construct(ty, variant, values.as_ref()).ok()
    }

    fn get_array_element(&mut self, ty: usize) -> MollieType {
        let ty = TypeInfoRef::new(ty);
        let element = if let &TypeInfo::Array(element, _) = self.checker.solver.get_info(ty) {
            element
        } else {
            ty
        };

        element.as_ir_type(&self.checker.solver, self.compiler.isa())
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

        let element = if let &TypeInfo::Array(element, _) = self.checker.solver.get_info(TypeInfoRef::new(element)) {
            element
        } else {
            TypeInfoRef::new(element)
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
        ty: TypeInfoRef,
        variant: AdtVariantRef,
        fields: &[(FieldRef, String, Option<(ExprRef, Span)>)],
    ) -> CompileResult<MolValue> {
        let mut values = Vec::new();
        let hash = self.hash_of(ty);
        let &TypeInfo::Adt(adt_ref, adt_kind, ..) = self.checker.solver.get_info(ty) else {
            unreachable!()
        };

        let is_enum = matches!(adt_kind, AdtKind::Enum);

        if is_enum {
            values.push(self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index().cast_signed() as i64));
        }

        for (field_ref, (field, field_type)) in self
            .compiler
            .try_get_adt_variant(hash, variant)
            .unwrap_or_else(|| panic!("can't construct {}: it wasn't compiled", self.checker.short_display_of_type(ty, None)))
            .fields
            .clone()
            .into_iter()
        {
            if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                if is_enum && field_ref.index() == 0 {
                    continue;
                }

                let (got_ty, got) = match property.2 {
                    Some(expr) => (Some(ast[expr.0].ty), expr.0.compile(ast, self)?),
                    None => match &field.default_value {
                        Some(value) => (
                            None,
                            compile_constant(field_type.index(), value.clone(), self).map_or(MolValue::Nothing, |v| match v {
                                ConstValue::Value(value) => MolValue::Value(value),
                                ConstValue::FatPtr(value, metadata) => MolValue::FatPtr(value, metadata),
                            }),
                        ),
                        None => unimplemented!(
                            "can't compile {}: {field:?} => no value",
                            self.checker.adt_types[adt_ref].variants[variant].fields[field_ref].0
                        ),
                    },
                };

                match (field.ty, &got) {
                    (MollieType::Regular(ty), &MolValue::Value(v)) => {
                        debug_assert_eq!(
                            ty,
                            self.fn_builder.func.dfg.value_type(v),
                            "got incorrect type for {}::{}",
                            self.checker.adt_types[adt_ref].name.as_deref().unwrap_or_default(),
                            property.1
                        );

                        values.push(v);
                    }
                    (MollieType::Fat(ty, meta), got) => match *got {
                        MolValue::Value(v) => {
                            if let Some(got_ty) = got_ty {
                                debug_assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                let hash = self.checker.solver.hash_of(got_ty);
                                let metadata = self.fn_builder.ins().iconst(meta, hash.cast_signed());

                                values.push(v);
                                values.push(metadata);
                            }
                        }
                        MolValue::FatPtr(v, m) => {
                            debug_assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                            debug_assert_eq!(meta, self.fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                            values.push(v);
                            values.push(m);
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        }

        Ok(MolValue::Value(self.construct(ty, variant, &values)?))
    }
}
