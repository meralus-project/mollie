use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use indexmap::IndexMap;
use mollie_ir::FatPtr;
use mollie_parser::NodeExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{Type, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetNewPositionedType, GetNewType, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<NodeExpr> {
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn compile(mut self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.get_type(compiler)?;

        if let Some(component) = ty.variant.as_component() {
            for prop in &self.value.properties {
                let (.., expected) =
                    component
                        .properties
                        .iter()
                        .find(|(name, ..)| name == &prop.value.name.value.0)
                        .ok_or_else(|| TypeError::PropertyNotFound {
                            ty: Box::new(TypeKind::Component),
                            ty_name: Some(self.value.name.value.name.value.0.clone()),
                            property: prop.value.name.value.0.clone(),
                        })?;

                let old = compiler.infer.replace(expected.clone());

                let got = prop.value.value.get_type(compiler)?;

                if old.is_some() {
                    compiler.infer = old;
                } else if compiler.infer.is_some() {
                    compiler.infer.take();
                }

                if !got.variant.same_as(&expected.variant, &compiler.generics) {
                    return Err(TypeError::Unexpected {
                        got: Box::new(got.kind()),
                        expected: Box::new(expected.kind()),
                    }
                    .into());
                }
            }

            // let children = ComponentChildren::from(self.value.children.value.len());

            // if !component.children.validate(&children) {
            //     return Err(TypeError::InvalidChildren {
            //         got: children,
            //         expected: component.children,
            //         ty_name: self.value.name.value.name.value.0,
            //     }
            //     .into());
            // }

            let mut values = Vec::new();

            for (property_name, _, ty) in &component.properties {
                let property = self
                    .value
                    .properties
                    .iter()
                    .position(|property| &property.value.name.value.0 == property_name)
                    .map(|index| self.value.properties.remove(index));

                if let Some(property) = property {
                    let old = compiler.infer.replace(ty.clone());

                    if let ValueOrFunc::Value(v) = compiler.compile(fn_builder, property.value.value)? {
                        values.push(v);
                    }

                    if old.is_some() {
                        compiler.infer = old;
                    } else if compiler.infer.is_some() {
                        compiler.infer.take();
                    }
                } else if let Some(field) = component.properties.iter().position(|(name, ..)| name == property_name)
                    && let Some(v) = component
                        .structure
                        .default_for(&mut compiler.jit.module, &mut compiler.jit.data_desc, fn_builder, field)
                {
                    values.push(v);
                }
            }

            // let size = self.value.children.value.len();

            if let Some(ty) = &component.children {
                let mut children = Vec::new();

                for node in self.value.children.value {
                    let node_ty = node.get_type(compiler)?;

                    if let ValueOrFunc::Value(value) = compiler.compile(fn_builder, node)? {
                        if let Some(array) = ty.variant.as_array()
                            && let TypeVariant::Trait(trait_index) = array.element.variant
                        {
                            let vtable = compiler.vtables.get(&node_ty.variant).unwrap().get(&Some(trait_index)).unwrap();
                            let data_id = compiler.jit.module.declare_data_in_func(vtable.0, fn_builder.func);

                            let vtable = fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(), data_id);

                            children.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, value, vtable));
                        } else if let TypeVariant::Trait(trait_index) = ty.variant {
                            let vtable = compiler.vtables.get(&node_ty.variant).unwrap().get(&Some(trait_index)).unwrap().0;
                            let data_id = compiler.jit.module.declare_data_in_func(vtable, fn_builder.func);
                            let vtable = fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(), data_id);

                            children.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, value, vtable));
                        } else {
                            children.push(value);
                        }
                    }
                }

                if let Some(array) = ty.variant.as_array() {
                    let size = children.len();
                    let ptr = array.array.instance(compiler.jit.module.isa(), fn_builder, children);
                    let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), size.cast_signed() as i64);

                    values.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, ptr, size));
                } else if children.len() == 1 {
                    values.push(children[0]);
                }
            }

            compiler.generics = Vec::new();

            Ok(ValueOrFunc::Value(component.structure.instance(compiler.jit.module.isa(), fn_builder, values)))
        } else if let Some(structure) = ty.variant.as_struct() {
            let mut values = Vec::new();

            for ((name, ty), field) in structure.properties.iter().zip(&structure.structure.fields) {
                let property = self
                    .value
                    .properties
                    .iter()
                    .position(|property| &property.value.name.value.0 == name)
                    .map(|index| self.value.properties.remove(index));

                if let Some(property) = property {
                    let old = compiler.infer.replace(ty.clone());

                    let got = property.value.value.get_type(compiler)?;

                    if old.is_some() {
                        compiler.infer = old;
                    } else {
                        compiler.infer.take();
                    }

                    let old = compiler.infer.replace(ty.clone());

                    if let ValueOrFunc::Value(v) = compiler.compile(fn_builder, property.value.value)? {
                        assert_eq!(field.ty, fn_builder.func.dfg.value_type(v), "got incorrect type for {name}");

                        if let TypeVariant::Trait(trait_index) = ty.variant {
                            let vtable = compiler.vtables[compiler.get_vtable_index(&got.variant).unwrap()][&Some(trait_index)].0;
                            let data_id = compiler.jit.module.declare_data_in_func(vtable, fn_builder.func);
                            let vtable = fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(), data_id);

                            values.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, v, vtable));
                        } else {
                            values.push(v);
                        }
                    }

                    if old.is_some() {
                        compiler.infer = old;
                    } else {
                        compiler.infer.take();
                    }
                } else if let Some(field) = structure.properties.iter().position(|(property_name, _)| property_name == name)
                    && let Some(v) = structure
                        .structure
                        .default_for(&mut compiler.jit.module, &mut compiler.jit.data_desc, fn_builder, field)
                {
                    values.push(v);
                }
            }

            // for node in self.value.children.value {
            //     compiler.compile(chunk, fn_builder, node)?;
            // }

            compiler.generics = Vec::new();

            Ok(ValueOrFunc::Value(structure.structure.instance(compiler.jit.module.isa(), fn_builder, values)))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for NodeExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        let ty = compiler
            .get_type(&self.name.value.name.value.0)
            .ok_or_else(|| TypeError::NotFound {
                ty: Some(Box::new(TypeKind::OneOf(vec![TypeKind::Component, TypeKind::Struct]))),
                name: self.name.value.name.value.0.clone(),
            })?
            .clone();

        if let Some(structure) = ty.variant.as_struct() {
            let applied_generics = self
                .name
                .value
                .generics
                .iter()
                .map(|ty| ty.get_type(compiler))
                .collect::<TypeResult<Vec<_>>>()?;

            let mut resolved_generics = IndexMap::new();

            for prop in &self.properties {
                let (.., expected) =
                    structure
                        .properties
                        .iter()
                        .find(|(name, ..)| name == &prop.value.name.value.0)
                        .ok_or_else(|| TypeError::PropertyNotFound {
                            ty: Box::new(TypeKind::Struct),
                            ty_name: Some(self.name.value.name.value.0.clone()),
                            property: prop.value.name.value.0.clone(),
                        })?;

                let old = compiler.infer.replace(expected.clone());

                let got = prop.value.value.get_type(compiler)?;

                if old.is_some() {
                    compiler.infer = old;
                } else if compiler.infer.is_some() {
                    compiler.infer.take();
                }

                if let TypeVariant::Generic(position) = expected.variant
                    && position >= applied_generics.len()
                {
                    resolved_generics.insert(position, got);
                } else if let TypeVariant::Trait(trait_index) = expected.variant {
                    if !compiler.vtables[compiler.get_vtable_index(&got.variant).unwrap()].contains_key(&Some(trait_index)) {
                        return Err(TypeError::Unexpected {
                            got: Box::new(got.kind()),
                            expected: Box::new(expected.kind()),
                        });
                    }
                } else if !got.variant.same_as(&expected.variant, &applied_generics) {
                    return Err(TypeError::Unexpected {
                        got: Box::new(got.kind()),
                        expected: Box::new(expected.kind()),
                    });
                }
            }

            if resolved_generics.is_empty() {
                Ok(Type {
                    variant: ty.variant.clone(),
                    applied_generics,
                    declared_at: ty.declared_at,
                })
            } else {
                resolved_generics.sort_unstable_keys();

                Ok(Type {
                    variant: ty.variant.clone(),
                    applied_generics: resolved_generics.into_values().collect(),
                    declared_at: ty.declared_at,
                })
            }
        } else if ty.variant.is_component() {
            let mut applied_generics = Vec::new();

            for ty in &self.name.value.generics {
                applied_generics.push(ty.get_type(compiler)?);
            }

            Ok(Type {
                variant: ty.variant.clone(),
                applied_generics,
                declared_at: ty.declared_at,
            })
        } else {
            Err(TypeError::Unexpected {
                got: Box::new(ty.kind()),
                expected: Box::new(TypeKind::Struct.into()),
            })
        }
    }
}

impl GetNewType for NodeExpr {
    fn get_new_type(
        &self,
        compiler: &mut Compiler,
        core_types: &mollie_typing::CoreTypes,
        type_storage: &mut mollie_typing::TypeStorage,
        type_solver: &mut mollie_typing::TypeSolver,
        span: Span,
    ) -> TypeResult<mollie_typing::TypeInfoRef> {
        let ty = type_storage
            .get_named_type(&self.name.value.name.value.0)
            .ok_or_else(|| TypeError::NotFound {
                ty: Some(Box::new(TypeKind::OneOf(vec![TypeKind::Component, TypeKind::Struct]))),
                name: self.name.value.name.value.0.clone(),
            })?
            .clone();

        if let Some(structure) = ty.variant.as_struct() {
            let expected_structure = mollie_typing::TypeInfo::Struct(
                structure
                    .properties
                    .iter()
                    .map(|(name, prop)| {
                        (
                            name.clone(),
                            type_solver.add_info(mollie_typing::TypeInfo::Type(type_storage.ref_of_type(prop).unwrap())),
                        )
                    })
                    .collect::<Vec<_>>(),
            );

            let expected_structure = type_solver.add_info(expected_structure);

            let structure = mollie_typing::TypeInfo::Struct(
                structure
                    .properties
                    .iter()
                    .map(|(name, _)| {
                        let prop = self.properties.iter().find(|prop| &prop.value.name.value.0 == name)?;

                        prop.value
                            .value
                            .get_new_type(compiler, core_types, type_storage, type_solver)
                            .ok()
                            .map(|value| (prop.value.name.value.0.clone(), value))
                    })
                    .collect::<Option<Vec<_>>>()
                    .unwrap(),
            );

            let structure = type_solver.add_info(structure);

            type_solver.unify(expected_structure, structure);

            Ok(expected_structure)
        } else {
            unimplemented!()
        }
    }
}
