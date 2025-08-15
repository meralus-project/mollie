use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder},
};
use indexmap::IndexMap;
use mollie_parser::NodeExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::{FatPtr, Type, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<NodeExpr> {
    #[allow(clippy::too_many_lines)]
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

            for (property_name, _, property_type) in &component.properties {
                let property = self
                    .value
                    .properties
                    .iter()
                    .position(|property| &property.value.name.value.0 == property_name)
                    .map(|index| self.value.properties.remove(index));

                if let Some(property) = property {
                    let old = compiler.infer_ir.replace(property_type.variant.as_ir_type(compiler.jit.module.isa()));
                    let result = compiler.compile(fn_builder, property.value.value)?;

                    if old.is_some() {
                        compiler.infer_ir = old;
                    } else if compiler.infer_ir.is_some() {
                        compiler.infer_ir.take();
                    }

                    if let ValueOrFunc::Value(v) = result {
                        values.push(v);
                    }
                }
            }

            // let size = self.value.children.value.len();

            if let Some(ty) = &component.children {
                let mut children = Vec::new();

                for node in self.value.children.value {
                    let node_ty = node.get_type(compiler)?;

                    if let Some(array) = ty.variant.as_array()
                        && let TypeVariant::Trait(trait_index) = array.element.variant
                    {
                        let vtable = compiler.vtables.get(&node_ty.variant).unwrap().get(&Some(trait_index)).unwrap().0;

                        if let ValueOrFunc::Value(value) = compiler.compile(fn_builder, node)? {
                            println!("GUYS IT'S FAT PTR");
                            children.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, value, vtable));
                        }
                    } else if let TypeVariant::Trait(trait_index) = ty.variant {
                        let vtable = compiler.vtables.get(&node_ty.variant).unwrap().get(&Some(trait_index)).unwrap().0;

                        if let ValueOrFunc::Value(value) = compiler.compile(fn_builder, node)? {
                            println!("GUYS IT'S FAT PTR");
                            children.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, value, vtable));
                        }
                    } else if let ValueOrFunc::Value(value) = compiler.compile(fn_builder, node)? {
                        children.push(value);
                    }
                }

                if let Some(array) = ty.variant.as_array() {
                    let size = children.len();
                    let ptr = array.arr.instance(&compiler.jit.module, fn_builder, children);
                    let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), size.cast_signed() as i64);

                    values.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, ptr, size));
                } else if children.len() == 1 {
                    values.push(children[0]);
                }
            }

            compiler.generics = Vec::new();

            Ok(ValueOrFunc::Value(component.structure.instance(&compiler.jit.module, fn_builder, values)))
        } else if let Some(structure) = ty.variant.as_struct() {
            let mut values = Vec::new();

            for (name, ty) in &structure.properties {
                let property = self
                    .value
                    .properties
                    .iter()
                    .position(|property| &property.value.name.value.0 == name)
                    .map(|index| self.value.properties.remove(index));

                if let Some(property) = property {
                    let old = compiler.infer_ir.replace(ty.variant.as_ir_type(compiler.jit.module.isa()));
                    let result = compiler.compile(fn_builder, property.value.value)?;

                    if old.is_some() {
                        compiler.infer_ir = old;
                    } else if compiler.infer_ir.is_some() {
                        compiler.infer_ir.take();
                    }

                    if let ValueOrFunc::Value(v) = result {
                        values.push(v);
                    }
                }
            }

            // for node in self.value.children.value {
            //     compiler.compile(chunk, fn_builder, node)?;
            // }

            compiler.generics = Vec::new();

            Ok(ValueOrFunc::Value(structure.structure.instance(&compiler.jit.module, fn_builder, values)))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for NodeExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        let ty = compiler
            .types
            .get(&self.name.value.name.value.0)
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
