use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder, MemFlags},
};
use mollie_parser::{IndexExpr, IndexTarget};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ComplexType, FatPtr, FunctionType, PrimitiveType, Type, TypeKind, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeError, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IndexExpr> {
    #[allow(clippy::too_many_lines)]
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        let ty = self.value.target.get_type(compiler)?;

        let assign = compiler.assign.take();

        match self.value.index.value {
            IndexTarget::Named(index) => {
                if let Some((vtable, trait_index, function)) = compiler.find_vtable_function_index(&ty.variant, &index.0) {
                    if compiler.vtables[vtable][&trait_index].1[function]
                        .0
                        .variant
                        .as_function()
                        .is_some_and(|f| f.have_self)
                    {
                        let v = compiler.compile(fn_builder, *self.value.target)?;

                        compiler.this.replace(v);

                        return Ok(ValueOrFunc::Func(compiler.vtables[vtable][&trait_index].1[function].1.1));
                    }

                    return Err(TypeError::FunctionNotFound {
                        ty: Box::new(ty.variant.kind()),
                        ty_name: None,
                        function: index.0,
                    }
                    .into());
                } else if let Some(component) = ty.variant.as_component() {
                    let v = compiler.compile(fn_builder, *self.value.target)?;

                    if let Some(pos) = component.properties.iter().position(|(name, ..)| name == &index.0) {
                        if let Some(assign) = assign {
                            let value = compiler.compile(fn_builder, assign)?;

                            if let (ValueOrFunc::Value(v), ValueOrFunc::Value(value)) = (v, value) {
                                fn_builder.ins().store(MemFlags::trusted(), value, v, component.structure.fields[pos].offset);

                                return Ok(ValueOrFunc::Nothing);
                            }
                        } else if let ValueOrFunc::Value(v) = v {
                            return Ok(ValueOrFunc::Value(fn_builder.ins().load(
                                component.structure.fields[pos].ty,
                                MemFlags::trusted(),
                                v,
                                component.structure.fields[pos].offset,
                            )));
                        }

                        if let ValueOrFunc::Value(v) = v {
                            return Ok(ValueOrFunc::Value(fn_builder.ins().load(
                                component.structure.fields[pos].ty,
                                MemFlags::trusted(),
                                v,
                                component.structure.fields[pos].offset,
                            )));
                        }
                    } else if index.0 == "children" {
                        if let Some(assign) = assign {
                            let value = compiler.compile(fn_builder, assign)?;

                            if let (ValueOrFunc::Value(v), ValueOrFunc::Value(value)) = (v, value) {
                                fn_builder
                                    .ins()
                                    .store(MemFlags::trusted(), value, v, component.structure.fields.last().unwrap().offset);

                                return Ok(ValueOrFunc::Nothing);
                            }
                        } else if let ValueOrFunc::Value(v) = v {
                            return Ok(ValueOrFunc::Value(fn_builder.ins().load(
                                component.structure.fields.last().unwrap().ty,
                                MemFlags::trusted(),
                                v,
                                component.structure.fields.last().unwrap().offset,
                            )));
                        }
                    } else {
                        return Err(TypeError::PropertyNotFound {
                            ty: Box::new(TypeKind::Component),
                            ty_name: None,
                            property: index.0,
                        }
                        .into());
                    }
                } else if let Some(structure) = ty.variant.as_struct() {
                    let v = compiler.compile(fn_builder, *self.value.target)?;

                    if let Some(pos) = structure.properties.iter().position(|(name, ..)| name == &index.0) {
                        if let Some(assign) = assign {
                            let value = compiler.compile(fn_builder, assign)?;

                            if let (ValueOrFunc::Value(v), ValueOrFunc::Value(value)) = (v, value) {
                                fn_builder.ins().store(MemFlags::trusted(), v, value, structure.structure.fields[pos].offset);

                                return Ok(ValueOrFunc::Nothing);
                            }
                        } else if let ValueOrFunc::Value(v) = v {
                            return Ok(ValueOrFunc::Value(fn_builder.ins().load(
                                structure.structure.fields[pos].ty,
                                MemFlags::trusted(),
                                v,
                                structure.structure.fields[pos].offset,
                            )));
                        }
                    } else {
                        return Err(TypeError::PropertyNotFound {
                            ty: Box::new(TypeKind::Struct),
                            ty_name: None,
                            property: index.0,
                        }
                        .into());
                    }
                } else if let Some((_, _)) = ty.variant.as_trait_instance() {
                    // let function = compiler.traits[trait_index].functions.iter().position(|f| f.name == index.0).unwrap();

                    compiler.compile(fn_builder, *self.value.target)?;
                    // chunk.get_type_function2(Some(trait_index), function);
                    // compiler.compile(*self.value.target)?;
                } else if let TypeVariant::Trait(trait_index) = ty.variant {
                    let func = compiler.traits[trait_index].functions.iter().position(|f| f.name == index.0).unwrap();

                    if let ValueOrFunc::Value(fat_ptr) = compiler.compile(fn_builder, *self.value.target)? {
                        let value = FatPtr::get_ptr(compiler.jit.module.isa(), fn_builder, fat_ptr);

                        let size = compiler.jit.module.isa().pointer_type().bytes();
                        let size = i64::from(size);
                        let index = func.cast_signed() as i64;
                        let offset = size * index;

                        let vtable_ptr = FatPtr::get_metadata(compiler.jit.module.isa(), fn_builder, fat_ptr);
                        let vtable = fn_builder
                            .ins()
                            .load(compiler.jit.module.isa().pointer_type(), MemFlags::trusted(), vtable_ptr, 0);

                        let vtable_func = if offset > 0 {
                            let offset = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), offset);

                            fn_builder.ins().iadd(vtable, offset)
                        } else {
                            vtable
                        };

                        compiler.this.replace(ValueOrFunc::Value(value));

                        return Ok(ValueOrFunc::ExtFunc(compiler.traits[trait_index].functions[func].signature, vtable_func));
                    }
                }
            }
            IndexTarget::Expression(expression) => {
                if let Some(v) = ty.variant.as_array() {
                    let array = compiler.compile(fn_builder, *self.value.target)?;

                    let old = compiler.infer_ir.replace(compiler.jit.module.isa().pointer_type());

                    let index = compiler.compile(fn_builder, self.value.index.span.wrap(*expression)).unwrap();

                    if old.is_some() {
                        compiler.infer_ir = old;
                    } else if compiler.infer_ir.is_some() {
                        compiler.infer_ir.take();
                    }

                    if let (ValueOrFunc::Value(array), ValueOrFunc::Value(index)) = (array, index) {
                        let size = v.arr.ty.bytes();
                        let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), i64::from(size));
                        let offset = fn_builder.ins().imul(size, index);
                        let ptr = FatPtr::get_ptr(compiler.jit.module.isa(), fn_builder, array);
                        let ptr = fn_builder.ins().iadd(ptr, offset);

                        if let Some(assign) = assign {
                            if let ValueOrFunc::Value(value) = assign.compile(compiler, fn_builder)? {
                                fn_builder.ins().store(MemFlags::trusted(), value, ptr, 0);

                                return Ok(ValueOrFunc::Nothing);
                            }
                        } else {
                            return Ok(ValueOrFunc::Value(fn_builder.ins().load(v.arr.ty, MemFlags::trusted(), ptr, 0)));
                        }
                    }
                }
            }
        }

        unimplemented!()
    }
}

impl GetType for IndexExpr {
    #[allow(clippy::too_many_lines)]
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        if compiler.assign.is_some() {
            return Ok(TypeVariant::void().into());
        }

        let target = self.target.get_type(compiler)?;
        let mut result = match &self.index.value {
            IndexTarget::Named(property_name) => {
                if let Some((ty, _)) = compiler.find_vtable_function(&target.variant, &property_name.0) {
                    return Ok(Type {
                        variant: ty.variant.clone(),
                        applied_generics: target.applied_generics,
                        declared_at: ty.declared_at,
                    });
                }
                match target.variant {
                    TypeVariant::Generic(_) => todo!(),
                    TypeVariant::Primitive(_) => unimplemented!("primitive types doesn't have properties"),
                    TypeVariant::Trait(t) => {
                        return compiler.traits[t]
                            .functions
                            .iter()
                            .find(|func| func.name == property_name.0)
                            .map(|func| Type {
                                variant: TypeVariant::complex(ComplexType::Function(FunctionType {
                                    is_native: false,
                                    have_self: func.this,
                                    args: func.args.clone(),
                                    returns: Box::new(func.returns.clone()),
                                })),
                                applied_generics: target.applied_generics,
                                declared_at: compiler.traits[t].declared_at,
                            })
                            .ok_or_else(|| TypeError::PropertyNotFound {
                                ty: Box::new(TypeKind::Struct),
                                ty_name: None,
                                property: property_name.0.clone(),
                            });
                    }
                    TypeVariant::Complex(ref complex_type) => match &**complex_type {
                        ComplexType::Component(component) => {
                            if property_name.0 == "children" {
                                return Ok(Type {
                                    variant: component
                                        .children
                                        .as_ref()
                                        .ok_or_else(|| TypeError::PropertyNotFound {
                                            ty: Box::new(TypeKind::Component),
                                            ty_name: None,
                                            property: property_name.0.clone(),
                                        })?
                                        .clone()
                                        .variant,
                                    applied_generics: if target.applied_generics.is_empty() {
                                        vec![TypeVariant::Primitive(PrimitiveType::Component).into()]
                                    } else {
                                        target.applied_generics
                                    },
                                    declared_at: None,
                                });
                            }

                            component
                                .properties
                                .iter()
                                .find(|(name, ..)| name == &property_name.0)
                                .map(|(.., v)| v.clone().resolve_type(&target.applied_generics))
                                .ok_or_else(|| TypeError::PropertyNotFound {
                                    ty: Box::new(TypeKind::Component),
                                    ty_name: None,
                                    property: property_name.0.clone(),
                                })
                        }
                        ComplexType::Struct(structure) => structure
                            .properties
                            .iter()
                            .find(|(name, _)| name == &property_name.0)
                            .map(|(.., v)| v.clone().resolve_type(&target.applied_generics))
                            .ok_or_else(|| TypeError::PropertyNotFound {
                                ty: Box::new(TypeKind::Struct),
                                ty_name: None,
                                property: property_name.0.clone(),
                            }),
                        ComplexType::TraitInstance(ty, trait_index) => compiler.traits[*trait_index]
                            .functions
                            .iter()
                            .find(|f| f.name == property_name.0)
                            .map(|f| {
                                TypeVariant::complex(ComplexType::Function(FunctionType {
                                    is_native: false,
                                    have_self: f.this,
                                    args: {
                                        let mut args = vec![ty.clone()];

                                        args.extend(f.args.clone());

                                        args
                                    },
                                    returns: Box::new(f.returns.clone()),
                                }))
                                .into()
                            })
                            .ok_or_else(|| TypeError::PropertyNotFound {
                                ty: Box::new(TypeKind::Struct),
                                ty_name: None,
                                property: property_name.0.clone(),
                            }),
                        _ => unimplemented!(
                            "{} cannot be indexed by {}",
                            target.clone().resolve_type(&target.applied_generics),
                            property_name.0
                        ),
                    },
                }
            }
            IndexTarget::Expression(expression) => {
                let old = compiler.infer.replace(TypeVariant::usize().into());

                let index = expression.get_type(compiler, self.index.span)?;

                if old.is_some() {
                    compiler.infer = old;
                } else if compiler.infer.is_some() {
                    compiler.infer.take();
                }

                if !index.variant.same_as(&TypeVariant::Primitive(PrimitiveType::USize), &target.applied_generics) {
                    return Err(TypeError::Unexpected {
                        got: Box::new(index.kind()),
                        expected: Box::new(TypeVariant::Primitive(PrimitiveType::U32).kind().into()),
                    });
                }

                match target.variant {
                    TypeVariant::Generic(_) => todo!(),
                    TypeVariant::Primitive(_) => unimplemented!("primitive types doesn't have properties"),
                    TypeVariant::Trait(_) => unimplemented!(),
                    TypeVariant::Complex(complex_type) => match &*complex_type {
                        ComplexType::Array(array) => Ok(array.element.clone()),
                        _ => unimplemented!("functions cannot be indexed"),
                    },
                }
            }
        }?;

        result.applied_generics.extend(target.applied_generics);

        Ok(result)
    }
}
