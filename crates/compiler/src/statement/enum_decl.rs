use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::EnumDecl;
use mollie_shared::Positioned;
use mollie_typing::{ComplexType, EnumType, EnumVariant, Struct, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType};

impl Compile for Positioned<EnumDecl> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult {
        let mut variants = Vec::new();

        for (index, name) in self.value.name.value.generics.iter().enumerate() {
            compiler.add_type(&name.value.0, TypeVariant::Generic(index));
        }

        for variant in self.value.variants.value {
            let properties = if let Some(properties) = variant.value.properties {
                let mut props = Vec::with_capacity(properties.value.len());

                for property in properties.value {
                    props.push((property.value.name.value.0, property.value.ty.get_type(compiler)?));
                }

                Some(props)
            } else {
                None
            };

            let structure = properties.as_ref().map(|properties| {
                Struct::new(
                    properties
                        .iter()
                        .map(|property| (property.1.variant.as_ir_type(compiler.jit.module.isa()), None)),
                )
            });

            variants.push((variant.value.name.value.0, EnumVariant { properties, structure }));
        }

        for name in &self.value.name.value.generics {
            compiler.remove_type(&name.value.0);
        }

        let ty = Type {
            applied_generics: Vec::new(),
            variant: TypeVariant::complex(ComplexType::EnumType(EnumType {
                generics: self.value.name.value.generics.into_iter().map(|g| g.value.0).collect(),
                variants,
            })),
            declared_at: Some(self.span),
        };

        compiler.types.insert(self.value.name.value.name.value.0, ty);

        Ok(())
    }
}
