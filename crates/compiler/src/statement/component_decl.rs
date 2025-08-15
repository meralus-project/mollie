use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_parser::ComponentDecl;
use mollie_shared::Positioned;
use mollie_typing::{ComplexType, ComponentType, Struct, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType};

impl Compile for Positioned<ComponentDecl> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult {
        let mut properties = Vec::new();
        let mut children = None;
        let mut fields = Vec::new();

        for property in &self.value.properties {
            let nullable = property.value.nullable.is_some();
            let name = &property.value.name.value.0;
            let ty = property.value.ty.get_type(compiler)?;

            if name == "children" {
                children.replace(ty);
            } else {
                fields.push(ty.variant.as_ir_type(compiler.jit.module.isa()));

                properties.push((name.clone(), nullable, ty));
            }
        }

        if children.is_some() {
            fields.push(compiler.jit.module.isa().pointer_type());
        }

        let ty = Type {
            applied_generics: Vec::new(),
            variant: TypeVariant::complex(ComplexType::Component(ComponentType {
                structure: Struct::new(fields),
                properties,
                children,
            })),
            declared_at: Some(self.span),
        };

        compiler.types.insert(self.value.name.value.0, ty);

        // compiler.var(self.value.name.value.0, ty.variant);

        Ok(())
    }
}
