use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_const::ConstantValue;
use mollie_ir::Struct;
use mollie_parser::ComponentDecl;
use mollie_shared::Positioned;
use mollie_typing::{ComplexType, ComponentType, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType};

impl Compile for Positioned<ComponentDecl> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult {
        let mut properties = Vec::new();
        let mut children = None;
        let mut children_value = None;
        let mut fields = Vec::new();

        for property in &self.value.properties {
            let nullable = property.value.nullable.is_some();
            let name = &property.value.name.value.0;
            let ty = property.value.ty.get_type(compiler)?;
            let constant = property.value.default_value.as_ref().and_then(|value| ConstantValue::to_constant(&value.value));

            if name == "children" {
                children.replace(ty);
                children_value = constant;
            } else {
                fields.push((ty.variant.as_ir_type(compiler.jit.module.isa()), constant));

                properties.push((name.clone(), nullable, ty));
            }
        }

        if children.is_some() {
            fields.push((compiler.jit.module.isa().pointer_type(), children_value));
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

        compiler.add_declared_type(self.value.name.value.0, ty);

        // compiler.var(self.value.name.value.0, ty.variant);

        Ok(())
    }
}
