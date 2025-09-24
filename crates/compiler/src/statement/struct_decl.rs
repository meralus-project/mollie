use cranelift::{module::Module, prelude::FunctionBuilder};
use mollie_const::ConstantValue;
use mollie_parser::StructDecl;
use mollie_shared::Positioned;
use mollie_typing::{ComplexType, Struct, StructType, Type, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetPositionedType};

impl Compile for Positioned<StructDecl> {
    fn compile(self, compiler: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult {
        let mut properties = Vec::new();

        for (index, name) in self.value.name.value.generics.iter().enumerate() {
            compiler.add_type(&name.value.0, TypeVariant::Generic(index));
        }

        let mut fields = Vec::new();

        for property in &self.value.properties.value {
            let name = &property.value.name.value.0;
            let ty = property.value.ty.get_type(compiler)?;
            let constant = property.value.default_value.as_ref().and_then(|value| ConstantValue::to_constant(&value.value));

            fields.push((ty.variant.as_ir_type(compiler.jit.module.isa()), constant));

            properties.push((name.clone(), ty));
        }

        for name in &self.value.name.value.generics {
            compiler.remove_type(&name.value.0);
        }

        let ty = Type {
            applied_generics: Vec::new(),
            variant: TypeVariant::complex(ComplexType::Struct(StructType {
                structure: Struct::new(fields),
                generics: self.value.name.value.generics.into_iter().map(|g| g.value.0).collect(),
                properties,
            })),
            declared_at: Some(self.span),
        };

        compiler.types.insert(self.value.name.value.name.value.0, ty);

        // compiler.var(self.value.name.value.0, ty.variant);

        Ok(())
    }
}
