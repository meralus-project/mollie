use cranelift::{
    codegen::ir::Endianness,
    prelude::{FunctionBuilder, InstBuilder, MemFlags, types},
};
use mollie_parser::{IsExpr, IsPattern};
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<IsPattern> {
    fn compile(self, _: &mut Compiler, _: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        // match self.value {
        //     AsPattern::Literal(literal) => {
        //         self.span.wrap(literal).compile(compiler, fn_builder)?;

        //         chunk.equals();
        //     }
        //     AsPattern::TypeName { ty, name } => {
        //         let name_ty = if let Some(trait_index) =
        // compiler.traits.get_index_of(&ty.value.name.value.0) {
        // chunk.copy();             chunk.impls(trait_index);

        // TypeVariant::complex(ComplexType::TraitInstance(compiler.infer.take().
        // unwrap(), trait_index)).into()         } else {
        //             let mut name_ty = ty.value.name.get_type(compiler)?;
        //             let ty_index =
        // compiler.types.get_index_of(&ty.value.name.value.0).unwrap();

        //             for generic in ty.value.generics {
        //                 name_ty.applied_generics.push(generic.get_type(compiler)?);
        //             }

        //             chunk.copy();
        //             chunk.is_instance_of(ty_index, 0);

        //             name_ty
        //         };

        //         let start = chunk.len();

        //         chunk.jump_if_false(0);

        //         let id = compiler.var(name.value.0, name_ty);

        //         chunk.set_local(id);

        //         let constant = chunk.constant(mollie_vm::Value::Boolean(true));

        //         chunk.load_const(constant);

        //         chunk.jump(3);
        //         chunk[start] = Inst::JumpIfFalse(chunk.len() - start);
        //         chunk.pop();

        //         let constant = chunk.constant(mollie_vm::Value::Boolean(false));

        //         chunk.load_const(constant);
        //     }
        //     AsPattern::Enum { target, index, values } => {
        //         let ty = target.get_type(compiler)?;
        //         let ty = Type {
        //             variant: ty.variant,
        //             applied_generics: if let Some(infer) =
        // compiler.infer.take_if(|ty| !ty.applied_generics.is_empty()) {
        //                 infer.applied_generics
        //             } else {
        //                 index.value.generics.iter().map(|ty|
        // ty.get_type(compiler)).collect::<TypeResult<Vec<_>>>()?
        // },             declared_at: ty.declared_at,
        //         };

        //         println!("{ty:#?}");

        //         let variant = ty
        //             .variant
        //             .as_enum()
        //             .unwrap()
        //             .variants
        //             .iter()
        //             .position(|variant| variant.0 == index.value.name.value.0)
        //             .unwrap();

        //         let ty_index = compiler
        //             .types
        //             .get_index_of(&target.value.0)
        //             .ok_or(CompileError::VariableNotFound { name: target.value.0 })?;

        //         chunk.copy();
        //         chunk.is_instance_of(ty_index, variant);

        //         if let Some(values) = values {
        //             chunk.copy();

        //             let start = chunk.len();

        //             chunk.jump_if_false(0);
        //             chunk.pop();

        //             for value in values.value {
        //                 let property =
        // ty.variant.as_enum().unwrap().variants[variant]
        // .1                     .properties
        //                     .as_ref()
        //                     .unwrap()
        //                     .iter()
        //                     .position(|property| property.0 ==
        // value.value.name.value.0)                     .unwrap();

        //                 chunk.get_property(property);

        //                 if let Some(val) = value.value.value {
        //                     val.compile(compiler, fn_builder)?;
        //                 } else {
        //                     let id = compiler.var(
        //                         value.value.name.value.0,
        //
        // ty.variant.as_enum().unwrap().variants[variant].1.properties.as_ref().
        // unwrap()[property]                             .1
        //                             .clone()
        //                             .resolve_type(&ty.applied_generics),
        //                     );

        //                     chunk.set_local(id);

        //                     let constant =
        // chunk.constant(mollie_vm::Value::Boolean(true));

        //                     chunk.load_const(constant);
        //                 }
        //             }

        //             chunk[start] = Inst::JumpIfFalse(chunk.len() - start + 1);

        //             chunk.jump(4);
        //             chunk.pop();
        //             chunk.pop();

        //             let constant = chunk.constant(mollie_vm::Value::Boolean(false));

        //             chunk.load_const(constant);
        //         }
        //     }
        // }

        unimplemented!()
    }
}

impl Compile<ValueOrFunc> for Positioned<IsExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        // let ty = self.value.target.get_type(compiler)?;

        let value = self.value.target.compile(compiler, fn_builder)?;

        // compiler.infer.replace(ty);

        // self.value.pattern.compile(compiler, fn_builder)?;

        // compiler.infer.take();

        if let ValueOrFunc::Value(value) = value {
            Ok(ValueOrFunc::Value(fn_builder.ins().bitcast(
                types::I64,
                MemFlags::new().with_endianness(Endianness::Little),
                value,
            )))
        } else {
            unimplemented!()
        }
    }
}

impl GetType for IsExpr {
    fn get_type(&self, _: &mut Compiler, _: Span) -> TypeResult {
        Ok(TypeVariant::boolean().into())
    }
}
