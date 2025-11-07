use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder, Value, types},
};
use mollie_ir::FatPtr;
use mollie_parser::{LiteralExpr, Number as LiteralNumber};
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeResult};

impl Compile<Value> for Positioned<LiteralExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<Value> {
        use LiteralExpr::{Boolean, Null, Number, SizeUnit, String};
        use LiteralNumber::{F32, I64};

        let ty = self.get_type(compiler)?;

        match self.value {
            SizeUnit(..) => unimplemented!(),
            Number(number, postfix) => match (number, postfix.as_deref()) {
                (I64(value), _) => Ok(fn_builder.ins().iconst(ty.variant.as_ir_type(compiler.jit.module.isa()), value)),
                (F32(value), _) => Ok(fn_builder.ins().f32const(value)),
            },
            Boolean(value) => Ok(fn_builder.ins().iconst(types::I8, i64::from(value))),
            String(value) => {
                let len = value.len();

                compiler.jit.data_desc.define(value.into_bytes().into_boxed_slice());

                let id = compiler.jit.module.declare_anonymous_data(true, false).unwrap();

                compiler.jit.module.define_data(id, &compiler.jit.data_desc).unwrap();
                compiler.jit.data_desc.clear();
                compiler.jit.module.finalize_definitions().unwrap();

                let data_id = compiler.jit.module.declare_data_in_func(id, fn_builder.func);

                let ptr = fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(), data_id);
                let size = fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), len.cast_signed() as i64);

                Ok(FatPtr::new(compiler.jit.module.isa(), fn_builder, ptr, size))
            }
            Null => unimplemented!(),
        }
    }
}

impl GetType for LiteralExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        use LiteralExpr::{Boolean, Null, Number, SizeUnit, String};
        use LiteralNumber::{F32, I64};

        Ok(match self {
            SizeUnit(..) => TypeVariant::void(),
            Number(I64(_), postfix) => match postfix.as_deref() {
                Some("uint_size") => TypeVariant::usize(),
                Some("uint64") => TypeVariant::uint64(),
                Some("uint32") => TypeVariant::uint32(),
                Some("uint16") => TypeVariant::uint16(),
                Some("uint8") => TypeVariant::uint8(),
                Some("int_size") => TypeVariant::isize(),
                Some("int64") => TypeVariant::int64(),
                Some("int16") => TypeVariant::int16(),
                Some("int8") => TypeVariant::int8(),
                _ => compiler
                    .infer
                    .take_if(|ty| ty.variant.is_integer())
                    .map_or_else(TypeVariant::int32, |ty| ty.variant),
            },
            Number(F32(_), _) => TypeVariant::float(),
            Boolean(_) => TypeVariant::boolean(),
            String(_) => TypeVariant::string(),
            Null => TypeVariant::null(),
        }
        .into())
    }
}
