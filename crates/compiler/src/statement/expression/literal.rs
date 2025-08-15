use cranelift::{
    module::Module,
    prelude::{FunctionBuilder, InstBuilder, Value, types},
};
use mollie_parser::{LiteralExpr, Number as LiteralNumber};
use mollie_shared::{Positioned, Span};
use mollie_typing::{FatPtr, TypeVariant};

use crate::{Compile, CompileResult, Compiler, GetType, TypeResult};

impl Compile<Value> for Positioned<LiteralExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<Value> {
        use LiteralExpr::{Boolean, Null, Number, SizeUnit, String};
        use LiteralNumber::{F32, I64};

        match self.value {
            SizeUnit(..) => unimplemented!(),
            Number(number, postfix) => match (number, postfix.as_deref()) {
                (I64(value), Some("uint_size" | "int_size")) => Ok(fn_builder.ins().iconst(compiler.jit.module.isa().pointer_type(), value)),
                (I64(value), Some("uint64" | "int64")) => Ok(fn_builder.ins().iconst(types::I64, value)),
                (I64(value), Some("uint16" | "int16")) => Ok(fn_builder.ins().iconst(types::I16, value)),
                (I64(value), Some("uint8" | "int8")) => Ok(fn_builder.ins().iconst(types::I8, value)),
                (I64(value), Some("uint32" | "int32")) => Ok(fn_builder.ins().iconst(types::I32, value)),
                (I64(value), None) => {
                    if let Some(ty) = compiler.infer_ir.take() {
                        Ok(fn_builder.ins().iconst(ty, value))
                    } else {
                        Ok(fn_builder.ins().iconst(types::I32, value))
                    }
                }
                (I64(_), _) => unimplemented!(),
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

                let ptr = fn_builder.ins().symbol_value(compiler.jit.module.isa().pointer_type(), data_id);
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
            SizeUnit(..) => TypeVariant::of::<()>(),
            Number(I64(_), postfix) => match postfix.as_deref() {
                Some("uint64") => TypeVariant::of::<u64>(),
                Some("uint32") => TypeVariant::of::<u32>(),
                Some("uint16") => TypeVariant::of::<u16>(),
                Some("uint8") => TypeVariant::of::<u8>(),
                Some("int64") => TypeVariant::of::<i64>(),
                Some("int16") => TypeVariant::of::<i16>(),
                Some("int8") => TypeVariant::of::<i8>(),
                _ => compiler
                    .infer
                    .take_if(|ty| ty.variant.is_integer())
                    .map_or_else(TypeVariant::of::<i32>, |ty| ty.variant),
            },
            Number(F32(_), _) => TypeVariant::of::<f32>(),
            Boolean(_) => TypeVariant::of::<bool>(),
            String(_) => TypeVariant::of::<&str>(),
            Null => TypeVariant::null(),
        }
        .into())
    }
}
