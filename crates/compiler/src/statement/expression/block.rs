use cranelift::prelude::FunctionBuilder;
use mollie_parser::BlockExpr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetPositionedType, GetType, TypeResult, ValueOrFunc};

impl Compile<ValueOrFunc> for Positioned<BlockExpr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        for statement in self.value.stmts {
            compiler.compile(fn_builder, statement)?;
        }

        Ok(if let Some(final_statement) = self.value.final_stmt {
            compiler.compile(fn_builder, *final_statement)?
        } else {
            ValueOrFunc::Nothing
        })
    }
}

impl GetType for BlockExpr {
    fn get_type(&self, compiler: &mut Compiler, _: Span) -> TypeResult {
        self.final_stmt
            .as_ref()
            .map_or_else(|| Ok(TypeVariant::void().into()), |final_statement| final_statement.get_type(compiler))
    }
}
