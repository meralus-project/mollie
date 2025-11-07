use cranelift::prelude::FunctionBuilder;
use mollie_parser::Expr;
use mollie_shared::{Positioned, Span};
use mollie_typing::TypeVariant;

use crate::{Compile, CompileResult, Compiler, GetType, TypeResult, ValueOrFunc};

mod array;
mod as_expr;
mod binary;
mod block;
mod call;
mod closure_expr;
mod enum_path;
mod ident;
mod if_else;
mod index;
mod literal;
mod node;
mod type_index;
mod while_expr;

impl Compile<ValueOrFunc> for Positioned<Expr> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        use Expr::{Array, Binary, Block, Closure, EnumPath, FunctionCall, Ident, IfElse, Index, Is, Literal, Node, This, TypeIndex, While};

        match self.value {
            Literal(value) => compiler.compile(fn_builder, self.span.wrap(value)).map(ValueOrFunc::Value),
            FunctionCall(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Node(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Array(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Index(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            IfElse(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Binary(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            While(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            EnumPath(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Is(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Block(value) => {
                compiler.push_frame();

                let returns = compiler.compile(fn_builder, self.span.wrap(value))?;

                compiler.pop_frame();

                Ok(returns)
            }
            Ident(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            TypeIndex(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            Closure(value) => compiler.compile(fn_builder, self.span.wrap(value)),
            This => compiler
                .variables
                .get("self")
                .map_or_else(|| unimplemented!(), |v| Ok(ValueOrFunc::Value(fn_builder.use_var(*v)))),
        }
    }
}

impl GetType for Expr {
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult {
        use Expr::{Array, Binary, Block, Closure, EnumPath, FunctionCall, Ident, IfElse, Index, Is, Literal, Node, This, TypeIndex, While};

        match self {
            Literal(value) => value.get_type(compiler, span),
            FunctionCall(value) => value.get_type(compiler, span),
            Node(value) => value.get_type(compiler, span),
            IfElse(value) => value.get_type(compiler, span),
            Index(value) => value.get_type(compiler, span),
            Binary(value) => value.get_type(compiler, span),
            EnumPath(value) => value.get_type(compiler, span),
            Block(value) => value.get_type(compiler, span),
            Array(value) => value.get_type(compiler, span),
            TypeIndex(value) => value.get_type(compiler, span),
            Ident(value) => value.get_type(compiler, span),
            While(value) => value.get_type(compiler, span),
            Closure(value) => value.get_type(compiler, span),
            Is(_) => Ok(TypeVariant::boolean().into()),
            This => compiler.get_local_type("self"),
        }
    }
}
