use cranelift::prelude::FunctionBuilder;
use mollie_parser::Stmt;
use mollie_shared::{Positioned, Span};

use crate::{Compile, CompileResult, Compiler, GetType, TypeResult, ValueOrFunc};

mod component_decl;
mod enum_decl;
mod expression;
mod implementation;
mod struct_decl;
mod trait_decl;
mod variable_decl;

impl Compile<ValueOrFunc> for Positioned<Stmt> {
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        use Stmt::{ComponentDecl, EnumDecl, Expression, Impl, StructDecl, TraitDecl, VariableDecl};

        match self.value {
            Expression(value) => return compiler.compile(fn_builder, self.span.wrap(value)),
            ComponentDecl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
            StructDecl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
            Impl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
            TraitDecl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
            VariableDecl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
            EnumDecl(value) => compiler.compile(fn_builder, self.span.wrap(value))?,
        }

        Ok(ValueOrFunc::Nothing)
    }
}

impl GetType for Stmt {
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult {
        use Stmt::{ComponentDecl, EnumDecl, Expression, Impl, StructDecl, TraitDecl, VariableDecl};

        match self {
            Expression(value) => value.get_type(compiler, span),
            Impl(_) | ComponentDecl(_) | StructDecl(_) | TraitDecl(_) | VariableDecl(_) | EnumDecl(_) => Ok(().into()),
        }
    }
}
