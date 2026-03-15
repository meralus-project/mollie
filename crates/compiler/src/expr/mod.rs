mod array;
mod array_index;
mod binary;
mod call;
mod closure;
mod construct;
mod field_access;
mod if_else;
mod is_pattern;
mod literal;
mod var;
mod vtable_access;
mod r#while;

use std::cmp::Ordering;

use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use indexmap::map::Entry;
use mollie_index::Idx;
use mollie_ir::{MollieType, VTablePtr};
use mollie_typed_ast::{Expr, ExprRef, TypedAST};
use mollie_typing::{IntType, PrimitiveType, Type, UIntType};

use crate::{
    AsIrType, CompileTypedAST, MolValue,
    error::CompileResult,
    func::{FuncKey, FunctionCompiler},
};

const fn get_ir_type(primitive: PrimitiveType, ptr_type: ir::Type) -> ir::Type {
    match primitive {
        PrimitiveType::Int(int_type) => match int_type {
            IntType::ISize => ptr_type,
            IntType::I64 => ir::types::I64,
            IntType::I32 => ir::types::I32,
            IntType::I16 => ir::types::I16,
            IntType::I8 => ir::types::I8,
        },
        PrimitiveType::UInt(uint_type) => match uint_type {
            UIntType::USize => ptr_type,
            UIntType::U64 => ir::types::I64,
            UIntType::U32 => ir::types::I32,
            UIntType::U16 => ir::types::I16,
            UIntType::U8 => ir::types::I8,
        },
        PrimitiveType::F32 => ir::types::F32,
        PrimitiveType::Bool => ir::types::I8,
        PrimitiveType::String => ptr_type,
        PrimitiveType::Any | PrimitiveType::Void => ir::types::INVALID,
    }
}

impl<S, ML: mollie_typed_ast::ModuleLoader<S>, M: Module> CompileTypedAST<S, ML, M, MolValue> for ExprRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, S, ML, M>) -> CompileResult<MolValue> {
        match &ast[self].value {
            Expr::Lit(literal_expr) => compiler.compile_lit_expr(ast, self, literal_expr),
            &Expr::IfElse { condition, block, otherwise } => compiler.compile_if_expr(ast, condition, block, otherwise),
            &Expr::TypeCast(expr, ty) => {
                let value = expr.compile(ast, compiler)?.expect_value();
                let ty = compiler.type_context.type_context.core_types.cast_primitive(ty);

                if let (&Type::Primitive(got), &Type::Primitive(cast_to)) = (
                    &compiler.type_context.type_context.types[ast[expr].ty],
                    &compiler.type_context.type_context.types[ty],
                ) && let MollieType::Regular(ir_type) = ty.as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa())
                {
                    let ptr_type = compiler.compiler.ptr_type();
                    let a = get_ir_type(got, ptr_type);
                    let b = get_ir_type(cast_to, ptr_type);

                    let value = if got.is_num() && cast_to.is_num() {
                        if got.is_int() && cast_to.is_int() {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => compiler.fn_builder.ins().sextend(ir_type, value),
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        } else if got.is_uint() && cast_to.is_uint() {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => compiler.fn_builder.ins().uextend(ir_type, value),
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        } else {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => {
                                    if cast_to.is_int() {
                                        compiler.fn_builder.ins().sextend(ir_type, value)
                                    } else {
                                        compiler.fn_builder.ins().uextend(ir_type, value)
                                    }
                                }
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        }
                    } else if got.is_f32() && cast_to.is_num() {
                        let value = match a.bytes().cmp(&b.bytes()) {
                            Ordering::Less => {
                                if cast_to.is_int() {
                                    compiler.fn_builder.ins().sextend(ir::types::I32, value)
                                } else {
                                    compiler.fn_builder.ins().uextend(ir::types::I32, value)
                                }
                            }
                            Ordering::Equal => value,
                            Ordering::Greater => compiler.fn_builder.ins().ireduce(ir::types::I32, value),
                        };

                        if cast_to.is_int() {
                            compiler.fn_builder.ins().fcvt_to_sint(ir_type, value)
                        } else {
                            compiler.fn_builder.ins().fcvt_to_uint(ir_type, value)
                        }
                    } else if got.is_num() && cast_to.is_f32() {
                        let value = match a.bytes().cmp(&b.bytes()) {
                            Ordering::Less => {
                                if cast_to.is_int() {
                                    compiler.fn_builder.ins().sextend(ir::types::I32, value)
                                } else {
                                    compiler.fn_builder.ins().uextend(ir::types::I32, value)
                                }
                            }
                            Ordering::Equal => value,
                            Ordering::Greater => compiler.fn_builder.ins().ireduce(ir::types::I32, value),
                        };

                        if cast_to.is_int() {
                            compiler.fn_builder.ins().fcvt_from_sint(ir_type, value)
                        } else {
                            compiler.fn_builder.ins().fcvt_from_uint(ir_type, value)
                        }
                    } else {
                        value
                    };

                    Ok(MolValue::Value(value))
                } else {
                    Ok(MolValue::Value(value))
                }
            }
            Expr::Block(block_ref) => block_ref.compile(ast, compiler),
            Expr::Var(name) => compiler.compile_var_expr(ast, self, name.as_str()),
            &Expr::AdtIndex { target, field } => compiler.compile_field_access_expr(ast, self, target, field),
            &Expr::VTableIndex {
                target,
                target_ty,
                vtable,
                func,
            } => compiler.compile_vtable_index(ast, target, target_ty, vtable, func),
            &Expr::ArrayIndex { target, element } => compiler.compile_array_index(ast, self, target, element),
            &Expr::While { condition, block } => compiler.compile_while_expr(ast, condition, block),
            Expr::Array { elements, .. } => compiler.compile_array_expr(ast, self, elements.as_ref()),
            &Expr::Binary { operator, lhs, rhs } => compiler.compile_bin_expr(ast, lhs, operator.value, rhs),
            Expr::Call { func, args } => compiler.compile_call_expr(ast, *func, args.as_ref()),
            Expr::Closure { args, captures, body } => compiler.compile_closure_expr(ast, self, args.as_ref(), captures.as_ref(), *body),
            Expr::Construct { variant, fields, .. } => compiler.compile_construct(ast, ast[self].ty, *variant, fields.as_ref()),
            Expr::IsPattern { target, pattern } => compiler.compile_is_pattern_expr(ast, *target, pattern),
            &Expr::Func(func) => Ok(MolValue::FuncRef(match compiler.funcs.entry(FuncKey::Ref(func)) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let func = compiler
                        .compiler
                        .codegen
                        .module
                        .declare_func_in_func(compiler.compiler.func_ref_to_func_id[&func], compiler.fn_builder.func);

                    *entry.insert(func)
                }
            })),
            Expr::TraitFunc { target, func, .. } => {
                if let MolValue::FatPtr(value, vtable_ptr) = target.compile(ast, compiler)? {
                    let vtable_func = VTablePtr::get_func_ptr(compiler.compiler.isa(), &mut compiler.fn_builder, vtable_ptr, func.index() as u32);

                    compiler.this.replace(MolValue::Value(value));

                    Ok(MolValue::Value(vtable_func))
                } else {
                    unimplemented!("expected fat ptr for accessing dynamic vtable value")
                }
            }
            Expr::Error(_) => unreachable!(),
        }
    }
}
