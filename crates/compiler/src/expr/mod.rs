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
mod type_index;
mod var;
mod vtable_access;
mod r#while;

use std::cmp::Ordering;

use cranelift::{codegen::ir, module::Module, prelude::InstBuilder};
use mollie_ir::MollieType;
use mollie_typed_ast::{Expr, ExprRef, TypedAST};
use mollie_typing::{IntType, PrimitiveType, UIntType};

use crate::{AsIrType, CompileTypedAST, MolValue, error::CompileResult, func::FunctionCompiler};

fn get_ir_type(primitive: PrimitiveType, ptr_type: ir::Type) -> ir::Type {
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
        PrimitiveType::Float => ir::types::F32,
        _ => unimplemented!(),
    }
}

impl<S, M: Module> CompileTypedAST<S, M, MolValue> for ExprRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, S, M>) -> CompileResult<MolValue> {
        match &ast[self].value {
            Expr::Literal(literal_expr) => compiler.compile_lit_expr(ast, self, literal_expr),
            &Expr::If { condition, block, else_block } => compiler.compile_if_expr(ast, condition, block, else_block),
            &Expr::Cast { expr, ty } => {
                let value = expr.compile(ast, compiler)?.expect_value();

                if let (mollie_typing::TypeInfo::Primitive(got), mollie_typing::TypeInfo::Primitive(cast_to)) =
                    (compiler.checker.solver.get_info(ast[expr].ty), compiler.checker.solver.get_info(ty))
                    && let MollieType::Regular(ir_type) = ty.as_ir_type(&compiler.checker.solver, compiler.compiler.isa())
                {
                    let ptr_type = compiler.compiler.ptr_type();
                    let a = get_ir_type(*got, ptr_type);
                    let b = get_ir_type(*cast_to, ptr_type);

                    let value = if got.is_integer() && cast_to.is_integer() {
                        if got.is_signed_integer() && cast_to.is_signed_integer() {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => compiler.fn_builder.ins().sextend(ir_type, value),
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        } else if got.is_unsigned_integer() && cast_to.is_unsigned_integer() {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => compiler.fn_builder.ins().uextend(ir_type, value),
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        } else {
                            match a.bytes().cmp(&b.bytes()) {
                                Ordering::Less => {
                                    if cast_to.is_signed_integer() {
                                        compiler.fn_builder.ins().sextend(ir_type, value)
                                    } else {
                                        compiler.fn_builder.ins().uextend(ir_type, value)
                                    }
                                }
                                Ordering::Equal => value,
                                Ordering::Greater => compiler.fn_builder.ins().ireduce(ir_type, value),
                            }
                        }
                    } else if got.is_float() && cast_to.is_integer() {
                        let value = match a.bytes().cmp(&b.bytes()) {
                            Ordering::Less => {
                                if cast_to.is_signed_integer() {
                                    compiler.fn_builder.ins().sextend(ir::types::I32, value)
                                } else {
                                    compiler.fn_builder.ins().uextend(ir::types::I32, value)
                                }
                            }
                            Ordering::Equal => value,
                            Ordering::Greater => compiler.fn_builder.ins().ireduce(ir::types::I32, value),
                        };

                        if cast_to.is_signed_integer() {
                            compiler.fn_builder.ins().fcvt_to_sint(ir_type, value)
                        } else {
                            compiler.fn_builder.ins().fcvt_to_uint(ir_type, value)
                        }
                    } else if got.is_integer() && cast_to.is_float() {
                        let value = match a.bytes().cmp(&b.bytes()) {
                            Ordering::Less => {
                                if cast_to.is_signed_integer() {
                                    compiler.fn_builder.ins().sextend(ir::types::I32, value)
                                } else {
                                    compiler.fn_builder.ins().uextend(ir::types::I32, value)
                                }
                            }
                            Ordering::Equal => value,
                            Ordering::Greater => compiler.fn_builder.ins().ireduce(ir::types::I32, value),
                        };

                        if cast_to.is_signed_integer() {
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
            &Expr::Access { target, field } => compiler.compile_field_access_expr(ast, self, target, field),
            &Expr::VTableAccess { target, func } => compiler.compile_vtable_access(ast, target, func),
            &Expr::Index { target, index } => compiler.compile_array_index(ast, self, target, index),
            &Expr::While { condition, block } => compiler.compile_while_expr(ast, condition, block),
            Expr::Array(elements) => compiler.compile_array_expr(ast, self, elements.as_ref()),
            &Expr::Binary { operator, lhs, rhs } => compiler.compile_bin_expr(ast, lhs, operator, rhs),
            Expr::Call { func, args } => compiler.compile_call_expr(ast, *func, args.as_ref()),
            Expr::Closure { args, captures, body } => compiler.compile_closure_expr(ast, self, args.as_ref(), captures.as_ref(), *body),
            Expr::Construct { ty, variant, fields } => compiler.compile_construct(ast, *ty, *variant, fields.as_ref()),
            Expr::IsPattern { target, pattern } => compiler.compile_is_pattern_expr(ast, *target, pattern),
            &Expr::TypeIndex { ty, path } => compiler.compile_type_index_expr(ast, ty, path),
            Expr::Nothing => Ok(MolValue::Nothing),
            Expr::Error(_) => unreachable!(),
        }
    }
}
